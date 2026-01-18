;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;;; JSON pull parser
;;;
(library (scheme2k io json pull (0 9 3))
  (export json-next-token make-json-pull-parser)
  (import
    (rename (rnrs)                         (fxarithmetic-shift-left fx<<))
    (only (chezscheme)                     fx1+)
    (only (scheme2k bootstrap)             raise-errorf)
    (rename
      (only (scheme2k containers bytespan) bytespan bytespan->bytevector*! bytespan-insert-right/u8!)
                                           (bytespan-insert-right/u8! bytes-append!))
    (only (scheme2k containers utf8b)      bytespan-insert-right/char!))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(define (whitespace? b)
  (and (fixnum? b)
       (or (fx=? b 32) (fx=? b 9) (fx=? b 10) (fx=? b 13))))

(define (digit? b)
  (and (fixnum? b) (fx<=? 48 b 57))) ; 0 ... 9

(define (digit19? b)
  (and (fixnum? b) (fx<=? 49 b 57))) ; 1 ... 9

(define (hex-digit? b)
  (and (fixnum? b)
       (or (fx<=? 48 b 57)     ; 0 ... 9
           (fx<=? 65 b 70)     ; A ... F
           (fx<=? 97 b 102)))) ; a ... f

(define (hex-value b)
  (cond ((fx<=? b 57) (fx- b 48))
        ((fx<=? b 70) (fx- b 55))
        (else         (fx- b 87))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reader helpers

(define (skip-ws p)
  (let ((b (lookahead-u8 p)))
    (when (whitespace? b)
      (get-u8 p)
      (skip-ws p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String parsing

(define (bytes->string bytes)
  ;; bytespan must contain valid utf8
  (utf8->string (bytespan->bytevector*! bytes)))

(define (parse-string p)
  ;; opening quote already consumed
  (let %parse-string ((p p) (bytes (bytespan)))
    (let ((b (get-u8 p)))
      (cond
        ((not (fixnum? b))
          (raise-errorf 'json "unexpected EOF in json string"))
        ((fx=? b 34) ;; closing quote
          (bytes->string bytes))
        ((fx=? b 92) ;; backslash, starts escape sequence
          (let ((e (get-u8 p)))
            (cond
              ((not (fixnum? e)) (raise-errorf 'json "unexpected EOF in json string"))
              ((fx=? e 34)  (bytes-append! bytes e)  (%parse-string p bytes))
              ((fx=? e 47)  (bytes-append! bytes e)  (%parse-string p bytes))
              ((fx=? e 92)  (bytes-append! bytes e)  (%parse-string p bytes))
              ((fx=? e 98)  (bytes-append! bytes 8)  (%parse-string p bytes))
              ((fx=? e 102) (bytes-append! bytes 12) (%parse-string p bytes))
              ((fx=? e 110) (bytes-append! bytes 10) (%parse-string p bytes))
              ((fx=? e 114) (bytes-append! bytes 13) (%parse-string p bytes))
              ((fx=? e 116) (bytes-append! bytes 9)  (%parse-string p bytes))
              ((fx=? e 117) ;; \uXXXX
                (let* ((u16 (parse-string-hex4 p))
                       (codepoint
                         (cond
                           ((fx<=? #xD800 u16 #xDBFF)
                             (let ((low16 (parse-string-low-surrogate p)))
                               (fx+ (fx<< (fx- u16 #xD800) 10)
                                    (fx+ low16 (fx- #x10000 #xDC00)))))
                           ((fx<=? #xDC00 u16 #xDFFF)
                             (raise-errorf 'json
                               "unpaired low surrogate \\u~4,'0X in json string escape" u16))
                           (else
                             u16))))
                  (bytespan-insert-right/char! bytes (integer->char codepoint)))
                (%parse-string p bytes))
              (else (raise-errorf 'json "invalid byte ~s in json string escape" e)))))
        (else
          (when (fx<? b 32)
            (raise-errorf 'json "invalid control byte ~s in json string" b))
          (bytes-append! bytes b)
          (%parse-string p bytes))))))


;; parse the four hexadecimal digits after \u
;; and return them as a fixnum in 0 ... #xFFFF
(define (parse-string-hex4 p)
  (let %loop ((i 0) (u16 0))
    (if (fx=? i 4)
      u16
      (let ((h (get-u8 p)))
        (unless (hex-digit? h)
          (raise-errorf 'json "invalid hexadecimal digit after \\u in json string escape"))
        (%loop (fx1+ i)
               (fxior (fx<< u16 4) (hex-value h)))))))


;; parse an Unicode low surrogate
;; i.e. an escape sequence \uXXXX containing a fixnum in #xDC00 ... #xDFFF
(define (parse-string-low-surrogate p)
  (unless (and (eqv? (get-u8 p) 92)
               (eqv? (get-u8 p) 117))
    (raise-errorf 'json "missing low surrogate \\uXXXX after high surrogate in json string"))
  (let ((u16 (parse-string-hex4 p)))
    (unless (fx<=? #xDC00 u16 #xDFFF)
      (raise-errorf 'json "out-of-range low surrogate \\u~4,'0X after high surrogate in json string" u16))
    u16))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Number parsing

(define (parse-number p first)
  (let %loop ((p p) (bytes (bytespan first)))
    (let ((b (lookahead-u8 p)))
      ;; FIXME validate json number syntax
      (if (and (fixnum? b)
               (or (digit? b)
                   (fx=? b 46) (fx=? b 101) (fx=? b 69)
                   (fx=? b 43) (fx=? b 45)))
        (begin
          (get-u8 p)
          (bytes-append! bytes b)
          (%loop p bytes))
        (string->number (bytes->string bytes))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Literal parsing

(define (expect-bytes p bytes)
  (for-each
    (lambda (b)
      (unless (eqv? b (get-u8 p))
        (raise-errorf 'json "invalid literal")))
    bytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main pull tokenizer

(define (json-next-token p)
  (skip-ws p)
  (let ((b (get-u8 p)))
    (cond
      ((not (fixnum? b)) 'eof)
      ((fx=? b 123) 'lbrace)
      ((fx=? b 125) 'rbrace)
      ((fx=? b 91)  'lbracket)
      ((fx=? b 93)  'rbracket)
      ((fx=? b 58)  'colon)
      ((fx=? b 44)  'comma)
      ((fx=? b 34)  (parse-string p))
      ((or (digit? b) (fx=? b 45))
        (parse-number p b))
      ((fx=? b 116) (expect-bytes p '(114 117 101)) #t)    ; true
      ((fx=? b 102) (expect-bytes p '(97 108 115 101)) #f) ; false
      ((fx=? b 110) (expect-bytes p '(117 108 108)) '())   ; nil
      (else
        (raise-errorf 'json "unexpected byte")))))


(define (make-json-pull-parser p)
  (let ((state-stack '(top))
        (finished? #f))

    (define (push s)
      (set! state-stack (cons s state-stack)))

    (define (pop)
      (set! state-stack (cdr state-stack)))

    (define (state)
      (car state-stack))

    (define (atomic-value-token? t)
      (or (boolean? t) (null? t) (number? t) (string? t)))

    (define (value-start-token? t)
      (or (atomic-value-token? t)
          (memq t '(lbrace lbracket))))

    (define (accept-value-start t)
      (case t
        ((lbrace)
         (push 'object-expect-key))
        ((lbracket)
         (push 'array-expect-value))
        (else
         ;; atomic value: nothing to push
         #f)))

    (define (next-token)
      (when finished?
        (raise-errorf 'json "token requested after eof"))

      (let ((t (json-next-token p)))
          (case (state)

            ;; =====================================================
            ;; Top level
            ((top)
             (cond
               ((equal? t 'eof)
                (raise-errorf 'json "empty input"))
               ((value-start-token? t)
                (pop)
                (accept-value-start t)
                (push 'done)
                t)
               (else
                (raise-errorf 'json "invalid top-level token" t))))

            ;; =====================================================
            ;; Done (only EOF allowed)
            ((done)
             (if (equal? t 'eof)
                 (begin
                   (set! finished? #t)
                   t)
                 (raise-errorf 'json "trailing data after JSON value" t)))

            ;; =====================================================
            ;; Array
            ((array-expect-value)
             (cond
               ((equal? t 'rbracket)
                (pop)
                t)
               ((value-start-token? t)
                (pop)
                (accept-value-start t)
                (push 'array-after-value)
                t)
               (else
                (raise-errorf 'json "expected value or ']'" t))))

            ((array-after-value)
             (cond
               ((equal? t 'comma)
                (pop)
                (push 'array-expect-value)
                t)
               ((equal? t 'rbracket)
                (pop)
                t)
               (else
                (raise-errorf 'json "expected ',' or ']'" t))))

            ;; =====================================================
            ;; Object
            ((object-expect-key)
             (cond
               ((equal? t 'rbrace)
                (pop)
                t)
               ((eq? (car t) 'string)
                (pop)
                (push 'object-expect-colon)
                t)
               (else
                (raise-errorf 'json "expected string key or '}'" t))))

            ((object-expect-colon)
             (if (equal? t 'colon)
                 (begin
                   (pop)
                   (push 'object-expect-value)
                   t)
                 (raise-errorf 'json "expected ':'" t)))

            ((object-expect-value)
             (if (value-start-token? t)
                 (begin
                   (pop)
                   (accept-value-start t)
                   (push 'object-after-value)
                   t)
                 (raise-errorf 'json "expected value" t)))

            ((object-after-value)
             (cond
               ((equal? t 'comma)
                (pop)
                (push 'object-expect-key)
                t)
               ((equal? t 'rbrace)
                (pop)
                t)
               (else
                (raise-errorf 'json "expected ',' or '}'" t))))

            (else
             (raise-errorf 'json "invalid parser state" state)))))

    next-token))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example usage for (json-next-token)
;;
;; (define p (open-bytevector-input-port
;;            (string->utf8 "{\"a\": [1, true]}")))
;; (let %loop ()
;;   (let ((t (json-next-token p)))
;;     (display t) (newline)
;;     (unless (equal? t 'eof) (%loop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example usage for (make-json-parser)
;;
;; (define parser
;;   (make-json-parser
;;     (open-bytevector-input-port
;;      (string->utf8 "{\"a\": [1, true]}"))))
;;
;; (let %loop ()
;;   (let ((t (parser)))
;;     (display t) (newline)
;;     (unless (equal? t 'eof)
;;       (%loop))))


) ; close library
