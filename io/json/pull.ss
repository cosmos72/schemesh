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
    (rnrs)
    (only (chezscheme) fx<=? fx=? fx1+ fxarithmetic-shift-left fxior)
    (only (scheme2k containers charspan) charspan charspan-insert-right! charspan->string*! make-charspan))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(define (whitespace? b)
  (or (fx=? b 32) (fx=? b 9) (fx=? b 10) (fx=? b 13)))

(define (digit? b)
  (and (fixnum? b) (fx<=? 48 b 57))) ; 0 ... 9

(define (hex-digit? b)
  (and (fixnum? b)
       (or (fx<=? 48 b 57)     ; 0 ... 9
           (fx<=? 65 b 70)     ; A ... F
           (fx<=? 97 b 102)))) ; a ... f

(define (hex-value b)
  (cond ((digit? b) (- b 48))
        ((>= b 97) (+ 10 (- b 97)))
        (else (+ 10 (- b 65)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reader helpers

(define (skip-ws p)
  (let ((b (lookahead-u8 p)))
    (when (and (not (eof-object? b)) (whitespace? b))
      (get-u8 p)
      (skip-ws p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String parsing

(define chars-append! charspan-insert-right!)

(define (parse-string p)
  ;; opening quote already consumed
  (let %parse-string ((p p) (chars (make-charspan 0)))
    (let ((b (get-u8 p)))
      (cond
        ((not (fixnum? b))
          (error 'json "unexpected EOF in json string"))
        ((fx=? b 34) ;; closing quote
          (charspan->string*! chars))
        ((fx=? b 92) ;; escape
          (let ((e (get-u8 p)))
            (cond
              ((not (fixnum? e))
                (error 'json "unexpected EOF in json string"))
              ((fx=? e 34)  (chars-append! chars #\")         (%parse-string p chars))
              ((fx=? e 92)  (chars-append! chars #\\)         (%parse-string p chars))
              ((fx=? e 47)  (chars-append! chars #\/)         (%parse-string p chars))
              ((fx=? e 98)  (chars-append! chars #\backspace) (%parse-string p chars))
              ((fx=? e 102) (chars-append! chars #\page)      (%parse-string p chars))
              ((fx=? e 110) (chars-append! chars #\newline)   (%parse-string p chars))
              ((fx=? e 114) (chars-append! chars #\return)    (%parse-string p chars))
              ((fx=? e 116) (chars-append! chars #\tab)       (%parse-string p chars))
              ((fx=? e 117) ;; \uXXXX
                ;; FIXME: handle surrogate pairs \uXXXX\uYYYY
                (let %loop ((i 0) (v 0))
                  (if (fx=? i 4)
                    (begin
                      (chars-append! chars (integer->char v))
                      (%parse-string p chars))
                    (let ((h (get-u8 p)))
                      (unless (hex-digit? h)
                        (error 'json "invalid \\u escape"))
                      (%loop (fx1+ i) (fxior (fxarithmetic-shift-left v 4) (hex-value h)))))))
              (else (error 'json "invalid escape")))))
        (else
          ;; FIXME: combine UTF-8 bytes into chars
          (chars-append! chars (integer->char b))
          (%parse-string p chars))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Number parsing

(define (parse-number p first)
  (let %loop ((p p) (chars (charspan (integer->char first))))
    (let ((b (lookahead-u8 p)))
      ;; FIXME validate json number syntax
      (if (and (fixnum? b)
               (or (digit? b)
                   (fx=? b 46) (fx=? b 101) (fx=? b 69)
                   (fx=? b 43) (fx=? b 45)))
        (begin
          (get-u8 p)
          (chars-append! chars (integer->char b))
          (%loop p chars))
        (string->number (charspan->string*! chars))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Literal parsing

(define (expect-bytes p bytes)
  (for-each
    (lambda (b)
      (unless (eqv? b (get-u8 p))
        (error 'json "invalid literal")))
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
        (error 'json "unexpected byte")))))


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
        (error 'json "token requested after eof"))

      (let ((t (json-next-token p)))
          (case (state)

            ;; =====================================================
            ;; Top level
            ((top)
             (cond
               ((equal? t 'eof)
                (error 'json "empty input"))
               ((value-start-token? t)
                (pop)
                (accept-value-start t)
                (push 'done)
                t)
               (else
                (error 'json "invalid top-level token" t))))

            ;; =====================================================
            ;; Done (only EOF allowed)
            ((done)
             (if (equal? t 'eof)
                 (begin
                   (set! finished? #t)
                   t)
                 (error 'json "trailing data after JSON value" t)))

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
                (error 'json "expected value or ']'" t))))

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
                (error 'json "expected ',' or ']'" t))))

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
                (error 'json "expected string key or '}'" t))))

            ((object-expect-colon)
             (if (equal? t 'colon)
                 (begin
                   (pop)
                   (push 'object-expect-value)
                   t)
                 (error 'json "expected ':'" t)))

            ((object-expect-value)
             (if (value-start-token? t)
                 (begin
                   (pop)
                   (accept-value-start t)
                   (push 'object-after-value)
                   t)
                 (error 'json "expected value" t)))

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
                (error 'json "expected ',' or '}'" t))))

            (else
             (error 'json "invalid parser state" state)))))

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
