;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;;; JSON pull parser
;;;
(library (scheme2k io json read (0 9 3))
  (export json-read-token make-json-reader)
  (import
    (rename (rnrs)                         (fxarithmetic-shift-left fx<<))
    (only (chezscheme)                     fx1+ record-writer void)
    (only (scheme2k bootstrap)             assert* raise-errorf)
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

(define (parse-string p bytes)
  ;; opening quote already consumed
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
            ((fx=? e 34)  (bytes-append! bytes e)  (parse-string p bytes))
            ((fx=? e 47)  (bytes-append! bytes e)  (parse-string p bytes))
            ((fx=? e 92)  (bytes-append! bytes e)  (parse-string p bytes))
            ((fx=? e 98)  (bytes-append! bytes 8)  (parse-string p bytes))
            ((fx=? e 102) (bytes-append! bytes 12) (parse-string p bytes))
            ((fx=? e 110) (bytes-append! bytes 10) (parse-string p bytes))
            ((fx=? e 114) (bytes-append! bytes 13) (parse-string p bytes))
            ((fx=? e 116) (bytes-append! bytes 9)  (parse-string p bytes))
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
              (parse-string p bytes))
            (else (raise-errorf 'json "invalid byte ~s in json string escape" e)))))
      (else
        (when (fx<? b 32)
          (raise-errorf 'json "invalid control byte ~s in json string" b))
        (bytes-append! bytes b)
        (parse-string p bytes)))))


;; parse the four hexadecimal digits after \u
;; and return them as a fixnum in 0 ... #xFFFF
(define (parse-string-hex4 p)
  (let %loop ((i 0) (u16 0))
    (if (fx=? i 4)
      u16
      (let ((b (get-u8 p)))
        (unless (hex-digit? b)
          (raise-errorf 'json "invalid byte ~s in json string escape \\u" b))
        (%loop (fx1+ i)
               (fxior (fx<< u16 4) (hex-value b)))))))


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

(define (parse-number p bytes first)
  (let* ((b (if (fx=? first 45) ; #\-
              (begin
                (bytes-append! bytes first)
                (get-u8 p))
              first))
         (initial0
           (if (fixnum? b)
             (fx=? first 48) ; #\0
             (raise-eof-in-number))))
    (bytes-append! bytes b)
    (unless initial0
      (parse-optional-digits p bytes)))

  (let ((b (lookahead-u8 p)))
    (case b
      ((46) ; #\.
        (get-u8 p)
        (bytes-append! bytes b)
        (parse-digits p bytes))))

  (let ((b (lookahead-u8 p)))
    (case b
      ((69 101) ; #\E #\e
        (get-u8 p)
        (bytes-append! bytes b)
        (parse-exponent p bytes))))

  (bytes->number bytes))


;; parse zero or more base-10 digits
(define (parse-optional-digits p bytes)
  (let ((b (lookahead-u8 p)))
    (when (digit? b)
      (bytes-append! bytes b)
      (get-u8 p)
      (parse-optional-digits p bytes))))


;; parse one or more base-10 digits
(define (parse-digits p bytes)
  (let ((b (lookahead-u8 p)))
    (unless (digit? b)
      (raise-invalid-digit b))
    (bytes-append! bytes b)
    (get-u8 p))
  (parse-optional-digits p bytes))


;; parse json number exponent after #\E or #\e
(define (parse-exponent p bytes)
  (let ((b (lookahead-u8 p)))
    (case b
      ((43 45) ; #\+ #\-
        (bytes-append! bytes b)
        (get-u8 p))))
  (parse-digits p bytes))


(define (bytes->number bytes)
  (string->number (bytes->string bytes)))

(define (raise-invalid-digit b)
  (if (fixnum? b)
    (raise-errorf 'json "invalid byte ~s in json number digits" b)
    (raise-eof-in-number)))

(define (raise-eof-in-number)
  (raise-errorf 'json "unexpected EOF in json number"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Literal parsing

(define (expect-bytes p bytes)
  (for-each
    (lambda (b)
      (unless (eqv? b (get-u8 p))
        (raise-errorf 'json "invalid literal")))
    bytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Raw tokenizer. Validates grammar, does not validate syntax

(define (read-token p)
  (skip-ws p)
  (let ((b (get-u8 p)))
    (cond
      ((not (fixnum? b)) (eof-object))
      ((fx=? b 123) #\{)
      ((fx=? b 125) #\})
      ((fx=? b 91)  #\[)
      ((fx=? b 93)  #\])
      ((fx=? b 58)  #\:)
      ((fx=? b 44)  #\,)
      ((fx=? b 34)  (parse-string p (bytespan)))
      ((or (digit? b) (fx=? b 45))
        (parse-number p (bytespan) b))
      ((fx=? b 116) (expect-bytes p '(114 117 101)) #t)    ; true
      ((fx=? b 102) (expect-bytes p '(97 108 115 101)) #f) ; false
      ((fx=? b 110) (expect-bytes p '(117 108 108)) '())   ; nil
      (else
        (raise-errorf 'json "unexpected byte")))))

(define-record-type (json-reader %make-json-reader json-reader?)
  (fields
    port            ; binary input port
    (mutable stack) ; list of states
    (mutable eof?)) ; boolean
  (nongenerative %json-reader-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define (make-json-reader p)
  (assert* 'make-json-reader (binary-port? p))
  (assert* 'make-json-reader (input-port? p))
  (%make-json-reader p '(top) #f))


(define (push r state)
  (json-reader-stack-set! r (cons state (json-reader-stack r))))

(define (pop r)
  (json-reader-stack-set! r (cdr (json-reader-stack r))))

(define (state r)
  (car (json-reader-stack r)))

(define (atomic-value-token? tok)
  (not (or (char? tok) (eof-object? tok))))

(define (value-start-token? tok)
  (or (atomic-value-token? tok) (eqv? tok #\{) (eqv? tok #\[)))

(define (accept-value-start r tok)
  (case tok
    ((#\{)
      (push r 'object-expect-key))
    ((#\[)
      (push r 'array-expect-value))
    (else ;; atomic value: nothing to push
      (void))))


(define (validate-top r tok)
  (cond
    ((eof-object? tok)
      (raise-errorf 'json "empty json input"))
    ((value-start-token? tok)
      (pop r)
      (push r 'done)
      (accept-value-start r tok))
    (else
      (raise-errorf 'json "invalid json top-level token" tok))))


(define (validate-done r tok)
  (if (eof-object? tok)
    (json-reader-eof?-set! r #t)
    (raise-errorf 'json "trailing data after top-level value" tok)))


(define (validate-array-expect-value r tok)
  (cond
    ((eqv? tok #\])
      (pop r))
    ((value-start-token? tok)
      (pop r)
      (push r 'array-after-value)
      (accept-value-start r tok))
    (else
     (raise-errorf 'json "expecting value or ']' in json array, found ~s" tok))))


(define (validate-array-after-value r tok)
  (case tok
    ((#\,)
      (pop r)
      (push r 'array-expect-value))
    ((#\])
      (pop r))
    (else
      (raise-errorf 'json "expecting ',' or ']' in json array, found ~s" tok))))


(define (validate-object-expect-key r tok)
  (cond
    ((eqv? tok #\})
      (pop r))
    ((string? tok)
      (pop r)
      (push r 'object-expect-colon))
    (else
      (raise-errorf 'json "expecting string key or '}' in json object, found ~s" tok))))


(define (validate-object-expect-colon r tok)
  (unless (eqv? tok #\:)
    (raise-errorf 'json "expected ':'" tok))
  (pop r)
  (push r 'object-expect-value))


(define (validate-object-expect-value r tok)
  (unless (value-start-token? tok)
    (raise-errorf 'json "expecting value in json object, found ~s" tok))
  (pop r)
  (push r 'object-after-value)
  (accept-value-start r tok))


(define (validate-object-after-value r tok)
  (case tok
    ((#\,)
      (pop r)
      (push r 'object-expect-key))
    ((#\})
      (pop r))
    (else
       (raise-errorf 'json "expecting ',' or '}' in json object, found ~s" tok))))


(define (json-read-token r)
  (if (json-reader-eof? r)
    (eof-object)
    (let ((tok (read-token (json-reader-port r))))
      (case (state r)
        ((top)
          (validate-top r tok))
        ((done)
          (validate-done r tok))
        ((array-expect-value)
          (validate-array-expect-value r tok))
        ((array-after-value)
          (validate-array-after-value r tok))
        ((object-expect-key)
         (validate-object-expect-key r tok))
        ((object-expect-colon)
         (validate-object-expect-colon r tok))
        ((object-expect-value)
         (validate-object-expect-value r tok))
        ((object-after-value)
         (validate-object-after-value r tok))
        (else
         (raise-errorf 'json "invalid json-reader state: ~s" (state r))))
      tok)))


(record-writer (record-type-descriptor json-reader)
  (lambda (r out writer)
    (display "#<json-reader>" out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example usage for (make-json-reader)
;;
#|
(define (json-read-all p)
  (let loop ((r (make-json-reader p)))
    (let ((tok (json-read-token r)))
      (unless (eof-object? tok)
        (if (char? tok)
          (display tok)
          (write tok))
        (loop r)))))

(json-read-all
  (open-bytevector-input-port
    (string->utf8 "{\"a\": [1, true]}")))
|#

) ; close library
