;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/json.ss

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; JSON pull parser


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

(define raise-json
  (case-lambda
    ((str)
      (raise-errorf 'json-read-token str))
    ((fmt arg)
      (raise-errorf 'json-read-token fmt arg))))

(define (raise-eof-in-number)
  (raise-json "unexpected EOF in json number"))

(define (raise-invalid-digit b)
  (if (fixnum? b)
    (raise-json "invalid byte ~s in json number digits" b)
    (raise-eof-in-number)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reader helpers

(define (skip-ws p)
  (let ((b (lookahead-u8 p)))
    (when (whitespace? b)
      (get-u8 p)
      (skip-ws p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String parsing


(define (bytes-append! bytes b)
  (when bytes
    (bytespan-insert-right/u8! bytes b)))


;; bytes->string should accept only valid utf8 - currently not checked.
(define (bytes->string bytes)
  (if bytes
    (utf8b-bytespan->string bytes)
    ""))


(define (parse-string p bytes)
  ;; opening quote already consumed
  (let ((b (get-u8 p)))
    (cond
      ((not (fixnum? b))
        (raise-json "unexpected EOF in json string"))
      ((fx=? b 34) ;; closing quote
        (bytes->string bytes))
      ((fx=? b 92) ;; backslash, starts escape sequence
        (let ((e (get-u8 p)))
          (cond
            ((not (fixnum? e)) (raise-json "unexpected EOF in json string"))
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
                     (ch
                       (integer->char
                         (cond
                           ((fx<=? #xD800 u16 #xDBFF)
                             (let ((low16 (parse-string-low-surrogate p)))
                               (fx+ (fx<< (fx- u16 #xD800) 10)
                                    (fx+ low16 (fx- #x10000 #xDC00)))))
                           ((fx<=? #xDC00 u16 #xDFFF)
                             (raise-errorf 'json
                               "unpaired low surrogate \\u~4,'0X in json string escape" u16))
                           (else
                             u16)))))
                (when bytes
                  (bytespan-insert-right/char! bytes ch)))
              (parse-string p bytes))
            (else
              (raise-json "invalid byte ~s in json string escape" e)))))
      (else
        (when (fx<? b 32)
          (raise-json "invalid control byte ~s in json string" b))
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
          (raise-json "invalid byte ~s in json string escape \\u" b))
        (%loop (fx1+ i)
               (fxior (fx<< u16 4) (hex-value b)))))))


;; parse an Unicode low surrogate
;; i.e. an escape sequence \uXXXX containing a fixnum in #xDC00 ... #xDFFF
(define (parse-string-low-surrogate p)
  (unless (and (eqv? (get-u8 p) 92)
               (eqv? (get-u8 p) 117))
    (raise-json "missing low surrogate \\uXXXX after high surrogate in json string"))
  (let ((u16 (parse-string-hex4 p)))
    (unless (fx<=? #xDC00 u16 #xDFFF)
      (raise-json "out-of-range low surrogate \\u~4,'0X after high surrogate in json string" u16))
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
  (if bytes
    (string->number (bytes->string bytes))
    0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Literal parsing

(define (expect-bytes p bytes)
  (for-each
    (lambda (b)
      (unless (eqv? b (get-u8 p))
        (raise-json "invalid literal")))
    bytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Raw tokenizer. Validates grammar, does not validate syntax

(define (next-token p buf)
  (skip-ws p)
  (let ((b (get-u8 p)))
    (cond
      ((not (fixnum? b)) (eof-object))
      ((fx=? b 44)  #\,)
      ((fx=? b 58)  #\:)
      ((fx=? b 91)  #\[)
      ((fx=? b 93)  #\])
      ((fx=? b 123) #\{)
      ((fx=? b 125) #\})
      ((fx=? b 34)
        (when buf
          (bytespan-clear! buf))
        (parse-string p buf))
      ((or (digit? b) (fx=? b 45))
        (when buf
          (bytespan-clear! buf))
        (parse-number p buf b))
      ((fx=? b 102) (expect-bytes p '(97 108 115 101)) #f) ; false -> #f
      ((fx=? b 110) (expect-bytes p '(117 108 108)) '())   ; null  -> '()
      ((fx=? b 116) (expect-bytes p '(114 117 101)) #t)    ; true  -> #t
      (else
        (raise-json "unexpected byte ~s" b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validating parser

;; parser states
(define $top                 0)
(define $done                1)
(define $array-expect-value  2)
(define $array-after-value   3)
(define $object-expect-key   4)
(define $object-expect-colon 5)
(define $object-expect-value 6)
(define $object-after-value  7)


(define-record-type (json-reader %make-json-reader json-reader?)
  (fields
    port            ; binary input port
    stack           ; bytespan contaning stack of states
    buffer          ; bytespan buffer for parsing strings and numbers
    (mutable eof?)) ; boolean
  (nongenerative %json-reader-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define (make-json-reader p)
  (assert* 'make-json-reader (binary-port? p))
  (assert* 'make-json-reader (input-port? p))
  (%make-json-reader p (bytespan $top) (bytespan) #f))


(define (json-reader-depth r)
  (fx1- (bytespan-length (json-reader-stack r))))

(define (push r state)
  (bytes-append! (json-reader-stack r) state))

(define (pop r)
  (bytespan-delete-right! (json-reader-stack r) 1))

(define (change r state)
  (let* ((st (json-reader-stack r)))
    (bytespan-set/u8! st (fx1- (bytespan-length st)) state)))

(define (state r)
  (bytespan-ref-right/u8 (json-reader-stack r)))

(define (atomic-value-token? tok)
  (not (or (char? tok) (eof-object? tok))))

(define (value-start-token? tok)
  (or (atomic-value-token? tok) (eqv? tok #\{) (eqv? tok #\[)))

(define (accept-value-start r tok)
  (case tok
    ((#\{)
      (push r $object-expect-key))
    ((#\[)
      (push r $array-expect-value))
    (else ;; atomic value: nothing to push
      (void))))


(define (validate-top r tok)
  (cond
    ((eof-object? tok)
      (raise-json "empty json input"))
    ((value-start-token? tok)
      (change r $done)
      (accept-value-start r tok))
    (else
      (raise-json "invalid json top-level token ~s" tok))))


(define (validate-done r tok)
  (unless (eof-object? tok)
    (raise-json "trailing token ~s after top-level value" tok)))


(define (validate-array-expect-value r tok)
  (cond
    ((eqv? tok #\])
      (pop r))
    ((value-start-token? tok)
      (change r $array-after-value)
      (accept-value-start r tok))
    (else
     (raise-json "expecting value or ']' in json array, found ~s" tok))))


(define (validate-array-after-value r tok)
  (case tok
    ((#\,)
      (change r $array-expect-value))
    ((#\])
      (pop r))
    (else
      (raise-json "expecting ',' or ']' in json array, found ~s" tok))))


(define (validate-object-expect-key r tok)
  (cond
    ((eqv? tok #\})
      (pop r))
    ((string? tok)
      (change r $object-expect-colon))
    (else
      (raise-json "expecting string key or '}' in json object, found ~s" tok))))


(define (validate-object-expect-colon r tok)
  (unless (eqv? tok #\:)
    (raise-json "expecting ':' after json object key, found ~s" tok))
  (change r $object-expect-value))


(define (validate-object-expect-value r tok)
  (unless (value-start-token? tok)
    (raise-json "expecting value in json object, found ~s" tok))
  (change r $object-after-value)
  (accept-value-start r tok))


(define (validate-object-after-value r tok)
  (case tok
    ((#\,)
      (change r $object-expect-key))
    ((#\})
      (pop r))
    (else
       (raise-json "expecting ',' or '}' in json object, found ~s" tok))))


(define (json-read-token* r buf)
  (if (json-reader-eof? r)
    (eof-object)
    (let ((tok (next-token (json-reader-port r) buf))
          (st  (state r)))
      (when (eof-object? tok)
        (json-reader-eof?-set! r #t))
      (cond
        ((fx=? st $top)
          (validate-top r tok))
        ((fx=? st $done)
          (validate-done r tok))
        ((fx=? st $array-expect-value)
          (validate-array-expect-value r tok))
        ((fx=? st $array-after-value)
          (validate-array-after-value r tok))
        ((fx=? st $object-expect-key)
          (validate-object-expect-key r tok))
        ((fx=? st $object-expect-colon)
          (validate-object-expect-colon r tok))
        ((fx=? st $object-expect-value)
          (validate-object-expect-value r tok))
        ((fx=? st $object-after-value)
          (validate-object-after-value r tok))
        (else
          (raise-json "inconsistent json-reader state: ~s" st)))
      tok)))


;; read and return next token, which can be one of:
;;   #!eof   i.e. the (eof-object)
;;   '()     i.e. null
;;   #t      i.e. true
;;   #f      i.e. false
;;   an exact or inexact real number
;;   a string
;;   a character among:
;;     #\{   i.e. left  brace
;;     #\}   i.e. right brace
;;     #\[   i.e. left  bracket
;;     #\]   i.e. right bracket
;;     #\,   i.e. comma
;;     #\:   i.e. colon
(define (json-read-token r)
  (json-read-token* r (json-reader-buffer r)))


;; skip next token and return its kind, which can be one of:
;;   #!eof   i.e. the (eof-object)
;;   '()     i.e. null
;;   #t      i.e. true
;;   #f      i.e. false
;;   0       i.e. a number was skipped
;;   ""      i.e. a string was skipped
;;   a character among:
;;     #\{   i.e. left  brace
;;     #\}   i.e. right brace
;;     #\[   i.e. left  bracket
;;     #\]   i.e. right bracket
;;     #\,   i.e. comma
;;     #\:   i.e. colon
(define (json-skip-token r)
  (json-read-token* r #f))


;; skip next value and return its kind, which can be one of:
;;   #!eof   i.e. the (eof-object)
;;   '()     i.e. null
;;   #t      i.e. true
;;   #f      i.e. false
;;   0       i.e. a number was skipped
;;   ""      i.e. a string was skipped
;;   'array  i.e. an array [ ... ] was skipped
;;   'object i.e. an object { ... } was skipped
;;   a character among:
;;     #\}   i.e. right brace
;;     #\]   i.e. right bracket
;;     #\,   i.e. comma
;;     #\:   i.e. colon
(define (json-skip-value r)
  (let ((tok0 (json-skip-token r)))
    (if (or (eqv? tok0 #\[) (eqv? tok0 #\{))
      (let %skip-value ((r r) (depth0 (json-reader-depth r)))
        (let ((tok (json-skip-token r)))
          (if (or (eof-object? tok) (fx<? (json-reader-depth r) depth0))
            (if (eqv? tok0 #\[) 'array 'object)
            (%skip-value r depth0))))
      tok0)))