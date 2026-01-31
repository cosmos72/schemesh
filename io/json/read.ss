;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/json/json.ss

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; JSON pull parser

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
  (parent obj-reader)
  (fields
    in              ; binary input port
    stack           ; bytespan contaning stack of states
    buffer)         ; bytespan buffer for parsing strings and numbers
  (protocol
    (lambda (args->new)
      (lambda (in)
        ((args->new %json-reader-get %json-reader-close)
          in (bytespan $top) (bytespan)))))
  (nongenerative %json-reader-7c46d04b-34f4-4046-b5c7-b63753c1be40))


(define make-json-reader
  (case-lambda
    ((in)
      (assert* 'make-json-reader (port? in))
      (assert* 'make-json-reader (binary-port? in))
      (assert* 'make-json-reader (input-port? in))
      (%make-json-reader in))
    (()
      (make-json-reader (sh-stdin)))))


(define (json-reader-depth rx)
  (fx1- (bytespan-length (json-reader-stack rx))))


(define (json-reader-eof? rx)
  (assert* 'json-reader-eof? (json-reader? rx))
  (obj-reader-eof? rx))


(define (json-reader-close rx)
  (assert* 'json-reader-close (json-reader? rx))
  (obj-reader-close rx))


;; called by (json-reader-close) -> (obj-reader-close)
(define (%json-reader-close rx)
  (close-port (json-reader-in rx)))


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
      (raise-errorf 'json-reader-get-token str))
    ((fmt arg)
      (raise-errorf 'json-reader-get-token fmt arg))))

(define (raise-eof-in-number)
  (raise-json "unexpected EOF in json number"))

(define (raise-invalid-digit b)
  (if (fixnum? b)
    (raise-json "invalid byte ~s in json number digits" b)
    (raise-eof-in-number)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reader helpers

(define (skip-ws in)
  (let ((b (lookahead-u8 in)))
    (when (whitespace? b)
      (get-u8 in)
      (skip-ws in))))

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


(define (parse-string in bytes)
  ;; opening quote already consumed
  (let ((b (get-u8 in)))
    (cond
      ((not (fixnum? b))
        (raise-json "unexpected EOF in json string"))
      ((fx=? b 34) ;; closing quote
        (bytes->string bytes))
      ((fx=? b 92) ;; backslash, starts escape sequence
        (let ((e (get-u8 in)))
          (cond
            ((not (fixnum? e)) (raise-json "unexpected EOF in json string"))
            ((fx=? e 34)  (bytes-append! bytes e)  (parse-string in bytes))
            ((fx=? e 47)  (bytes-append! bytes e)  (parse-string in bytes))
            ((fx=? e 92)  (bytes-append! bytes e)  (parse-string in bytes))
            ((fx=? e 98)  (bytes-append! bytes 8)  (parse-string in bytes))
            ((fx=? e 102) (bytes-append! bytes 12) (parse-string in bytes))
            ((fx=? e 110) (bytes-append! bytes 10) (parse-string in bytes))
            ((fx=? e 114) (bytes-append! bytes 13) (parse-string in bytes))
            ((fx=? e 116) (bytes-append! bytes 9)  (parse-string in bytes))
            ((fx=? e 117) ;; \uXXXX
              (let* ((u16 (parse-string-hex4 in))
                     (ch
                       (integer->char
                         (cond
                           ((fx<=? #xD800 u16 #xDBFF)
                             (let ((low16 (parse-string-low-surrogate in)))
                               (fx+ (fx<< (fx- u16 #xD800) 10)
                                    (fx+ low16 (fx- #x10000 #xDC00)))))
                           ((fx<=? #xDC00 u16 #xDFFF)
                             (raise-errorf 'json
                               "unpaired low surrogate \\u~4,'0X in json string escape" u16))
                           (else
                             u16)))))
                (when bytes
                  (bytespan-insert-right/char! bytes ch)))
              (parse-string in bytes))
            (else
              (raise-json "invalid byte ~s in json string escape" e)))))
      (else
        (when (fx<? b 32)
          (raise-json "invalid control byte ~s in json string" b))
        (bytes-append! bytes b)
        (parse-string in bytes)))))


;; parse the four hexadecimal digits after \u
;; and return them as a fixnum in 0 ... #xFFFF
(define (parse-string-hex4 in)
  (let %loop ((i 0) (u16 0))
    (if (fx=? i 4)
      u16
      (let ((b (get-u8 in)))
        (unless (hex-digit? b)
          (raise-json "invalid byte ~s in json string escape \\u" b))
        (%loop (fx1+ i)
               (fxior (fx<< u16 4) (hex-value b)))))))


;; parse an Unicode low surrogate
;; i.e. an escape sequence \uXXXX containing a fixnum in #xDC00 ... #xDFFF
(define (parse-string-low-surrogate in)
  (unless (and (eqv? (get-u8 in) 92)
               (eqv? (get-u8 in) 117))
    (raise-json "missing low surrogate \\uXXXX after high surrogate in json string"))
  (let ((u16 (parse-string-hex4 in)))
    (unless (fx<=? #xDC00 u16 #xDFFF)
      (raise-json "out-of-range low surrogate \\u~4,'0X after high surrogate in json string" u16))
    u16))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Number parsing

(define (parse-number in bytes first)
  (let* ((b (if (fx=? first 45) ; #\-
              (begin
                (bytes-append! bytes first)
                (get-u8 in))
              first))
         (initial0
           (if (fixnum? b)
             (fx=? first 48) ; #\0
             (raise-eof-in-number))))
    (bytes-append! bytes b)
    (unless initial0
      (parse-optional-digits in bytes)))

  (let ((b (lookahead-u8 in)))
    (case b
      ((46) ; #\.
        (get-u8 in)
        (bytes-append! bytes b)
        (parse-digits in bytes))))

  (let ((b (lookahead-u8 in)))
    (case b
      ((69 101) ; #\E #\e
        (get-u8 in)
        (bytes-append! bytes b)
        (parse-exponent in bytes))))

  (bytes->number bytes))


;; parse zero or more base-10 digits
(define (parse-optional-digits in bytes)
  (let ((b (lookahead-u8 in)))
    (when (digit? b)
      (bytes-append! bytes b)
      (get-u8 in)
      (parse-optional-digits in bytes))))


;; parse one or more base-10 digits
(define (parse-digits in bytes)
  (let ((b (lookahead-u8 in)))
    (unless (digit? b)
      (raise-invalid-digit b))
    (bytes-append! bytes b)
    (get-u8 in))
  (parse-optional-digits in bytes))


;; parse json number exponent after #\E or #\e
(define (parse-exponent in bytes)
  (let ((b (lookahead-u8 in)))
    (case b
      ((43 45) ; #\+ #\-
        (bytes-append! bytes b)
        (get-u8 in))))
  (parse-digits in bytes))


(define (bytes->number bytes)
  (if bytes
    (string->number (bytes->string bytes))
    0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Literal parsing

(define (expect-bytes in bytes)
  (for-each
    (lambda (b)
      (unless (eqv? b (get-u8 in))
        (raise-json "invalid literal")))
    bytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Raw tokenizer. Validates grammar, does not validate syntax

(define (next-token in buf)
  (skip-ws in)
  (let ((b (get-u8 in)))
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
        (parse-string in buf))
      ((or (digit? b) (fx=? b 45))
        (when buf
          (bytespan-clear! buf))
        (parse-number in buf b))
      ((fx=? b 102) (expect-bytes in '(97 108 115 101)) #f)  ; false -> #f
      ((fx=? b 110) (expect-bytes in '(117 108 108)) (void)) ; null  -> (void)
      ((fx=? b 116) (expect-bytes in '(114 117 101)) #t)     ; true  -> #t
      (else
        (raise-json "unexpected byte ~s" b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validating parser

(define (push rx state)
  (bytes-append! (json-reader-stack rx) state))

(define (pop rx)
  (bytespan-delete-right! (json-reader-stack rx) 1))

(define (change rx state)
  (let* ((st (json-reader-stack rx)))
    (bytespan-set/u8! st (fx1- (bytespan-length st)) state)))

(define (state rx)
  (bytespan-ref-right/u8 (json-reader-stack rx)))

(define (atomic-value-token? tok)
  (not (or (char? tok) (eof-object? tok))))

(define (value-start-token? tok)
  (or (atomic-value-token? tok) (eqv? tok #\{) (eqv? tok #\[)))

(define (accept-value-start rx tok)
  (case tok
    ((#\{)
      (push rx $object-expect-key))
    ((#\[)
      (push rx $array-expect-value))
    (else ;; atomic value: nothing to push
      (void))))


(define (validate-top rx tok)
  (cond
    ((eof-object? tok)
      (raise-json "empty json input"))
    ((value-start-token? tok)
      (change rx $done)
      (accept-value-start rx tok))
    (else
      (raise-json "invalid json top-level token ~s" tok))))


(define (validate-done rx tok)
  (unless (eof-object? tok)
    (raise-json "trailing token ~s after top-level value" tok)))


(define (validate-array-expect-value rx tok)
  (cond
    ((eqv? tok #\])
      (pop rx))
    ((value-start-token? tok)
      (change rx $array-after-value)
      (accept-value-start rx tok))
    (else
     (raise-json "expecting value or ']' in json array, found ~s" tok))))


(define (validate-array-after-value rx tok)
  (case tok
    ((#\,)
      (change rx $array-expect-value))
    ((#\])
      (pop rx))
    (else
      (raise-json "expecting ',' or ']' in json array, found ~s" tok))))


(define (validate-object-expect-key rx tok)
  (cond
    ((eqv? tok #\})
      (pop rx))
    ((string? tok)
      (change rx $object-expect-colon))
    (else
      (raise-json "expecting string key or '}' in json object, found ~s" tok))))


(define (validate-object-expect-colon rx tok)
  (unless (eqv? tok #\:)
    (raise-json "expecting ':' after json object key, found ~s" tok))
  (change rx $object-expect-value))


(define (validate-object-expect-value rx tok)
  (unless (value-start-token? tok)
    (raise-json "expecting value in json object, found ~s" tok))
  (change rx $object-after-value)
  (accept-value-start rx tok))


(define (validate-object-after-value rx tok)
  (case tok
    ((#\,)
      (change rx $object-expect-key))
    ((#\})
      (pop rx))
    (else
       (raise-json "expecting ',' or '}' in json object, found ~s" tok))))


(define (validate-next-token rx buf)
  (if (json-reader-eof? rx)
    (eof-object)
    (let ((tok (next-token (json-reader-in rx) buf))
          (st  (state rx)))
      (cond
        ((fx=? st $top)
          (validate-top rx tok))
        ((fx=? st $done)
          (validate-done rx tok))
        ((fx=? st $array-expect-value)
          (validate-array-expect-value rx tok))
        ((fx=? st $array-after-value)
          (validate-array-after-value rx tok))
        ((fx=? st $object-expect-key)
          (validate-object-expect-key rx tok))
        ((fx=? st $object-expect-colon)
          (validate-object-expect-colon rx tok))
        ((fx=? st $object-expect-value)
          (validate-object-expect-value rx tok))
        ((fx=? st $object-after-value)
          (validate-object-after-value rx tok))
        (else
          (raise-json "inconsistent json-reader state: ~s" st)))
      tok)))


;; read and return next json token, which can be one of:
;;   #!eof   i.e. the (eof-object)
;;   (void)  i.e. null
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
(define (json-reader-get-token rx)
  (validate-next-token rx (json-reader-buffer rx)))



;; read next json value and return it, which can be one of:
;;   #!eof   i.e. the (eof-object)
;;   (void)  i.e. null
;;   #t      i.e. true
;;   #f      i.e. false
;;   an exact or inexact real number
;;   a string
;;   a span, representing a json array
;;   a plist, representing a json object
;;   a character among:
;;     #\,   i.e. comma
;;     #\:   i.e. colon
(define (json-reader-get-value rx)
  (let ((tok0 (json-reader-get-token rx)))
    (case tok0
      ((#\[)
        (let %read-array ((rx rx) (sp (span)) (elem (json-reader-get-value rx)))
          (cond
            ((or (eof-object? elem) (eqv? elem #\]))
              sp)
            (else
              (unless (eqv? #\, elem)
                (span-insert-right! sp elem))
              (%read-array rx sp (json-reader-get-value rx))))))
      ((#\{)
        (let %read-object ((rx rx) (plist '()) (key (json-reader-get-value rx)))
          (cond
            ((or (eof-object? key) (eqv? key #\}))
              (reverse! plist))
            ((eqv? #\, key)
              (%read-object rx plist (json-reader-get-value rx)))
            (else
              (assert* 'json-reader-get-value (string? key))
              (let ((key   (string->symbol key))
                    (colon (json-reader-get-token rx)))
                (assert* 'json-reader-get-value (eqv? #\: colon))
                (let ((value (json-reader-get-value rx)))
                  (assert-not* 'json-reader-get-value (eof-object? value))
                  (assert-not* 'json-reader-get-value (char? value))
                  ;; plist will be reverse before returning it => insert value before key
                  (%read-object rx (plist-add plist value key)
                                  (json-reader-get-value rx))))))))
      (else
        tok0))))


;; skip next json token and return its kind, which can be one of:
;;   #!eof   i.e. the (eof-object)
;;   (void)  i.e. null
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
(define (json-reader-skip-token rx)
  (validate-next-token rx #f))


;; skip next json value and return its kind, which can be one of:
;;   #!eof   i.e. the (eof-object)
;;   (void)  i.e. null
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
(define (json-reader-skip-value rx)
  (let ((tok0 (json-reader-skip-token rx)))
    (if (or (eqv? tok0 #\[) (eqv? tok0 #\{))
      (let %skip-value ((rx rx) (depth0 (json-reader-depth rx)))
        (let ((tok (json-reader-skip-token rx)))
          (if (or (eof-object? tok) (fx<? (json-reader-depth rx) depth0))
            (if (eqv? tok0 #\[) 'array 'object)
            (%skip-value rx depth0))))
      tok0)))


;; forget accumulated stack of states, skip whitespace and restart parsing, expecting a top-level json value or #!eof.
;; useful to parse multiple concatenated json documents, as for example NDJSON standard.
;; does not clear the eof? flag.
(define (json-reader-restart rx)
  (let ((in    (json-reader-in rx))
        (stack (json-reader-stack rx)))
    (skip-ws in)
    (bytespan-resize-right! stack 1)
    (bytespan-set/u8! stack 0 (if (eof-object? (lookahead-u8 in))
                                $done
                                $top))))


;; return obj as obj-reader should do:
;;   either (values obj #t) if it's a valid item
;;   or (values #<unspecified> #f) if it's #!eof
(define (to-item obj)
  (values obj (not (eof-object? obj))))


;; autotect json variant present in input port, and read next item from it:
;; if input port contains one or more top-level json values, for example as NDJSON expects, scan each one sequentially:
;;   if there's no next top-level value, return (values #<unspecified> #f) indicating end-of-file
;;   if next top-level value is a json array, then return its elements one by one as items
;;   otherwise return next top-level value as a single item
;;
;; this function does NOT allow separators : or , after top-level json values
(define (json-reader-get rx)
  (assert* 'json-reader-get (json-reader? rx))
  (obj-reader-get rx))


;; called by (json-reader-get) -> (obj-reader-get)
(define (%json-reader-get rx)
  (let ((in    (json-reader-in rx))
        (depth (json-reader-depth rx)))
    (assert* 'json-reader-get (fx<=? depth 1))
    (if (fxzero? depth)
       ;; in case we already read some previous json document. also skips whitespace.
      (json-reader-restart rx)
      (skip-ws in))
    (case (lookahead-u8 in)
      ((91)  ; #\[
        (cond
          ((fxzero? depth)
            ;; json document is a json array => return its elements one by one as items
            (json-reader-skip-token rx)
            (%json-reader-get rx))
          (else
            ;; found a json array => return it as a single item
            (to-item (json-reader-get-value rx)))))
      ((44 93) ; #\, #\]
        ;; found end of top-level json array,
        ;; or separator between elements in top-level json array.
        ;; skip it and retry.
        (json-reader-skip-token rx)
        (%json-reader-get rx))
      (else
        ;; top-level value is an object, or an atomic value, or a syntax error => return it as a single item
        (to-item (json-reader-get-value rx))))))




