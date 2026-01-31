;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/json/json.ss


(define-record-type (json-writer %make-json-writer json-writer?)
  (parent obj-writer)
  (fields
    out                   ; textual input port
    (mutable prologue?)   ; #t before first call to any (json-writer-put...) function
    (mutable epilogue?))  ; #t if we should write #\] before closing the port
  (protocol
    (lambda (args->new)
      (lambda (out)
        ((args->new %json-writer-put %json-writer-close)
          out #t #f))))
  (nongenerative %json-writer-7c46d04b-34f4-4046-b5c7-b63753c1be40))


(define make-json-writer
  (case-lambda
    ((out)
      (assert* 'make-json-writer (port? out))
      (assert* 'make-json-writer (textual-port? out))
      (assert* 'make-json-writer (output-port? out))
      (%make-json-writer out))
    (()
      (make-json-writer (current-output-port)))))


(define (json-writer-eof? tx)
  (assert* 'json-writer-eof? (json-writer? tx))
  (obj-writer-eof? tx))


(define (json-writer-close tx)
  (assert* 'json-writer-close (json-writer? tx))
  (obj-writer-close tx))


;; called by (json-writer-close) -> (obj-writer-close)
(define (%json-writer-close tx)
  (let ((out (json-writer-out tx)))
    (when (json-writer-epilogue? tx)
      (put-string out "\n]\n")
      (json-writer-epilogue?-set! tx #f)
      (json-writer-prologue?-set! tx #t))
    (unless (port-closed? out)
      (close-port out))))


(define (json-writer-put-token tx tok)
  (let ((out (json-writer-out tx)))
    (cond
      ((string? tok)
        ;; FIXME must escape characters #\x0 ... #\x1f with JSON syntax, not scheme syntax
        (write tok out))
      ((boolean? tok)
        (put-string out (if tok "true" "false")))
      ((eq? tok (void))
        (put-string out "null"))
      ((char? tok)
        (put-char out tok))
      ((number? tok)
        (write tok out))
      ((eof-object? tok)
        (void))
      (else
        (raise-errorf 'json-writer-put-token "unsupported token: ~s" tok))))
  (json-writer-prologue?-set! tx #f))


(define (json-writer-put-value tx obj)
  (let ((out (json-writer-out tx)))
    (cond
      ((null? obj)
        (put-string out "{}"))
      ((pair? obj)
        (assert* 'json-writer-put-value (plist? obj))
        (put-char out #\{)
        (let ((first? #t))
          (for-plist ((key val obj))
            (if first?
              (set! first? #f)
              (put-char out #\,))
            (assert* 'json-writer-put-value (symbol? key))
            ; FIXME must escape characters #\x0 ... #\x1f with JSON syntax, not scheme syntax
            (write (symbol->string key) out)
            (put-char out #\:)
            (json-writer-put-value tx val)))
        (put-char out #\}))
      ((record? obj)
        (assert* 'json-writer-put-value (span? obj))
        (put-char out #\[)
        (do ((i 0 (fx1+ i))
             (n (span-length obj)))
            ((fx>=? i n))
          (unless (fxzero? i)
            (put-char out #\,))
          (json-writer-put-value tx (span-ref obj i)))
        (put-char out #\]))
      (else
        (json-writer-put-token tx obj))))
  (json-writer-prologue?-set! tx #f))


(define (json-writer-put tx obj)
  (assert* 'json-writer-put (json-writer? tx))
  (obj-writer-put tx obj))


;; called by (json-writer-put) -> (obj-writer-put)
(define (%json-writer-put tx obj)
  (let ((out (json-writer-out tx)))
    (if (json-writer-prologue? tx)
      (begin
        (put-string out "[\n")
        (json-writer-prologue?-set! tx #f)
        (json-writer-epilogue?-set! tx #t))
      (put-string out ",\n"))
  (json-writer-put-value tx obj)))

