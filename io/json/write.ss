;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/json/json.ss

(define json-write-token
  (case-lambda
    ((tok port)
      (cond
        ((string? tok)
          ;; FIXME must escape characters #\x0 ... #\x1f with JSON syntax, not scheme syntax
          (write tok port))
        ((boolean? tok)
          (put-string port (if tok "true" "false")))
        ((eq? tok (void))
          (put-string port "null"))
        ((char? tok)
          (put-char port tok))
        ((number? tok)
          (write tok port))
        ((eof-object? tok)
          (void))
        (else
          (raise-errorf 'json-write-token "unsupported token: ~s" tok))))
    ((tok)
      (json-write-token tok (current-output-port)))))


(define json-write-value
  (case-lambda
    ((obj port)
      (cond
        ((null? obj)
          (put-string port "{}"))
        ((pair? obj)
          (assert* 'json-write-value (plist? obj))
          (put-char port #\{)
          (let ((first? #t))
            (for-plist ((key val obj))
              (if first?
                (set! first? #f)
                (put-char port #\,))
              (assert* 'json-write-value (symbol? key))
              ; FIXME must escape characters #\x0 ... #\x1f with JSON syntax, not scheme syntax
              (write (symbol->string key) port)
              (put-char port #\:)
              (json-write-value val port)))
          (put-char port #\}))
        ((record? obj)
          (assert* 'json-write-value (span? obj))
          (put-char port #\[)
          (do ((i 0 (fx1+ i))
               (n (span-length obj)))
              ((fx>=? i n))
            (unless (fxzero? i)
              (put-char port #\,))
            (json-write-value (span-ref obj i) port))
          (put-char port #\]))
        (else
          (json-write-token obj port))))
    ((obj)
      (json-write-value obj (current-output-port)))))
