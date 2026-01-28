;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/json.ss

(define json-write-token
  (case-lambda
    ((tok port)
      (cond
        ((string? tok)
          (write tok port))
        ((boolean? tok)
          (put-string port (if tok "true" "false")))
        ((null? tok)
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
    ((value port)
      ;; TODO: implement
      (void))
    ((value)
      (json-write-value value (current-output-port)))))