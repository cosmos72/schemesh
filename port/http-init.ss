;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; scheme wrapper around libcurl

(library (schemesh port http init (0 9 2))
  (export
    http-library-path http-init string->http-condition)
  (import
    (rnrs)
    (only (chezscheme)                foreign-procedure format load-shared-object
                                      make-parameter make-continuation-condition)
    (only (schemesh bootstrap)        assert*)
    (only (schemesh containers utf8b) utf8b->string))


(define http-library-path (make-parameter
    "/usr/local/lib/libchez_curl.so.0"
    (lambda (path)
      (assert* 'http-library-path (string? path))
      path)))


(define (http-init)
  (load-shared-object (http-library-path))
  (let ((err ((foreign-procedure "http_global_init" () int))))
    (unless (zero? err)
      (let ((str (utf8b->string ((foreign-procedure __collect_safe "http_global_strerror" (int) u8*) err))))
        (raise (string->http-condition 'http-init str))))))


(define (string->http-condition who str)
  (call/cc
    (lambda (k)
      (condition
        (make-error)
        (make-continuation-condition k)
        (make-non-continuable-violation)
        (make-who-condition (if (symbol? who) who (format #f "~s" who)))
        ;; (make-format-condition)
        (make-message-condition str)))))

)
