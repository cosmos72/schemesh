;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k posix base (1 0 1))
  (export
    c-errno c-errno-einval c-errno->string c-exit c-hostname raise-c-errno warn-c-errno)
  (import
    (rnrs)
    (only (chezscheme)             foreign-procedure format)
    (only (scheme2k bootstrap)     raise-errorf))

(define c-errno         (foreign-procedure "c_errno" () int))
(define c-errno-einval  ((foreign-procedure "c_errno_einval" () int))) ;; integer, not a procedure
(define c-errno->string (foreign-procedure "c_errno_to_string" (int) ptr))
(define c-exit          (foreign-procedure "c_exit" (int) int))

(define c-hostname
  (let* ((hostname-or-error ((foreign-procedure "c_get_hostname" () ptr)))
         (hostname (if (string? hostname-or-error) hostname-or-error "???")))
    (lambda ()
      hostname)))

(define (raise-c-errno who c-who c-errno . c-args)
  ; (debugf "raise-c-errno ~s ~s" who c-errno)
  (raise-errorf who "C function ~s~s failed with error ~s: ~a"
    c-who c-args c-errno (if (integer? c-errno) (c-errno->string c-errno) "unknown error")))

(define (warn-c-errno who c-who c-errno . c-args)
  (let ((port (current-error-port)))
    (format port "\x1b;[1;33m; Warning in ~s: C function ~s~s failed with error ~s: ~a\x1b;[m\n"
            who c-who c-args c-errno (c-errno->string c-errno))
    (flush-output-port port)))

) ; close library
