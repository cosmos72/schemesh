;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; Define the functions (sh-fd-allocate) (sh-fd-release) to manage reserved fds


(library (schemesh shell fds (0 7 0))
  (export
    sh-fd sh-fd* sh-fd? sh-fd->int sh-fd-copy sh-fd-allocate sh-fd-release sh-fd-stdin sh-fd-stdout sh-fd-stderr)
  (import
    (rnrs)
    (only (chezscheme) fx1+ fx1- make-thread-parameter record-writer)
    (only (schemesh bootstrap) assert* raise-errorf)
    (schemesh containers bitmap)
    (only (schemesh posix fd) fd-open-max))


;; 1+ highest reserved fd
(define fd-max (fd-open-max))

;; lowest reserved fd
(define fd-min (fx* 3 (fxarithmetic-shift-right fd-max 2)))

;; bitmap of reserved fds
(define fd-bitmap (make-bitmap (fx- fd-max fd-min)))


;; reference-counted file descriptor
(define-record-type
  (%sh-fd %make-sh-fd sh-fd?)
  (fields
    (immutable int sh-fd->int) ; unsigned fixnum: file descriptor
    (mutable   refcount))      ; fixnum: reference count
  (nongenerative #{%sh-fd cjsh4sku94arywo64878o3mil-0}))


;; wrap a file descriptor (an unsigned fixnum) and an optional reference count into sh-fd
(define sh-fd
  (case-lambda
    ((int)          (sh-fd* int 1))
    ((int refcount) (sh-fd* int refcount))))


;; wrap a file descriptor (an unsigned fixnum) and a mandatory reference count into sh-fd
(define (sh-fd* int refcount)
  (assert* 'sh-fd (fx<? -1 int fd-max))
  (assert* 'sh-fd (fx>=? refcount 0))
  (%make-sh-fd int refcount))


;; increase by one the reference count of an sh-fd
;; return the sh-fd
(define (sh-fd-copy fd)
  (assert* 'sh-fd-copy (sh-fd? fd))
  (%sh-fd-refcount-set! fd (fx1+ (%sh-fd-refcount fd)))
  fd)


;; reserve a fd, return its number wrapped inside a sh-fd
(define (sh-fd-allocate)
  (let ((index (bitmap-last-zero fd-bitmap)))
    (when (fx<? index 0)
      (raise-errorf 'sh-fd-allocate "too many reserved fds: " index))
    (bitmap-set! fd-bitmap index 1)
    (sh-fd (fx+ fd-min index))))


;; decrease by one the reference count of an sh-fd,
;; unreserve it if reference count becomes <= zero
;; return #t if if reference count becomes <= zero
(define (sh-fd-release fd)
  (assert* 'sh-fd-release (sh-fd? fd))
  (let* ((refcount      (fx1- (%sh-fd-refcount fd)))
         (unreserve? (fx<=? refcount 0)))
    (%sh-fd-refcount-set! fd refcount)
    (when unreserve?
      (let ((index (fx- (sh-fd->int fd) fd-min)))
        (assert* 'sh-fd-release (fx=? 1 (bitmap-ref fd-bitmap index)))
        (bitmap-set! fd-bitmap index 0)))
    unreserve?))


;; thread parameter (sh-fd-stdin) must be an unsigned fixnum,
;; it is used as shell's standard input file descriptor
(define sh-fd-stdin
  (make-thread-parameter
    0
    (lambda (fd)
      (unless (and (fixnum? fd) (fx<? -1 fd fd-max))
        (raise-errorf 'sh-fd-stdin "invalid file descriptor, must be a fixnum in [0, fd-max - 1]: ~s" fd))
      fd)))


;; thread parameter (sh-fd-stdout) must be an unsigned fixnum,
;; it is used as shell's standard output file descriptor
(define sh-fd-stdout
  (make-thread-parameter
    1
    (lambda (fd)
      (unless (and (fixnum? fd) (fx<? -1 fd fd-max))
        (raise-errorf 'sh-fd-stdout "invalid file descriptor, must be a fixnum in [0, fd-max - 1]: ~s" fd))
      fd)))


;; thread parameter (sh-fd-stderr) must be an unsigned fixnum,
;; it is used as shell's standard error file descriptor
(define sh-fd-stderr
  (make-thread-parameter
    2
    (lambda (fd)
      (unless (and (fixnum? fd) (fx<? -1 fd fd-max))
        (raise-errorf 'sh-fd-stderr "invalid file descriptor, must be a fixnum in [0, fd-max - 1]: " fd))
      fd)))


; customize how "sh-fd" objects are printed
(record-writer (record-type-descriptor %sh-fd)
  (lambda (fd port writer)
    (display "(sh-fd " port)
    (display (sh-fd->int fd) port)
    (let ((refcount (%sh-fd-refcount fd)))
      (unless (fx=? 1 refcount)
        (display #\space port)
        (display refcount port)))
    (display ")" port)))

) ; close library
