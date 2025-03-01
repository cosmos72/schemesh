;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; Define the functions (s-fd-allocate) (s-fd-release) to manage reserved fds


(library (schemesh shell fds (0 7 7))
  (export
    s-fd s-fd* s-fd? s-fd->int s-fd-copy s-fd-allocate s-fd-release
    sh-fd-stdin sh-fd-stdout sh-fd-stderr)
  (import
    (rnrs)
    (only (chezscheme) fx1+ fx1- open-fd-output-port record-writer)
    (only (schemesh bootstrap) assert* sh-make-thread-parameter raise-errorf)
    (schemesh containers bitmap)
    (only (schemesh containers hashtable) hashtable-iterate)
    (only (schemesh posix fd) fd-open-max))


;; 1+ highest reserved fd
(define fd-max (fd-open-max))

;; lowest reserved fd
(define fd-min (fx* 3 (fxarithmetic-shift-right fd-max 2)))

;; bitmap of reserved fds
(define fd-bitmap (make-bitmap (fx- fd-max fd-min)))


;; reference-counted file descriptor
(define-record-type
  (%s-fd %make-s-fd s-fd?)
  (fields
    (immutable int s-fd->int) ; unsigned fixnum: file descriptor
    (mutable   refcount))     ; fixnum: reference count
  (nongenerative #{%s-fd cjsh4sku94arywo64878o3mil-0}))


;; wrap a file descriptor (an unsigned fixnum) and an optional reference count into s-fd
(define s-fd
  (case-lambda
    ((int)          (s-fd* int 1))
    ((int refcount) (s-fd* int refcount))))


;; wrap a file descriptor (an unsigned fixnum) and a mandatory reference count into s-fd
(define (s-fd* int refcount)
  (assert* 's-fd (fx<? -1 int fd-max))
  (assert* 's-fd (fx>=? refcount 0))
  (%make-s-fd int refcount))


;; increase by one the reference count of an s-fd
;; return the s-fd argument
(define (s-fd-copy fd)
  (assert* 's-fd-copy (s-fd? fd))
  (%s-fd-refcount-set! fd (fx1+ (%s-fd-refcount fd)))
  fd)


;; reserve a fd, return its number wrapped inside a s-fd
(define (s-fd-allocate)
  (let ((index (bitmap-last-zero fd-bitmap)))
    (when (fx<? index 0)
      (raise-errorf 's-fd-allocate "too many reserved fds: ~s" index))
    (bitmap-set! fd-bitmap index 1)
    (s-fd (fx+ fd-min index))))


;; decrease by one the reference count of an s-fd,
;; unreserve it if reference count becomes <= zero
;; return #t if if reference count becomes <= zero
(define (s-fd-release fd)
  (assert* 's-fd-release (s-fd? fd))
  (let* ((refcount      (fx1- (%s-fd-refcount fd)))
         (unreserve? (fx<=? refcount 0)))
    (%s-fd-refcount-set! fd refcount)
    (when unreserve?
      (let ((index (fx- (s-fd->int fd) fd-min)))
        (assert* 's-fd-release (fx=? 1 (bitmap-ref fd-bitmap index)))
        (bitmap-set! fd-bitmap index 0)))
    unreserve?))


;; thread parameter (sh-fd-stdin) must be an unsigned fixnum,
;; it is used as shell's standard input file descriptor
(define sh-fd-stdin
  (sh-make-thread-parameter
    0
    (lambda (fd)
      (unless (and (fixnum? fd) (fx<? -1 fd fd-max))
        (raise-errorf 'sh-fd-stdin "invalid file descriptor, must be a fixnum in [0, fd-max): ~s" fd))
      fd)))


;; thread parameter (sh-fd-stdout) must be an unsigned fixnum,
;; it is used as shell's standard output file descriptor
(define sh-fd-stdout
  (sh-make-thread-parameter
    1
    (lambda (fd)
      (unless (and (fixnum? fd) (fx<? -1 fd fd-max))
        (raise-errorf 'sh-fd-stdout "invalid file descriptor, must be a fixnum in [0, fd-max): ~s" fd))
      fd)))


;; thread parameter (sh-fd-stderr) must be an unsigned fixnum,
;; it is used as shell's standard error file descriptor
(define sh-fd-stderr
  (sh-make-thread-parameter
    2
    (lambda (fd)
      (unless (and (fixnum? fd) (fx<? -1 fd fd-max))
        (raise-errorf 'sh-fd-stderr "invalid file descriptor, must be a fixnum in [0, fd-max): ~s" fd))
      fd)))


; customize how "s-fd" objects are printed
(record-writer (record-type-descriptor %s-fd)
  (lambda (fd port writer)
    (display "(s-fd " port)
    (display (s-fd->int fd) port)
    (let ((refcount (%s-fd-refcount fd)))
      (unless (fx=? 1 refcount)
        (display #\space port)
        (display refcount port)))
    (display ")" port)))

) ; close library
