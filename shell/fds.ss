;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; Define the functions (s-fd-allocate) (s-fd-release) to manage reserved fds
;; Define the record-type (s-fd) containing a reference-counted file descriptor


(library (schemesh shell fds (1 0 2))
  (export
    s-fd s-fd* s-fd? s-fd->int s-fd-copy s-fd-allocate s-fd-release)
  (import
    (rnrs)
    (only (chezscheme) fx1+ fx1- record-writer)
    (only (scheme2k bootstrap) assert* raise-errorf)
    (scheme2k containers bitmap)
    (only (scheme2k posix fd) fd-open-max))


;; 1+ highest reserved fd
(define fd-max (fd-open-max))

;; lowest reserved fd
(define fd-min (fx* 3 (fxarithmetic-shift-right fd-max 2)))

;; bitmap of reserved fds
(define fd-bitmap (make-bitmap (fx- fd-max fd-min)))


;; reference-counted file descriptor
(define-record-type (%s-fd %make-s-fd s-fd?)
  (fields
    (immutable int s-fd->int) ; unsigned fixnum: file descriptor
    (mutable   refcount))     ; fixnum: reference count
  (nongenerative %s-fd-7c46d04b-34f4-4046-b5c7-b63753c1be39))


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


;; an s-fd indicating "close this file descriptor"
(define s-fd-1 (%make-s-fd -1 0))

;; reserve a fd, return its number wrapped inside a s-fd
(define (s-fd-allocate to-fd-or-bytevector)
  (if (eqv? -1 to-fd-or-bytevector)
    s-fd-1
    (let ((index (bitmap-last-zero fd-bitmap)))
      (when (fx<? index 0)
        (raise-errorf 's-fd-allocate "too many reserved fds: ~s" index))
      (bitmap-set! fd-bitmap index 1)
      (s-fd (fx+ fd-min index)))))


;; decrease by one the reference count of an s-fd.
;; if reference count becomes <= zero, unreserve it and return #t
;; otherwise return #f
(define (s-fd-release fd)
  (assert* 's-fd-release (s-fd? fd))
  (let ((refcount   (fx1- (%s-fd-refcount fd)))
        (fd-int     (s-fd->int fd)))
    (when (fx>=? refcount 0)
      (%s-fd-refcount-set! fd refcount))
    (and (fx>=? fd-int 0) ;; never release nor close file descriptor -1
         (fx<=? refcount 0)
         (let ((index (fx- fd-int fd-min)))
           (assert* 's-fd-release (fx=? 1 (bitmap-ref fd-bitmap index)))
           (bitmap-set! fd-bitmap index 0)
           #t))))


;; customize how "s-fd" objects are printed
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
