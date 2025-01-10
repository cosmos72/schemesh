;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should only be included inside a (library ...) definition


;; return charspan containing current directory,
;; or charspan containing current directory of specified job-or-id.
(define sh-cwd
  (case-lambda
    (()          (job-cwd sh-globals))
    ((job-or-id) (job-cwd (sh-job job-or-id)))))


;; set the current directory of specified job or job-id to specified path.
;; path must be a string or charspan.
;;
;; if job-or-id resolves to sh-globals, it is equivalent to (sh-cd path).
;;
;; in all other cases, path is taken as-is, i.e. it is not normalized
;; and is not validated against filesystem contents.
(define (sh-cwd-set! job-or-id path)
  (let ((job (sh-job job-or-id)))
    (if (eq? job sh-globals)
      (sh-cd path)
      (job-cwd-set! job (if (charspan? path) path (string->charspan* path))))))



;; change current directory to specified path.
;; path must be a a string or charspan.
(define sh-cd
  (case-lambda
    (()     (sh-cd* (sh-env sh-globals "HOME")))
    ((path) (sh-cd* path))
    ((path . extra-args) (raise-errorf 'cd "too many arguments"))))


;; internal function called by (sh-cd)
(define sh-cd*
  (let ((c_chdir (foreign-procedure "c_chdir" (ptr) int)))
    (lambda (path)
      (let* ((suffix (text->sh-path* path))
             (dir (if (sh-path-absolute? suffix)
                      (sh-path->subpath suffix)
                      (sh-path-append (sh-cwd) suffix)))
             (err (c_chdir (string->utf8b/0 (charspan->string dir)))))
        (if (= err 0)
          (job-cwd-set! sh-globals dir)
          (raise-errorf 'cd "~a: ~a" path (c-errno->string err)))))))


(define sh-pwd
  (case-lambda
    (()   (sh-pwd* (sh-fd-stdout)))
    ((fd) (sh-pwd* fd))))


(define (sh-pwd* fd)
  (let ((wbuf (make-bytespan 0)))
    (bytespan-insert-back/cspan! wbuf (sh-cwd))
    (bytespan-insert-back/u8! wbuf 10) ; newline
     ; TODO: loop on short writes
    (fd-write fd (bytespan-peek-data wbuf)
              (bytespan-peek-beg wbuf) (bytespan-peek-end wbuf))
    (void)))



;; the "cd" builtin
(define (builtin-cd job prog-and-args options)
  (assert-string-list? 'sh-builtin-cd prog-and-args)
  (apply sh-cd (cdr prog-and-args)))


;; the "pwd" builtin
(define (builtin-pwd job prog-and-args options)
  (assert-string-list? 'sh-builtin-pwd prog-and-args)
  (sh-pwd))
