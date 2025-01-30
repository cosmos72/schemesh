;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only from file shell/job.ss


;; return charspan containing current directory of specified job.
;; if job's cwd is not set, recursively retrieve it from parent job
(define (job-cwd job)
  (do ((parent job (job-parent parent)))
      ((or (not (sh-job? parent)) (%job-cwd parent))
        (%job-cwd (if (sh-job? parent) parent (sh-globals))))))


;; return (job-cwd job), or #f if it's equal to global working directory.
(define (job-cwd-if-set job)
  (let ((job-dir    (job-cwd job))
        (global-dir (%job-cwd (sh-globals))))
    (if (charspan=? job-dir global-dir)
      #f
      job-dir)))


;; return charspan containing current directory,
;; or charspan containing current directory of specified job-or-id.
;;
;; NOTE: returned charspan must NOT be modified.
(define sh-cwd
  (case-lambda
    (()          (%job-cwd (sh-globals)))
    ((job-or-id) (job-cwd (sh-job job-or-id)))))


;; set the current directory of specified job or job-id to specified path.
;; path must be a string or charspan.
;;
;; if job-or-id resolves to (sh-globals), it is equivalent to (sh-cd path).
;;
;; in all other cases, path is taken as-is, i.e. it is not normalized
;; and is not validated against filesystem contents.
(define (sh-cwd-set! job-or-id path)
  (let ((job (sh-job job-or-id)))
    (if (eq? job (sh-globals))
      (sh-cd path)
      (job-cwd-set! job (if (charspan? path)
                          (charspan-copy path)
                          (string->charspan path))))))



;; change current directory to specified path.
;; path must be a a string or charspan.
(define sh-cd
  (case-lambda
    (()     (sh-cd* (sh-env-ref #t "HOME")))
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
          (job-cwd-set! (sh-globals) dir)
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
  (assert-string-list? 'builtin-cd prog-and-args)
  (apply sh-cd (cdr prog-and-args)))


;; the "pwd" builtin
(define (builtin-pwd job prog-and-args options)
  (assert-string-list? 'builtin-pwd prog-and-args)
  (sh-pwd))




;; return home directory of specified username, which must be a string.
;;
;; if username is not specified, return global environment variable "HOME",
;; which is expected to contain home directory for current user.
;;
;; otherwise return home directory for specified username, retrieved with C function getpwnam_r()
;;
;; returned value is a string, or #f if an error occurred
(define sh-userhome
  (case-lambda
    (()
      (sh-env-ref #t "HOME" #f))
    ((username)
      (let ((userhome (get-userhome (string->utf8b/0 username))))
        (if (string? userhome)
          userhome
          #f)))))


(define get-userhome (foreign-procedure "c_get_userhome" (ptr) ptr))


;; if subpath is specified, return (string-append (xdg-cache-home) "/" subpath)
;; otherwise return (string-append (xdg-cache-home) "/")
(define sh-xdg-cache-home/
  (case-lambda
    (()         (string-append (xdg-cache-home) "/"))
    ((subpath)  (string-append (xdg-cache-home) "/" subpath))))


;; if subpath is specified, return (string-append (xdg-config-home) "/" subpath)
;; otherwise return (string-append (xdg-config-home) "/")
(define sh-xdg-config-home/
  (case-lambda
    (()         (string-append (xdg-config-home) "/"))
    ((subpath)  (string-append (xdg-config-home) "/" subpath))))


;; return string containing environment variable $XDG_CACHE_HOME
;; or $HOME/.cache if the former is not set.
(define (xdg-cache-home)
  (or
    (sh-env-ref #t "XDG_CACHE_HOME" #f)
    (string-append (sh-env-ref #t "HOME" "") "/.cache")))


;; return string containing environment variable $XDG_CONFIG_HOME
;; or $HOME/.config if the former is not set.
(define (xdg-config-home)
  (or
    (sh-env-ref #t "XDG_CONFIG_HOME" #f)
    (string-append (sh-env-ref #t "HOME" "") "/.config")))
