;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file shell/job.ss


;; return charspan containing current directory of specified job.
;; if job's cwd is not set, recursively retrieve it from parent job
;;
;; NOTE: returned charspan must NOT be modified.
(define (job-cwd job)
  (do ((parent job (job-parent parent)))
      ((or (not (sh-job? parent)) (%job-cwd parent))
        (%job-cwd (if (sh-job? parent) parent (sh-globals))))))


;; return (job-cwd job), or #f if it's equal to global working directory.
;;
;; NOTE: returned charspan must NOT be modified.
(define (job-cwd-if-set job)
  (let ((job-dir    (job-cwd job))
        (global-dir (%job-cwd (sh-globals))))
    (if (charspan=? job-dir global-dir)
      #f
      job-dir)))


;; return charspan containing previous directory of specified job.
;; if job's owd is not set, recursively retrieve it from parent job
;;
;; NOTE: returned charspan must NOT be modified.
(define (job-owd job)
  (do ((parent job (job-parent parent)))
      ((or (not (sh-job? parent)) (%job-owd parent))
        (%job-owd (if (sh-job? parent) parent (sh-globals))))))


;; return charspan containing current directory,
;; or charspan containing current directory of specified job-or-id.
;;
;; NOTE: returned charspan must NOT be modified.
(define sh-cwd
  (case-lambda
    (()          (%job-cwd (sh-globals)))
    ((job-or-id) (job-cwd  (sh-job job-or-id)))))


;; set the current directory of a job or job-id to specified path.
;; if job or job-id is not specified, defaults to (sh-globals)
;; if path is not specified, defaults to job's environment variable $HOME.
;; if path is specified, it must be a string or charspan.
;;
;; path will be normalized, and must be an accessible directory on filesystem.
;; if job resolves to (sh-globals), C function chdir() will also be called.
;;
;; Returns (void) if successful, otherwise raises exception.
(define sh-cd
  (case-lambda
    (()
      (job-cd #t (sh-env-ref #t "HOME")))

    ((arg)
      (if (or (string? arg) (charspan? arg))
        (job-cd #t arg)
        (let ((job (sh-job arg)))
          (job-cd job (sh-env-ref job "HOME")))))

    ((job-or-id path)
      (job-cd job-or-id path))

    ((job-or-id path . extra-args)
      (raise-errorf 'cd "too many arguments"))))


;; internal function called by (sh-cd)
;;
;; set the current directory of job to specified path,
;; that will be normalized and must be an accessible directory on filesystem.
;; if job resolves to (sh-globals), C function chdir() will also be called.
;;
;; Returns (void) if successful, otherwise raises exception.
(define (job-cd job-or-id path)
  (let* ((job    (sh-job job-or-id))
         (suffix (text->sh-path* path))
         (dir    (if (sh-path-absolute? suffix)
                   (sh-path->subpath suffix)
                   (sh-path-append (sh-cwd job) suffix)))
         (c-err  (job-cd/bv0 job (bytespan->bytevector*! (charspan->utf8b/0 dir)))))
    (if (= c-err 0)
      (job-cwd-set! job dir)
      (raise-errorf 'cd "~s: ~a"
        (if (string? path) path (charspan->string path))
        (c-errno->string c-err))))) ; returns (failed 1)


;; internal function called by (sh-cd) -> (job-cd)
;;
;; actually calls C functions chdir() or stat()
(define job-cd/bv0
  (let ((c-chdir (foreign-procedure "c_chdir" (ptr) int))
        (c-errno-enotdir ((foreign-procedure "c_errno_enotdir" () int)))
        (c-errno-enoent  ((foreign-procedure "c_errno_enoent" () int))))
    (lambda (job path-bv0)
      (if (eq? job (sh-globals))
        ;; no need to call Chez Scheme (cd path)
        ;; it autodetects that current process changed its current directory
        (c-chdir path-bv0)
        (let ((ret (file-type path-bv0 '(catch))))
          (cond
            ((eq? ret 'dir)
              0)
            ((symbol? ret) ; not a directory
              c-errno-enotdir)
            ((and (fixnum? ret) (fx<? ret 0))
              ret)  ; some C error
            (else          ; no such file or directory, or some other error
              c-errno-enoent)))))))


;; store path as job's current directory.
;; called by (sh-cd), which calls (job-cd) after C function to change the directory succeeded.
(define (job-cwd-set! job path)
  (when (charspan? path)
    (%job-owd-set! job (job-cwd job)) ; save current directory into old directory
    (%job-cwd-set! job path)))        ; save path into current directory


;; set the current directory of a job or job-id to previous working directory stored in (job-cwd).
;; if job or job-id is not specified, defaults to (sh-globals)
;;
;; Returns (void) if successful, otherwise raises exception.
(define sh-cd-
  (case-lambda
    ((job-or-id)
      (let* ((job  (sh-job job-or-id))
             (path (job-owd job)))
        (unless path
          (raise-errorf 'cd- "old working directory is not set for job ~s" job))
        (job-cd job path)))
    (()
      (sh-cd- #t))
    ((job-or-id . extra-args)
      (raise-errorf 'cd- "too many arguments"))))


(define sh-pwd
  (case-lambda
    (()          (sh-pwd* #t (sh-fd 1)))
    ((job-or-id) (sh-pwd* job-or-id (sh-fd 1)))))


(define (sh-pwd* job-or-id fd)
  (let ((wbuf (make-bytespan 0)))
    (bytespan-insert-right/charspan! wbuf (sh-cwd job-or-id))
    (bytespan-insert-right/u8! wbuf 10) ; newline
    (fd-write-all fd (bytespan-peek-data wbuf)
                  (bytespan-peek-beg wbuf) (bytespan-peek-end wbuf))
    (void)))



;; the "cd" builtin: set current directory of job's parent.
;; For safety, throws an exception if setting current directory fails.
;;
;; As all builtins do, must return job status.
(define (builtin-cd job prog-and-args options)
  (assert-string-list? 'builtin-cd prog-and-args)
  (apply sh-cd (or (job-parent job) (sh-globals)) (cdr prog-and-args)))


;; the "cd-" builtin: set current directory of job's parent
;; to its previous value saved in (job-owd)
;; For safety, throws an exception if setting current directory fails.
;;
;; As all builtins do, must return job status.
(define (builtin-cd- job prog-and-args options)
  (assert-string-list? 'builtin-cd- prog-and-args)
  (unless (null? (cdr prog-and-args))
    (raise-errorf 'cd- "too many arguments"))
  (sh-cd- (or (job-parent job) (sh-globals))))


;; the "pwd" builtin: print working directory of parent job,
;; or of specified job-id.
;;
;; As all builtins do, must return job status.
(define (builtin-pwd job prog-and-args options)
  (assert-string-list? 'builtin-pwd prog-and-args)
  (let ((arg (list->integer-or-false (cdr prog-and-args))))
    (sh-pwd (or arg (job-parent job)))))


;; convert string str to an integer, and return such integer.
;;
;; return #f str is not a string, or is an empty string,
;; or contains characters that are not decimal digits.
(define (string->integer-or-false str)
  (and (string? str)
       (string-is-unsigned-base10-integer? str)
       (string->number str)))


;; extract first element of list l, which must be a string,
;; and return it converted to an integer.
;;
;; return #f if l is not a list, or does not have length 1, or first element is not a string,
;; or is an empty string, or contains characters that are not decimal digits.
(define (list->integer-or-false l)
  (and (pair? l)
       (null? (cdr l))
       (string->integer-or-false (car l))))



;; return home directory of specified username, which must be a string.
;;
;; if username is not specified, return environment variable "HOME" for current job,
;; which is expected to contain home directory for current user.
;;
;; otherwise return home directory for specified username, retrieved with C function getpwnam_r()
;;
;; returned value is a string, or #f if an error occurred
(define sh-userhome
  (case-lambda
    (()
      (sh-env-ref #f "HOME" #f))
    ((username)
      (let ((userhome (username->homedir (string->utf8b/0 username))))
        (if (string? userhome)
          userhome
          #f)))))


(define username->homedir (foreign-procedure "c_get_userhome" (ptr) ptr))


;; if subpath is specified, return (string-append (xdg-cache-home) "/" subpath)
;; otherwise return (string-append (xdg-cache-home) "/")
(define xdg-cache-home/
  (case-lambda
    (()         (string-append (xdg-cache-home) "/"))
    ((subpath)  (string-append (xdg-cache-home) "/" subpath))))


;; if subpath is specified, return (string-append (xdg-config-home) "/" subpath)
;; otherwise return (string-append (xdg-config-home) "/")
(define xdg-config-home/
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
