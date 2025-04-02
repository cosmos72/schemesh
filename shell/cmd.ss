;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file shell/job.ss


(define (make-sh-cmd program-and-args)
  (let ((current-job (sh-current-job)))
    (%make-cmd
      #f #f #f #f     ; id oid pid pgid
      (new) #f       ; last-status exception
      (span) 0 #f     ; redirections
      cmd-start #f    ; start-proc step-proc
      #f #f           ; working directory, old working directory - initially inherited from parent job
      #f              ; overridden environment variables - initially none
      #f              ; env var assignments - initially none
      (and current-job (job-parent current-job)) ; temp parent job
      (or current-job (sh-globals))              ; default parent job
      program-and-args
      #f)))           ; expanded arg-list


;; Create a cmd to later spawn it. Each argument must be a string.
;; If you want to use procedures as args, see (sh-cmd*)
(define (sh-cmd . program-and-args)
  (assert-string-list? 'make-sh-cmd program-and-args)
  (make-sh-cmd program-and-args))


;; NOTE: this is an internal implementation function, use (sh-start) instead.
;; This function does not update job's status, and does not register job
;; into global (sh-pid-table) nor into global job-id table.
;;
;; Description:
;; Call any procedure found in (cmd-arg-list c),
;; then repeatedly expand aliases in the produced argument list.
;;
;; If the argument list is empty, copy overridden environment variables,
;; including lazy ones, to parent process. This implements the syntax "ENV_VAR" '= "VALUE"
;;
;; Otherwise, if the argument list starts with the name of a builtin,
;; execute such builtin.
;;
;; Otherwise, start a command i.e. fork() and exec() an external process,
;; optionally inserting it into an existing process group.
;;
;; The new process is started in background, i.e. the foreground process group is NOT set
;; to the process group of the newly created process.
;;
;; Options is a list of zero or more of the following:
;;   process-group-id: an integer, if present and > 0 the new process will be inserted
;;   into the corresponding process group id - which must already exist.
;;
;; Returns job status, which is also stored in (sh-job-last-status)
;;   and may be one of (void) (ok ...) (failed ...) (running ...) (stopped ...) (killed ...) (exception ...) etc.
;;   For the complete list of possible returned job statuses, see (sh-job-status).
(define (cmd-start c options)
  (assert* 'sh-cmd (eq? 'running (job-last-status->kind c)))
  (job-status-set! 'cmd-start c
    (let ((prog-and-args (cmd-arg-list c)))
      (if (string-list? prog-and-args)

        ;; all command line arguments are strings, proceed
        (start-command-or-builtin-or-alias c prog-and-args options)

        ;; some command line argument is a procedure or sh-expr:
        ;; setup fds remapping before calling them, because they may want to use (sh-fd N)
        (begin
          (job-remap-fds! c)
          (let ((l (cmd-arg-list-call-sh-expr-and-procedures c prog-and-args)))
            (if (or (pair? l) (null? l))
              (start-command-or-builtin-or-alias c l options)
              l)))))))


;; internal function called by (cmd-start):
;; call procedures in prog-and-args.
;; Return the expanded command line, which is always a list of strings.
;;
;; if calling procedures in prog-and-args raises a condition,
;; return the condition object wrapped in an (exception) or  (killed) status.
(define (cmd-arg-list-call-sh-expr-and-procedures c prog-and-args)
  (try
    (let %loop-call-sh-expr-and-procedures ((args prog-and-args) (l '()))
      (if (null? args)
        (let ((l (reverse! l)))
          (assert-string-list? 'sh-start l)
          ;; return list of strings
          l)
        (let ((l (cmd-arg-call-sh-expr-or-procedure c (car args) l)))
          (%loop-call-sh-expr-and-procedures (cdr args) l))))
    (catch (ex)
      (if (received-signal? ex)
        ;; return a (killed) status containing the signal name extracted from (received-signal) condition object
        (killed (received-signal-name ex))
        ;; return an (exception) status containing the condition object
        (exception ex)))))


;; internal function called by (cmd-start):
;; if arg is a procedure then call it, optionally passing current job as the only argument.
;; Such procedure must return a string or list-of-strings, which are reverse-consed
;; at the beginning of list-of-strings l.
;; Return the updated list.
(define (cmd-arg-call-sh-expr-or-procedure c arg l)
  (let ((expanded
          (cond
            ((sh-expr? arg) (ok->values (sh-run arg)))    ; run sh-expr job, raise exception if zero or 2+ results
            ((not (procedure? arg)) arg)
            ((logbit? 1 (procedure-arity-mask arg)) (arg c)) ; call (proc job)
            (else (arg)))))                                  ; call (proc)
    ; (debugf "cmd-arg-call-sh-expr-or-procedure cmd=~s arg=~s expanded=~s l=~s" c arg expanded l)
    (cond
      ((eq? (void) expanded)
        l)
      ((null? expanded)
        l)
      ((pair? expanded)
        (for-list ((e expanded))
          (assert* 'sh-start (string? e))
          (set! l (cons e l)))
        l)
      ((string? expanded)
        (cons expanded l))
      (else
        (raise-errorf 'sh-start "value ~s returned by ~s in job ~s is not a string, a list of strings, or (void)"
          expanded arg c)))))


;; NOTE: this is an internal implementation function, use (sh-start) instead.
;; This function does not update job's status, and does not register job
;; into global (sh-pid-table) nor into global job-id table.
;;
;; Description:
;; Repeatedly expand aliases in args
;;
;; If the expanded argument list is empty, copy overridden environment variables,
;; including lazy ones, to parent process. This implements the syntax "ENV_VAR" '= "VALUE"
;;
;; Otherwise, if the expand argument list starts with the name of a builtin,
;; execute such builtin.
;;
;; Otherwise, start a command i.e. fork() and exec() an external process,
;; optionally inserting it into an existing process group.
;;
;; Returns job status,
;;   which may be one of (void) (failed ...) (running ...) etc.
;;   For the complete list of possible returned job statuses, see (sh-job-status).
(define (start-command-or-builtin-or-alias c program-and-args options)
  (assert* 'sh-cmd (sh-cmd? c))

  ;; expand aliases in args
  ;; sanity: (sh-aliases-expand) ignores aliases for "builtin"
  (let ((prog-and-args (sh-aliases-expand program-and-args)))

    ;; (debugf "cmd-start expanded-prog-and-args=~s builtin=~s" prog-and-args builtin)
    (unless (eq? prog-and-args (cmd-arg-list c))
      ;; save expanded cmd-arg-list for more accurate pretty-printing
      (cmd-expanded-arg-list-set! c prog-and-args))

    ;; apply lazy environment variables *after* expanding cmd-arg-list
    ;; and *after* setting job's parent
    (job-env/apply-lazy! c 'export)

    (if (null? prog-and-args)
      (job-env-copy-into-parent! c) ; return (void) job status = success
      (let ((builtin (sh-find-builtin prog-and-args)))
        (if builtin
          ; expanded arg[0] is a builtin, call it.
          (builtin-start builtin c prog-and-args options)  ; returns job status
          ; expanded arg[0] is a not builtin or alias, spawn a subprocess
          (cmd-spawn c prog-and-args options)))))) ; returns job status


;; returns job status
(define (start-command-or-builtin-or-alias-from-another-builtin c program-and-args options)
  (assert* 'sh-cmd (sh-cmd? c))

  ;; expand aliases in args
  ;; sanity: (sh-aliases-expand) ignores aliases for "builtin"
  (let ((prog-and-args (sh-aliases-expand program-and-args)))
    ;; (debugf "cmd-start expanded-prog-and-args=~s builtin=~s" prog-and-args builtin)
    (unless (eq? prog-and-args (cmd-arg-list c))
      ;; save expanded cmd-arg-list for more accurate pretty-printing
      (cmd-expanded-arg-list-set! c prog-and-args))
    ; lazy environment was applied already by the outer (start-command-or-builtin-or-alias)
    (if (null? prog-and-args)
      (void) ; return success
      (let ((builtin (sh-find-builtin prog-and-args)))
        (if builtin
          ; expanded arg[0] is a builtin, call it.
          (builtin-start builtin c prog-and-args options) ; returns job status
          ; expanded arg[0] is a not builtin or alias, spawn a subprocess
          (cmd-spawn c prog-and-args options)))))) ; returns job status


;; internal function called by (cmd-start) to spawn a subprocess.
;; returns job status.
(define cmd-spawn
  (let ((c-cmd-spawn (foreign-procedure "c_cmd_spawn" (ptr ptr ptr ptr int) int)))
    (lambda (c prog-and-args options)
      (let* ((process-group-id (options->process-group-id options))
             (job-dir (job-cwd-if-set c))
             (ret (c-cmd-spawn
                    (list->argv prog-and-args)
                    (if job-dir (text->bytevector0 job-dir) #f)
                    (job-make-c-redirect-vector c)
                    (sh-env->argv c 'export)
                    (or process-group-id -1))))
        ;; (debugf "cmd-spawn pid=~s prog-and-args=~s job=~s " ret prog-and-args c)
        (when (< ret 0)
          (job-status-set! 'cmd-spawn c (failed ret))
          (raise-c-errno 'sh-start 'fork ret))
        (job-pid-set! c ret)
        (job-pgid-set! c process-group-id)
        (job-status-set/running! c)))))


;; internal function called by (builtin-exec) to exec a subprocess.
;; if C exec() fails, returns job status.
(define cmd-exec
  (let ((c-cmd-exec (foreign-procedure "c_cmd_exec" (ptr ptr ptr ptr) int)))
    (lambda (c argv options)
      (let* ((job-dir (job-cwd-if-set c))
             (ret (c-cmd-exec
                    argv
                    (if job-dir (text->bytevector0 job-dir) #f)
                    (job-make-c-redirect-vector c)
                    (sh-env->argv c 'export))))
        ; (c-cmd-exec) returns only if it failed
        (failed (if (and (integer? ret) (not (zero? ret))) ret -1))))))


;; internal function called by (cmd-spawn)
;; creates and fills a vector with job's redirections and its parents redirections
(define (job-make-c-redirect-vector job)
  (let* ((child-dir (job-cwd-if-set job))
         (n (job-count-c-redirect-vector job 0))
         (v (make-vector n)))
    (do ((parent job (job-parent parent)))
        ((not parent))
      (set! n (job-fill-c-redirect-vector/norecurse parent child-dir v n)))
    ; (debugf "job-make-c-redirect-vector job=~s redirect-vector=~s" job v)
    v))


;; count and return the total number of redirections (* 4) of a job,
;; including its parents redirections
(define (job-count-c-redirect-vector job n)
  (if job
    ; add job's redirect count to n, and recurse to parent
    (job-count-c-redirect-vector (job-parent job) (fx+ n (span-length (job-redirects job))))
    n))


;; copy job's redirections to vector v, without recursing to job's parents.
;; returns (fx- pos (number-of-copied-elements))
(define (job-fill-c-redirect-vector/norecurse job child-dir v end-pos)
  (let ((parent-dir (job-cwd-if-set job))
        (n          (span-length (job-redirects job))))
    (do ((index (fx- n 4)  (fx- index 4))
         (pos   end-pos    (fx- pos 4)))
        ((fx<? index 0) pos)
      (job-fill-c-redirect-vector/at job parent-dir child-dir v index (fx- pos 4)))))


;; copy a single job redirection to vector v, at v[pos] ... v[pos+3]
;;
;; note: must prefix any relative path with job's working directory,
;; because job may be a parent job with a different working directory
(define (job-fill-c-redirect-vector/at job parent-dir child-dir v index pos)
  (let* ((dir           (%parent-dir-if-different parent-dir child-dir))
         (redirects     (job-redirects job))
         (fd            (span-ref redirects index))
         (direction-ch  (span-ref redirects (fx1+ index)))
         ;; fd may need to be redirected to a different file descriptor due to fd remapping
         (remapped-fd   (job-find-fd-remap job fd))
         (to            (if (fx=? fd remapped-fd)
                          ; no remapping found, extract redirection.
                          (job-extract-redirection-to-fd-or-bytevector0 job dir redirects index)
                          ; remapping found, use it
                          remapped-fd)))

    ; (debugf "job-fill-c-redirect-vector job=~s redirect fd=~s -> fd=~s, remapped-fd = ~s" job fd to remapped-fd)
    (vector-set! v pos fd)
    (vector-set! v (fx1+  pos) direction-ch)
    ;; to-fd must be placed at pos + 2
    (vector-set! v (fx+ 2 pos) (if (fixnum? to) to #f))
    ;; to-bytevector0 must be placed at pos + 3
    (vector-set! v (fx+ 3 pos) (if (fixnum? to) #f to))))


;; return parent-dir if different from child-dir, otherwise return #f
;; Note: parent-dir and child-dir may be #f
(define (%parent-dir-if-different parent-dir child-dir)
  (cond
    ((and parent-dir child-dir)
      (if (charspan=? parent-dir child-dir) #f parent-dir))
    (child-dir
      (sh-cwd))
    (else
      parent-dir)))
