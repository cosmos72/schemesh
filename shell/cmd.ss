;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only from file shell/job.ss



;; Create a cmd to later spawn it. Each argument must be a string.
;; If you want to use procedures as args, see (sh-cmd*)
(define (sh-cmd . program-and-args)
  (assert-string-list? 'sh-cmd program-and-args)
  (make-cmd program-and-args))


(define (make-cmd program-and-args)
  (%make-cmd
    #f #f #f        ; id pid pgid
    '(new . 0) #f   ; last-status exception
    (span) 0 #f '() ; redirections
    start-cmd #f    ; start-proc step-proc
    #f              ; working directory - initially inherited by parent job
    #f              ; overridden environment variables - initially none
    #f              ; env var assignments - initially none
    #f (sh-globals) ; no temp parent. default parent job is initially the global job
    program-and-args
    #f))            ; expanded arg-list



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
;; Updates job status, and also returns it,
;;   which may be one of (void) '(exited ...) '(running ...) etc.
;;   For the complete list of possible returned job statuses, see (sh-job-status).
(define (start-cmd c options)
  (assert* 'sh-cmd (eq? 'running (job-last-status->kind c)))
  (job-status-set! 'start-cmd c
    (start-command-or-builtin-or-alias c (cmd-arg-list-apply c) options)))



;; internal function called by (start-cmd):
;; call procedures in cmd-arg-list.
;; Return the expanded command line, which is always a list of strings.
(define (cmd-arg-list-apply c)
  (let ((prog-and-args (cmd-arg-list c)))
    (if (string-list? prog-and-args)
      prog-and-args
      (let ((l '()))
        (list-iterate prog-and-args
          (lambda (arg)
            (set! l (cmd-arg-apply c arg l))))
        (set! l (reverse! l))
        (assert-string-list? 'sh-start l)
        l))))


;; internal function called by (start-cmd):
;; if arg is a procedure then call it, optionally passing current job as the only argument.
;; Such procedure must return a string or list-of-strings, which are reverse-consed
;; at the beginning of list-of-strings l.
;; Return the updated list.
(define (cmd-arg-apply c arg l)
  (let ((expanded
          (cond
            ((not (procedure? arg)) arg)
            ((logbit? 1 (procedure-arity-mask arg)) (arg c)) ; call closure (lambda (job) ...)
            (#t   (arg)))))                                  ; call closure (lambda () ...)
    ; (debugf "cmd-arg-apply cmd=~s arg=~s expanded=~s l=~s" c arg expanded l)
    (cond
      ((eq? (void) expanded)
        l)
      ((null? expanded)
        l)
      ((pair? expanded)
        (list-iterate expanded
          (lambda (e)
            (assert* 'sh-start (string? e))
            (set! l (cons e l))))
        l)
      ((string? expanded)
        (cons expanded l))
      (#t
        (raise-errorf 'sh-start "value ~s returned by closure ~s in job ~s is not a string, a list of strings, or (void)"
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
;;   which may be one of (void) '(exited ...) '(running ...) etc.
;;   For the complete list of possible returned job statuses, see (sh-job-status).
(define (start-command-or-builtin-or-alias c program-and-args options)
  (assert* 'sh-cmd (sh-cmd? c))

       ;; set parent job if requested. currently redundant,
       ;; as we are only called by (start-any/throw) that already does the same.
  (let ((options (options->set-temp-parent! c options))
        ;; expand aliases in args
        ;; sanity: (sh-aliases-expand) ignores aliases for "builtin"
        (prog-and-args (sh-aliases-expand program-and-args)))

    ;; (debugf "start-cmd expanded-prog-and-args=~s builtin=~s" prog-and-args builtin)
    (unless (eq? prog-and-args (cmd-arg-list c))
      ;; save expanded cmd-arg-list for more accurate pretty-printing
      (cmd-expanded-arg-list-set! c prog-and-args))

    (if (null? prog-and-args)
      (begin
        ;; apply lazy environment variables *after* expanding cmd-arg-list
        (job-env/apply-lazy! c 'maintain)
        (job-env-copy-into-parent! c)
        (void)) ; return job status = success
      (let ((builtin (sh-find-builtin prog-and-args)))
        ;; apply lazy environment variables *after* expanding cmd-arg-list
        ;; and *after* setting job's parent
        (job-env/apply-lazy! c 'export)
        (if builtin
          ; expanded arg[0] is a builtin, call it.
          (start-builtin builtin c prog-and-args options)  ; returns job status
          ; expanded arg[0] is a not builtin or alias, spawn a subprocess
          (spawn-cmd c (list->argv prog-and-args) options)))))) ; returns job status


;; returns job status
(define (start-command-or-builtin-or-alias-from-another-builtin c program-and-args options)
  (assert* 'sh-cmd (sh-cmd? c))

  ;; expand aliases in args
  ;; sanity: (sh-aliases-expand) ignores aliases for "builtin"
  (let ((prog-and-args (sh-aliases-expand program-and-args)))
    ;; (debugf "start-cmd expanded-prog-and-args=~s builtin=~s" prog-and-args builtin)
    (unless (eq? prog-and-args (cmd-arg-list c))
      ;; save expanded cmd-arg-list for more accurate pretty-printing
      (cmd-expanded-arg-list-set! c prog-and-args))
    ; lazy environment was applied already by the outer (start-command-or-builtin-or-alias)
    (if (null? prog-and-args)
      (void) ; return success
      (let ((builtin (sh-find-builtin prog-and-args)))
        (if builtin
          ; expanded arg[0] is a builtin, call it.
          ; redirections were applied already by the outer (start-command-or-builtin-or-alias)
          (start-builtin-already-redirected builtin c prog-and-args options) ; returns job status
          ; expanded arg[0] is a not builtin or alias, spawn a subprocess
          (spawn-cmd c (list->argv prog-and-args) options)))))) ; returns job status


;; internal function called by (start-cmd) to spawn a subprocess.
;; returns job status.
(define spawn-cmd
  (let ((c-spawn-cmd (foreign-procedure "c_cmd_spawn" (ptr ptr ptr ptr int) int)))
    (lambda (c argv options)
      (let* ((process-group-id (options->process-group-id options))
             (_                (options->set-temp-parent! c options))
             (job-dir (job-cwd-if-set c))
             (ret (c-spawn-cmd
                    argv
                    (if job-dir (text->bytevector0 job-dir) #f)
                    (job-make-c-redirect-vector c)
                    (sh-env->argv c 'export)
                    (or process-group-id -1))))
        (when (< ret 0)
          (job-status-set! c 'spawn-cmd (cons 'exited ret))
          (raise-c-errno 'sh-start 'fork ret))
        (job-pid-set! c ret)
        (job-pgid-set! c process-group-id)
        (job-last-status c)))))


;; internal function called by (builtin-exec) to exec a subprocess.
;; if C exec() fails, returns job status.
(define exec-cmd
  (let ((c-exec-cmd (foreign-procedure "c_cmd_exec" (ptr ptr ptr ptr) int)))
    (lambda (c argv options)
      (let* ((_       (options->set-temp-parent! c options))
             (job-dir (job-cwd-if-set c))
             (ret (c-exec-cmd
                    argv
                    (if job-dir (text->bytevector0 job-dir) #f)
                    (job-make-c-redirect-vector c)
                    (sh-env->argv c 'export))))
        ; (c-exec-cmd) returns only if it failed
        (cons 'exited (if (integer? ret) ret -1))))))


;; internal function called by (spawn-cmd)
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
    (#t
      parent-dir)))


;; Convert value returned by (pid-wait) to a symbolic job status.
;;
;; If (pid-wait) succeeds, it returns a pair (pid . wait-status).
;; Passing wait-status to this function converts it to a job status as follows:
;;   not a fixnum, or < 0 => return (cons 'unknown wait-status)
;;   0                    => return (void)
;;   1..255               => return (cons 'exited  wait-status)
;;   256 + kill_signal    => return (cons 'killed  signal-name)
;;   512 + stop_signal    => return (cons 'stopped signal-name)
;;   768                  => return (cons 'running #f)
;;   > 768                => return (cons 'unknown (fx- wait-status 768))
;;
(define (pid-wait-result->job-status wait-status)
  (let ((x wait-status))
    (cond ((or (not (fixnum? x)) (fx<? x 0)) (cons 'unknown x))
          ((fx=? x   0) (void))
          ((fx<? x 256) (cons 'exited  x))
          ((fx<? x 512) (cons 'killed  (signal-number->name (fxand x 255))))
          ((fx<? x 768) (cons 'stopped (signal-number->name (fxand x 255))))
          ((fx=? x 768) '(running . #f))
          (#t           (cons 'unknown (fx- x 768))))))


(define %pgid-foreground
  (let ((c-pgid-foreground (foreign-procedure "c_pgid_foreground" (int int) int)))
    (lambda (caller expected-pgid new-pgid)
      (let ((err (c-pgid-foreground expected-pgid new-pgid)))
        ; (c-pgid-foreground) may fail if new-pgid exited in the meantime
        ; (when (< err 0)
        ;  (raise-c-errno caller 'tcsetpgrp err new-pgid))
        err))))


(define-syntax with-foreground-pgid
  (syntax-rules ()
    ((_  caller expected-pgid new-pgid body ...)
      (let ((_caller        caller)
            (_expected-pgid expected-pgid)
            (_new-pgid      new-pgid)
            (_job-control?  (sh-job-control?)))
        (dynamic-wind
          (lambda () ; run before body
            (when _job-control?
              (%pgid-foreground _caller _expected-pgid _new-pgid)))
          (lambda ()
            body ...)
          (lambda () ; run after body
            (when _job-control?
              ; try to restore (sh-globals) as the foreground process group
              (%pgid-foreground _caller _new-pgid _expected-pgid))))))))


;; Internal function called by (advance-job)
(define (advance-pid mode job)
  ; (debugf "> advance-pid mode=~s job=~a pid=~s status=~s" mode (sh-job-display/string job) (job-pid job) (job-last-status job))
  (cond
    ((job-finished? job)
      (job-last-status job)) ; job exited, and exit status already available
    ((not (job-started? job))
      (raise-errorf mode "job not started yet: ~s" job))
    (#t
      (let ((pid  (job-pid job))
            (pgid (job-pgid job)))
        (if (and pgid (memq mode '(sh-fg sh-wait sh-sigcont+wait)))
          (with-foreground-pgid mode (job-pgid (sh-globals)) pgid
            (advance-pid/maybe-sigcont mode job pid pgid)
            (advance-pid/wait mode job pid pgid))
          (begin
            (advance-pid/maybe-sigcont mode job pid pgid)
            (advance-pid/wait mode job pid pgid)))))))


;; Internal function called by (advance-pid)
(define (advance-pid/maybe-sigcont mode job pid pgid)
  (assert* mode (> pid 0))
  (when pgid
    (assert* mode (> pgid 0)))
  (when (memq mode '(sh-fg sh-bg sh-sigcont+wait))
    ; send SIGCONT to job's process group, if present.
    ; otherwise send SIGCONT to job's process id. Both may raise error
    ; (debugf "advance-pid/sigcont > ~s ~s" mode job)
    (pid-kill (if (and pgid (> pgid 0)) (- pgid) pid) 'sigcont)
    ;; assume job is now running
    (job-status-set/running! job)))



;; Internal function called by (advance-pid)
(define (advance-pid/wait mode job pid pgid)
  ;; cannot call (sh-job-status), it would recurse back here.
  (let* ((old-status (job-last-status job))
         (new-status (if (job-status-finished? old-status)
                       old-status
                       (job-pids-wait job
                         (if (memq mode '(sh-bg sh-job-status)) 'nonblocking 'blocking)))))
    ; (debugf ">   advance-pid/wait mode=~s job=~a pid=~s new-status=~s" mode (sh-job-display/string job) (job-pid job) new-status)
    ; if may-block is 'non-blocking, new-status may be '(running . #f)
    ; indicating job status did not change i.e. it's (expected to be) still running
    (case (job-status->kind new-status)
      ((running)
        ; if new-status is '(running . #f), try to return '(running . job-id)
        (job-status-set/running! job))
      ((exited killed unknown)
        ; job exited, clean it up. Also allows user to later start it again.
        (pid->job-delete! (job-pid job))
        (job-status-set! 'advance-pid/wait job new-status)
        (job-id-unset! job) ; may show job summary
        (job-pid-set!  job #f)
        (job-pgid-set! job #f)
        new-status)
      ((stopped)
        ; process is stopped.
        ; if mode is sh-wait or sh-sigcont+wait
        ;   call (break) then wait for it again (which blocks until it changes status again)
        ; otherwise propagate process status and return.
        (if (memq mode '(sh-wait sh-sigcont+wait))
          (begin
            (advance-pid/break mode job pid pgid)
            (advance-pid/wait mode job pid pgid))
          (begin
            (job-status-set! 'advance-pid/wait job new-status)
            new-status)))
      (else
        (raise-errorf mode "job not started yet: ~s" job)))))


;; Internal function called by (advance-pid/wait):
;;
;; If may-block is 'nonblocking, call C function wait4(WNOHANG) in a loop,
;; as long as it tells us that *some* subprocess changed status,
;; and update the corresponding job status.
;; Return when wait4(WNOHANG) does not report any subprocess status change.
;;
;; Otherwise, if may-block is 'blocking, call C function wait4() in a loop,
;; waiting for *some* subprocess to change status,
;; and update the corresponding job status.
;; Return when the preferred-pid happens to change status.
;;
;; In all cases, if preferred-job is set, return its updated status.
(define (job-pids-wait preferred-job may-block)
  ;c (debugf ">   job-pids-wait may-block=~s preferred-job=~a" may-block (if preferred-job (sh-job-display/string preferred-job) preferred-job))
  (let ((done? #f))
    (until done?
      (let ((wait-result (pid-wait -1 may-block)))
        (if (pair? wait-result)
          (let ((job (pid->job (car wait-result))))
            (when job
              (let* ((old-status (job-last-status job))
                     (new-status (pid-wait-result->job-status (cdr wait-result))))
                (job-status-set! 'job-pids-wait job new-status)
                ; (debugf "... job-pids-wait old-status=~s new-status=~s job=~a" old-status new-status (sh-job-display/string job))
                (when (job-status-stopped-or-resumed? old-status new-status)
                  (sh-job-display/summary job)))

              ; (debugf "... job-pids-wait new-status=~s job=~a" (job-last-status job) (sh-job-display/string job))

              (if (eq? job preferred-job)
                ;; the job we are interested in changed status => don't block again
                (when (eq? may-block 'blocking)
                  (set! done? #t))

                ;; advance job that changed status and *all* it parents, before waiting again.
                ;; do NOT advance preferred-job, because that's what our callers are already doing.
                (let ((globals (sh-globals)))
                  (job-default-parents-iterate (job-parent job)
                    (lambda (parent)
                      (if (eq? parent globals)
                        #f
                        (let* ((old-status (job-last-status parent))
                               (new-status (sh-job-status parent)))
                          ; (debugf "... job-pids-wait old-status=~s new-status=~s parent=~a" old-status new-status (sh-job-display/string parent))
                          (when (job-status-stopped-or-resumed? old-status new-status)
                            (sh-job-display/summary parent))
                          new-status))))))))
          (set! done? #t))))) ; (pid-wait) did not report any status change => return

  (let ((ret (if preferred-job (job-last-status preferred-job) (void))))
    ;c (debugf "<   job-pids-wait ret=~s" ret)
    ret))



;; Internal function called by (advance-pid/wait)
;; when job is stopped in mode 'sh-wait or 'sh-sigcont+wait:
;; call (break) then send 'sigcont to job
;; if (break) raises an exception or resets scheme, then send 'sigint to job
(define (advance-pid/break mode job pid pgid)
   ; subshells should not directly perform I/O,
   ; they cannot write the "break> " prompt then read commands
   (when (sh-job-control?)
     (let ((break-returned-normally? #f)
          (global-pgid (job-pgid (sh-globals))))
      (dynamic-wind
        (lambda () ; before body
          (%pgid-foreground mode pgid global-pgid))
        (lambda () ; body
          (job-id-set! job)
          (break)
          (set! break-returned-normally? #t))
        (lambda ()
          (%pgid-foreground mode global-pgid pgid)
          ; send SIGCONT to job's process group, if present.
          ; otherwise send SIGCONT to job's process id. Both may raise error
          (pid-kill (if (> pgid 0) (- pgid) pid) 'sigcont)
          (unless break-returned-normally?
            (pid-kill (if (> pgid 0) (- pgid) pid) 'sigint)))))))
