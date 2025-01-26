;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only from file shell/job.ss


;; if truish, new process groups will be created as needed and sub-processes may be moved into them.
;; if #f, sub-processes will inherit the parent's process group.
;;
;; Needed by job control, usually set to #t in the main shell and to #f in subshells
(define sh-can-create-pgid? (make-thread-parameter #t))


;; if truish, allow changing the foreground process group
;; if #f, the foreground process group will never be changed by this process.
;;
;; Needed by job control, usually set to #t in the main shell and to #f in subshells
(define sh-can-set-fg-pgid? (make-thread-parameter #t))


;; Create a cmd to later spawn it. Each argument must be a string.
;; If you want to use procedures as args, see (sh-cmd*)
(define (sh-cmd . program-and-args)
  (assert-string-list? 'sh-cmd program-and-args)
  (make-cmd program-and-args))


(define (make-cmd program-and-args)
  (%make-cmd
    #f #f #f '(new . 0) ; id pid pgid last-status
    (span) 0 #f '() ; redirections
    cmd-start #f    ; start-proc step-proc
    #f              ; working directory - initially inherited by parent job
    #f              ; overridden environment variables - initially none
    #f              ; env var assignments - initially none
    (sh-globals)    ; parent job - initially the global job
    program-and-args
    #f))            ; expanded arg-list



;; NOTE: this is an internal implementation function, use (sh-start) instead.
;; This function does not update job's status, and does not register job
;; into global (pid->job) table nor into global job-id table.
;;
;; Description:
;; Start a cmd i.e. fork() and exec() an external process, optionally inserting it into
;; an existing process group.
;;
;; The new process is started in background, i.e. the foreground process group is NOT set
;; to the process group of the newly created process.
;;
;; Options is a list of zero or more of the following:
;;   process-group-id: an integer, if present and > 0 the new process will be inserted
;;   into the corresponding process group id - which must already exist.
(define (cmd-start c options)
  (assert* 'sh-cmd (sh-cmd? c))
  (assert* 'sh-cmd (eq? 'running (job-last-status->kind c)))

  ; expand procedures in cmd-arg-list, then expand aliases.
  ; sanity: (sh-alias-expand) ignores aliases for "builtin"
  (let* ((prog-and-args (sh-alias-expand (cmd-arg-list-expand c)))
         (builtin       (sh-find-builtin prog-and-args)))
    ; (debugf "cmd-start expanded-prog-and-args=~s builtin=~s" prog-and-args builtin)
    (unless (eq? prog-and-args (cmd-arg-list c))
      ; save expanded cmd-arg-list for more accurate pretty-printing
      (cmd-expanded-arg-list-set! c prog-and-args))
    ; apply lazy environment variables *after* expanding cmd-arg-list
    (job-env/apply-lazy! c)
    (if builtin
      ; expanded arg[0] is a builtin, call it.
      (cmd-start/builtin builtin c prog-and-args options)
       ; expanded arg[0] is a not builtin or alias, spawn a subprocess
      (cmd-spawn c (list->argv prog-and-args) options))))


;; internal function called by (cmd-start):
;; expand procedures in cmd-arg-list.
;; Return the expanded command line, which is always a list of strings.
(define (cmd-arg-list-expand c)
  (let ((prog-and-args (cmd-arg-list c)))
    (if (string-list? prog-and-args)
      prog-and-args
      (let ((l '()))
        (list-iterate prog-and-args
          (lambda (arg)
            (set! l (cmd-arg-expand c arg l))))
        (set! l (reverse! l))
        (assert-string-list? 'sh-start l)
        l))))


;; internal function called by (cmd-start):
;; if arg is a procedure then call it, optionally passing current job as the only argument.
;; Such procedure must return a string or list-of-strings, which are reverse-consed
;; at the beginning of list-of-strings l.
;; Return the updated list.
(define (cmd-arg-expand c arg l)
  (let ((expanded
          (cond
            ((not (procedure? arg)) arg)
            ((logbit? 1 (procedure-arity-mask arg)) (arg c)) ; call closure (lambda (job) ...)
            (#t   (arg)))))                                  ; call closure (lambda () ...)
    ; (debugf "cmd-arg-expand cmd=~s arg=~s expanded=~s l=~s" c arg expanded l)
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


;; internal function called by (cmd-start) to execute a builtin
(define (cmd-start/builtin builtin c prog-and-args options)
  (job-remap-fds! c)
  (job-status-set! 'cmd-start/builtin c
    (parameterize ((sh-fd-stdin  (job-find-fd-remap c 0))
                   (sh-fd-stdout (job-find-fd-remap c 1))
                   (sh-fd-stderr (job-find-fd-remap c 2)))
      (builtin/start builtin c prog-and-args options))))


;; internal function called by (cmd-start) to spawn a subprocess
(define cmd-spawn
  (let ((c-cmd-spawn (foreign-procedure "c_cmd_spawn" (ptr ptr ptr ptr int) int)))
    (lambda (c argv options)
      (let* ((process-group-id (job-start-options->process-group-id options))
             (job-dir (job-cwd-if-set c))
             (ret (c-cmd-spawn
                    argv
                    (if job-dir (text->bytevector0 job-dir) #f)
                    (job-make-c-redirect-vector c)
                    (sh-env->argv c 'exported)
                    (or process-group-id -1))))
        (when (< ret 0)
          (raise-c-errno 'sh-start 'fork ret))
        (job-pid-set! c ret)
        (job-pgid-set! c process-group-id)))))


;; internal function called by (sh-builtin-exec) to exec a subprocess
(define cmd-exec
  (let ((c-cmd-exec (foreign-procedure "c_cmd_exec" (ptr ptr ptr ptr) int)))
    (lambda (c argv options)
      (let* ((job-dir (job-cwd-if-set c))
             (ret (c-cmd-exec
                    argv
                    (if job-dir (text->bytevector0 job-dir) #f)
                    (job-make-c-redirect-vector c)
                    (sh-env->argv c 'exported))))
        ; (c-cmd-exec) returns only if it failed
        (job-status-set! 'cmd-exec c (cons 'exited (if (integer? ret) ret -1)))))))


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
    (#t
      parent-dir)))


;; Convert pid-wait-result to a symbolic job-status:
;;
;; If pid-wait-result is '() i.e. process status did not change,
;; return '(running . #f) indicating process is still running.
;;
;; If pid-wait-result is a pair (pid . exit-status) where exit-status is:
;;   not a fixnum, or < 0 => return (cons 'unknown exit-status)
;;   0                    => return (void)
;;   1..255               => return (cons 'exited  exit-status)
;;   256 + kill_signal    => return (cons 'killed  signal-name)
;;   512 + stop_signal    => return (cons 'stopped signal-name)
;;   >= 768               => return (cons 'unknown (fx- exit-status 768))
;;
;; Otherwise return (cons 'unknown pid-wait-result)
(define (pid-wait->job-status pid-wait-result)
  (cond
    ((pair? pid-wait-result)
      (let ((num (cdr pid-wait-result)))
        (cond ((or (not (fixnum? num)) (fx<? num 0)) (cons 'unknown num))
              ((fx=? num   0) (void))
              ((fx<? num 256) (cons 'exited  num))
              ((fx<? num 512) (cons 'killed  (signal-number->name (fxand num 255))))
              ((fx<? num 768) (cons 'stopped (signal-number->name (fxand num 255))))
              (#t             (cons 'unknown (fx- num 768))))))
    ((null? pid-wait-result)
      '(running . #f))
    (#t
      (cons 'unknown pid-wait-result))))


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
      (let ((_caller caller)
            (_expected-pgid expected-pgid)
            (_new-pgid new-pgid)
            (_can-set-fd-pgid? (sh-can-set-fg-pgid?)))
        (dynamic-wind
          (lambda () ; run before body
            (when _can-set-fd-pgid?
              (%pgid-foreground _caller _expected-pgid _new-pgid)))
          (lambda ()
            body ...)
          (lambda () ; run after body
            (when _can-set-fd-pgid?
              ; try to restore (sh-globals) as the foreground process group
              (%pgid-foreground _caller _new-pgid _expected-pgid))))))))


;; Internal function called by (job-advance)
(define (job-advance/pid mode job)
  ; (debugf "> job-advance/pid mode=~s job=~a pid=~s status=~s" mode (sh-job-display/string job) (job-pid job) (job-last-status job))
  (cond
    ((job-finished? job)
      (job-last-status job)) ; job exited, and exit status already available
    ((not (job-started? job))
      (raise-errorf mode "job not started yet: ~s" job))
    (#t
      (let ((pid  (job-pid job))
            (pgid (job-pgid job)))
        (if (and pgid (memq mode '(sh-fg sh-wait sh-sigcont+wait sh-subshell)))
          (with-foreground-pgid mode (job-pgid (sh-globals)) pgid
            (job-advance/pid/maybe-sigcont mode job pid pgid)
            (job-advance/pid/wait mode job pid pgid))
          (begin
            (job-advance/pid/maybe-sigcont mode job pid pgid)
            (job-advance/pid/wait mode job pid pgid)))))))


;; Internal function called by (job-advance/pid)
(define (job-advance/pid/maybe-sigcont mode job pid pgid)
  (assert* mode (> pid 0))
  (when pgid
    (assert* mode (> pgid 0)))
  (when (memq mode '(sh-fg sh-bg sh-sigcont+wait))
    ; send SIGCONT to job's process group, if present.
    ; otherwise send SIGCONT to job's process id. Both may raise error
    ; (debugf "job-advance/pid/sigcont > ~s ~s" mode job)
    (pid-kill (if (and pgid (> pgid 0)) (- pgid) pid) 'sigcont)))



;; Internal function called by (job-advance/pid)
(define (job-advance/pid/wait mode job pid pgid)
  ; no need to wait for ALL processes in job's process group:
  ; the only case where we spawn multiple processes in the same process group
  ; is a pipe i.e {a | b | c ...} and in such case we separately wait on the process id
  ; of each spawned process
  (let* ((pid-wait-ret (pid-wait (job-pid job)
                         (if (memq mode '(sh-bg sh-job-status)) 'nonblocking 'blocking)))
         (wait-status (pid-wait->job-status pid-wait-ret))
         (kind        (job-status->kind wait-status)))
    ; (debugf ">   job-advance/pid/wait mode=~s job=~a pid=~s wait-status=~s" mode (sh-job-display/string job) (job-pid job) wait-status)
    ; if may-block is 'non-blocking, wait-status may be '(running . #f)
    ; indicating job status did not change i.e. it's (expected to be) still running
    (case kind
      ((running)
        ; if wait-status is '(running . #f), try to return '(running . job-id)
        (job-status-set! 'job-advance/pid/wait job wait-status))
      ((exited killed unknown)
        ; job exited, clean it up. Also allows user to later start it again.
        (pid->job-delete! (job-pid job))
        (job-status-set! 'job-advance/pid/wait job wait-status)
        (job-id-unset! job) ; may show job summary
        (job-pid-set!  job #f)
        (job-pgid-set! job #f)
        wait-status)
      ((stopped)
        ; process is stopped.
        ; if mode is sh-wait or sh-sigcont+wait, call (break)
        ; then, if mode is sh-wait sh-sigcont+wait or sh-subshell,
        ;;  wait for it again (which blocks until it changes status again)
        ; otherwise propagate process status and return.
        (if (memq mode '(sh-wait sh-sigcont+wait sh-subshell))
          (begin
            (job-advance/pid/break mode job pid pgid)
            (job-advance/pid/wait mode job pid pgid))
          (begin
            (job-status-set! 'job-advance/pid/wait job wait-status)
            wait-status)))
      (else
        (raise-errorf mode "job not started yet: ~s" job)))))


;; Internal function called by (job-advance/pid/wait)
;; when job is stopped in mode 'sh-wait or 'sh-sigcont+wait:
;; call (break) then send 'sigcont to job
;; if (break) raises an exception or resets scheme, then send 'sigint to job
(define (job-advance/pid/break mode job pid pgid)
   ; subshells should not directly perform I/O,
   ; they cannot write the "break> " prompt then read commands
   (when (sh-can-set-fg-pgid?)
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
