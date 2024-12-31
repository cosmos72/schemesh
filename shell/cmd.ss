;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should only be included inside a (library ...) definition



;; Create a cmd to later spawn it. Each argument must be a string.
;; If you want to use procedures as args, see (sh-cmd*)
(define (sh-cmd . program-and-args)
  (assert-string-list? 'sh-cmd program-and-args)
  (make-cmd program-and-args))


(define (make-cmd program-and-args)
  (%make-cmd #f -1 -1 '(new . 0)
    (span) #f '() ; redirections
    cmd-start #f  ; start-proc step-proc
    (sh-cwd)      ; job working directory - initially current directory
    #f            ; overridden environment variables - initially none
    #f            ; env var assignments - initially none
    sh-globals    ; parent job - initially the global job
    program-and-args))



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
;;   process-group-id: a fixnum, if present and > 0 the new process will be inserted
;;   into the corresponding process group id - which must already exist.
(define (cmd-start c options)
  (assert* 'sh-cmd (sh-cmd? c))
  (assert* 'sh-cmd (eq? 'running (job-last-status->kind c)))

  ; expand procedures in cmd-arg-list,
  ; then expand aliases. sanity: (sh-alias-expand) ignores aliases for "builtin"
  (let* ((prog-and-args (sh-alias-expand (cmd-arg-list-expand c)))
         (builtin       (sh-find-builtin prog-and-args)))
    ; apply lazy environment variables *after* expanding cmd-arg-list
    (job-env/apply-lazy! c)
    (if builtin
      ; expanded arg[0] is a builtin, call it.
      (cmd-run/builtin builtin c prog-and-args options)
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
;; expand a single procedure in a cmd-arg-list element,
;; and reverse-cons it at the beginning of list-of-strings l.
;; Return the updated list.
(define (cmd-arg-expand c arg l)
  (let ((expanded
          (cond
            ((not (procedure? arg)) arg)
            ((logbit? 1 (procedure-arity-mask arg)) (arg c))
            (#t   (arg)))))
    (cond
      ((null? expanded)
        l)
      ((pair? expanded)
        (list-iterate expanded
          (lambda (e)
            (set! l (cons e l))))
        l)
      (#t
        (cons expanded l)))))


;; internal function called by (cmd-start) to execute a builtin
(define (cmd-run/builtin builtin c prog-and-args options)
  (job-remap-fds! c)
  (job-status-set! c
    (parameterize ((sh-fd-stdin  (job-find-fd-remap c 0))
                   (sh-fd-stdout (job-find-fd-remap c 1))
                   (sh-fd-stderr (job-find-fd-remap c 2)))
      (builtin c prog-and-args options))))


;; internal function called by (cmd-start) to spawn a subprocess
(define cmd-spawn
  (let ((c-spawn-pid (foreign-procedure "c_spawn_pid"
                        (ptr ptr ptr int) int)))
    (lambda (c argv options)
      (let* ((process-group-id (job-start-options->process-group-id options))
             (ret (c-spawn-pid
                    argv
                    (job-prepare-c-redirect-vector c)
                    (sh-env->argv c 'exported)
                    process-group-id)))
        (when (< ret 0)
          (raise-c-errno 'sh-start 'fork ret))
        (job-pid-set! c ret)
        (job-pgid-set! c (if (> process-group-id 0) process-group-id ret))))))


;; internal function called by (cmd-spawn)
;; creates and fills a vector with job's redirections and its parents redirections
(define (job-prepare-c-redirect-vector job)
  (let* ((n (job-count-c-redirect-vector job 0))
         (v (make-vector n)))
    (do ((parent job (job-parent parent)))
        ((not parent) v)
      (set! n (job-fill-c-redirect-vector-norecurse parent v n)))))


;; count and return the total number of redirections (* 4) of a job,
;; including its parents redirections
(define (job-count-c-redirect-vector job n)
  (if job
    ; add job's redirect count to n, and recurse to parent
    (job-count-c-redirect-vector (job-parent job) (fx+ n (span-length (job-redirects job))))
    n))


;; copy job's redirections to vector v, without recursing to job's parents.
;; returns (fx- pos (number-of-copied-elements))
(define (job-fill-c-redirect-vector-norecurse job v end-pos)
   (let* ((n         (span-length (job-redirects job)))
          (start-pos (fx- end-pos n)))
     (do ((index (fx- n 4) (fx- index 4)))
         ((fx<? index 0) start-pos)
       (job-fill-c-redirect-vector-at job v index start-pos))))


;; copy a single job redirection to vector v
(define (job-fill-c-redirect-vector-at job v index start-pos)
  (let* ((redirects     (job-redirects job))
         (fd            (span-ref redirects index))
         (direction-ch  (span-ref redirects (fx1+ index)))
         ;; redirection to file may already be opened on a different file descriptor
         ;; due to fd remapping
         (remapped-to   (job-find-fd-remap job fd))
         (to            (if (fx=? fd remapped-to)
                          (job-extract-redirection-to-fd-or-bytevector0 job redirects index)
                          remapped-to)))
    (vector-set! v start-pos fd)
    (vector-set! v (fx1+  start-pos) direction-ch)
    ;; to-fd must be placed at start-pos + 2
    (vector-set! v (fx+ 2 start-pos) (if (fixnum? to) to #f))
    ;; to-bytevector0 must be placed at start-pos + 3
    (vector-set! v (fx+ 3 start-pos) (if (fixnum? to) #f to))))



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
        (when (< err 0)
          (raise-c-errno caller 'tcsetpgrp err))
        err))))


(define-syntax with-foreground-pgid
  (syntax-rules ()
    ((_  caller expected-pgid new-pgid body ...)
      (let ((_caller caller)
            (_expected-pgid expected-pgid)
            (_new-pgid new-pgid))
        (dynamic-wind
          (lambda () ; run before body
            (%pgid-foreground _caller _expected-pgid _new-pgid))
          (lambda ()
            body ...)
          (lambda () ; run after body
            ; try to restore sh-globals as the foreground process group
            (%pgid-foreground _caller _new-pgid _expected-pgid)))))))


;; Internal function called by (job-advance)
(define (job-advance/pid mode job)
  ; (debugf "job-advance/pid > ~s ~s status=~s~%" mode job (job-last-status job))
  (cond
    ((job-finished? job)
      (job-last-status job)) ; job exited, and exit status already available
    ((not (job-started? job))
      (raise-errorf mode "job not started yet: ~s" job))
    (#t
      (let ((pid  (job-pid job))
            (pgid (job-pgid job)))
        (if (memq mode '(sh-fg sh-wait sh-sigcont+wait sh-subshell))
          (with-foreground-pgid mode (job-pgid sh-globals) pgid
            (job-advance/pid/maybe-sigcont mode job pid pgid)
            (job-advance/pid/wait mode job pid pgid))
          (begin
            (job-advance/pid/maybe-sigcont mode job pid pgid)
            (job-advance/pid/wait mode job pid pgid)))))))


;; Internal function called by (job-advance/pid)
(define (job-advance/pid/maybe-sigcont mode job pid pgid)
  (assert* mode (fx>? pid 0))
  (assert* mode (fx>? pgid 0))
  (when (memq mode '(sh-fg sh-bg sh-sigcont+wait))
    ; send SIGCONT to job's process group, if present.
    ; otherwise send SIGCONT to job's process id. Both may raise error
    ; (debugf "job-advance/pid/sigcont > ~s ~s~%" mode job)
    (pid-kill (if (fx>? pgid 0) (fx- pgid) pid) 'sigcont)))



;; Internal function called by (job-advance/pid)
(define (job-advance/pid/wait mode job pid pgid)
  ; no need to wait for ALL processes in job's process group:
  ; the only case where we spawn multiple processes in the same process group
  ; is a pipe i.e {a | b | c ...} and in such case we separately wait on the process id
  ; of each spawned process
  (let* ((may-block   (if (memq mode '(sh-bg sh-job-status)) 'nonblocking 'blocking))
         (wait-status (pid-wait->job-status (pid-wait (job-pid job) may-block)))
         (kind        (job-status->kind wait-status)))
    ; (debugf "job-advance/pid/wait > ~s ~s wait-status=~s~%" mode job wait-status)
    ; if may-block is 'non-blocking, wait-status may be '(running . #f)
    ; indicating job status did not change i.e. it's (expected to be) still running
    (case kind
      ((running)
        ; if wait-status is '(running . #f), try to return '(running . job-id)
        (job-status-set! job wait-status))
      ((exited killed unknown)
        ; job exited, clean it up. Also allows user to later start it again.
        (pid->job-delete! (job-pid job))
        (job-status-set! job wait-status)
        (job-id-unset! job) ; may show job summary
        (job-pid-set!  job -1)
        (job-pgid-set! job -1)
        wait-status)
      ((stopped)
        ; process is stopped.
        ; if mode is sh-wait or sh-sigcont+wait, call (break)
        ; then, if mode is sh-wait sh-sigcont+wait or sh-subshell, wait for it again (which blocks until it changes status again)
        ; otherwise propagate process status and return.
        (if (memq mode '(sh-wait sh-sigcont+wait sh-subshell))
          (begin
            (when (memq mode '(sh-wait sh-sigcont+wait))
              (job-advance/pid/break mode job pid pgid))
            (job-advance/pid/wait mode job pid pgid))
          (begin
            (job-status-set! job wait-status)
            wait-status)))
      (else
        (raise-errorf mode "job not started yet: ~s" job)))))


;; Internal function called by (job-advance/pid/wait)
;; when job is stopped in mode 'sh-wait or 'sh-sigcont+wait:
;; call (break) then send 'sigcont to job
;; if (break) raises an exception or resets scheme, then send 'sigint to job
(define (job-advance/pid/break mode job pid pgid)
  (let ((break-returned-normally? #f)
        (global-pgid (job-pgid sh-globals)))
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
        (pid-kill (if (fx>? pgid 0) (fx- pgid) pid) 'sigcont)
        (unless break-returned-normally?
          (pid-kill (if (fx>? pgid 0) (fx- pgid) pid) 'sigint))))))


;; the "command" builtin
(define (sh-builtin-command job prog-and-args options)
  (assert-string-list? 'sh-builtin-command prog-and-args)
  (assert* 'sh-builtin-command (string=? "command" (car prog-and-args)))
  (cmd-spawn job (list->argv (cdr prog-and-args)) options)
  (job-last-status job))
