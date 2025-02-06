;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only from file shell/job.ss



(define-record-type
  (job %make-job sh-job?)
  (fields
    (mutable id job-id %job-id-set!)       ; #f or fixnum >= 0: job id in (sh-globals)
    (mutable pid)                          ; #f or integer > 0: process id
    (mutable pgid job-pgid %job-pgid-set!) ; #f or integer > 0: process group id
     ; cons: last known status, or (void) if job exited successfully
    (mutable last-status job-last-status %job-last-status-set!)
    ; #f or exception that caused the job to terminate
    (mutable exception)
    ; span of quadruplets (fd mode to-fd-or-path-or-closure bytevector0)
    ; to open and redirect between fork() and exec()
    (mutable redirects)
    (mutable redirects-temp-n) ; fixnum, number elements at front of (job-redirects)
                               ; inserted by temporary redirections
    (mutable fds-to-remap) ; for builtins or multijobs, #f or hashmap job-logical-fd -> actual-fd-to-use
    (mutable fds-to-close) ; for builtins or multijobs, '() or list of fds to close at job exit
    start-proc      ; #f or procedure to run in main process.
                    ; receives as argument job followed by options.
    (mutable step-proc) ; #f or procedure.
                    ; For multijobs, will be called when a child job changes status.
                    ; For cmds, will be called in fork()ed child process and
                    ; receives as argument job followed by options.
                    ; For cmds, its return value is passed to (exit-with-job-status)
    (mutable cwd %job-cwd job-cwd-set!) ; charspan: working directory. if #f, use parent's cwd
    (mutable env)         ; #f or hashtable of overridden env variables: name -> value
    (mutable env-lazy)    ; #f or span of env variable name each followed by string or procedure
    (mutable temp-parent) ; temporary parent job, contains default values of env variables.
                          ; Unset when job finishes
    (mutable default-parent)) ; default parent job, contains default values of env variables
  (nongenerative #{job lbuqbuslefybk7xurqc6uyhyv-3}))


;; Define the record type "cmd"
(define-record-type
  (cmd %make-cmd sh-cmd?)
  (parent job)
  (fields
    arg-list                     ; list of strings and closures: program-name and args
    (mutable expanded-arg-list)) ; #f or list of strings: program-name and args after applying closures and expanding aliases
  (nongenerative #{cmd lbuqbuslefybk7xurqc6uyhyv-4}))


;; Define the record type "multijob"
(define-record-type
  (multijob %make-multijob sh-multijob?)
  (parent job)
  (fields
    kind                ; symbol: one of 'sh-and 'sh-or 'sh-not 'sh-list 'sh-subshell '#<global>
    (mutable current-child-index) ; -1 or index of currently running child job
    children)           ; span: children jobs.
  (nongenerative #{multijob lbuqbuslefybk7xurqc6uyhyv-5}))


;; Create a copy of job and all its children jobs, and return it.
;; Returned job will have no id, status '(new . 0) and specified parent, or (sh-globals) if not specified
(define sh-job-copy
  (case-lambda
    ((job) (sh-job-copy job (sh-globals)))
    ((job parent)
      (cond ((sh-cmd?      job) (cmd-copy      job parent))
            ((sh-multijob? job) (multijob-copy job parent))
            (raise-errorf 'sh-job-copy "~s is not a sh-cmd or a sh-multijob" job)))))


;; Create a copy of sh-cmd j, and return it.
;; Returned job will have no id, status '(new . 0) and specified parent.
(define (cmd-copy j parent)
  (%make-cmd
    #f #f #f             ; id pid pgid
    '(new . 0) #f        ; status exception
    (let ((redirects (job-redirects j)))
      (span-copy redirects (job-redirects-temp-n j) (span-length redirects)))
    0                    ; redirects-temp-n
    #f #f                ; fds-to-remap fds-to-close
    (job-start-proc j)
    (job-step-proc  j)
    (let ((cwd (%job-cwd j)))
      (and cwd (charspan-copy cwd)))
    (let ((env (job-env j)))
      (and env (hashtable-copy env)))
    (let ((env-lazy (job-env-lazy j)))
      (and env-lazy (span-copy env-lazy)))
    #f                   ; temp-parent
    parent               ; default-parent
    (filter (lambda (x) #t) (cmd-arg-list j)) ; copy arg-list
    #f))                 ; expanded-arg-list


;; Create a copy of sh-multijob j, and return it.
;; Returned job will have no id, status '(new . 0) and specified parent.
;; Children jobs will be copied recursively.
(define (multijob-copy j parent)
  (let* ((children (job-span-copy (multijob-children j)))
         (ret
    (%make-multijob
      #f #f #f             ; id pid pgid
      '(new . 0) #f        ; status exception
      (let ((redirects (job-redirects j)))
        (span-copy redirects (job-redirects-temp-n j) (span-length redirects)))
      0                    ; redirects-temp-n
      #f #f                ; fds-to-remap fds-to-close
      (job-start-proc j)
      (job-step-proc  j)
      (let ((cwd (%job-cwd j)))
        (and cwd (charspan-copy cwd)))
      (let ((env (job-env j)))
        (and env (hashtable-copy env)))
      (let ((env-lazy (job-env-lazy j)))
        (and env-lazy (span-copy env-lazy)))
      #f                   ; temp-parent
      parent               ; default-parent
      (multijob-kind j)
      -1                   ; current-child-index
      children)))
    (span-iterate children
      (lambda (i elem)
        (when (sh-job? elem)
          (job-default-parent-set! elem ret))))
    ret))


;; Create a copy of span containing jobs and symbols, and return it.
;; Returned jobs will have no id, status '(new . 0) and parent (sh-globals)
;; Children jobs will be copied recursively.
(define (job-span-copy src)
  (let ((dst (make-span (span-length src))))
    (span-iterate src
      (lambda (i elem)
        (span-set! i dst (if (sh-job? elem)
                           (sh-job-copy elem)
                           elem))))
    dst))


(define (%sh-redirect/fd-symbol->char caller symbol)
  (case symbol
    ((<&) #\<)
    ((>&) #\>)
    (else
      (raise-errorf caller "invalid redirect to fd direction, must be <& or >&: ~a" symbol))))


(define (%sh-redirect/file-symbol->char caller symbol)
  (case symbol
    ((<) #\<)
    ((>) #\>)
    ((<>) (integer->char #x2276)) ; #\≶
    ((>>) (integer->char #x00bb)) ; #\»
    (else
      (raise-errorf caller "invalid redirect to file direction, must be < > <> or >>: ~a" symbol))))


(define (%sh-redirect/fd-char->symbol caller ch)
  (case ch
    ((#\<) '<&)
    ((#\>) '>&)
    (else
      (raise-errorf caller "invalid redirect to fd character, must be <& or >&: ~a" ch))))


(define (%sh-redirect/file-char->symbol caller ch)
  (case (char->integer ch)
    ((#x3c) '<)
    ((#x3e) '>)
    ((#x2276) '<>)
    ((#x00bb) '>>)
    (else
      (raise-errorf caller "invalid redirect to file character, must be < <> > or >>: ~a" ch))))
