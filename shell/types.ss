;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should only be included inside a (library ...) definition



(define-record-type
  (job %make-job sh-job?)
  (fields
    (mutable id job-id %job-id-set!) ; fixnum: job id in (sh-globals), #f if not set
    (mutable pid)               ; fixnum: process id,       -1 if unknown
    (mutable pgid)              ; fixnum: process group id, -1 if unknown
     ; cons: last known status, or (void) if job exited successfully
    (mutable last-status job-last-status %job-last-status-set!)
    ; span of quadruplets (fd mode to-fd-or-path-or-closure bytevector0)
    ; to open and redirect between fork() and exec()
    (mutable redirects)
    (mutable fds-to-remap) ; for builtins or multijobs, #f or hashmap job-logical-fd -> actual-fd-to-use
    (mutable fds-to-close) ; for builtins or multijobs, '() or list of fds to close at job exit
    start-proc      ; #f or procedure to run in main process.
                    ; receives as argument job followed by options.
    step-proc       ; #f or procedure.
                    ; For multijobs, will be called when a child job changes status.
                    ; For cmds, will be called in fork()ed child process and
                    ; receives as argument job followed by options.
                    ; For cmds, its return value is passed to (exit-with-job-status)
    (mutable cwd)        ; charspan: working directory
    (mutable env)        ; #f or hashtable of overridden env variables: name -> value
    (mutable env-lazy)   ; #f or span of env variable name each followed by string or procedure
    (mutable parent))    ; parent job, contains default values of env variables
                         ; and default redirections
  (nongenerative #{job ghm1j1xb9o5tkkhhucwauly2c-1175}))


;; Define the record type "cmd"
(define-record-type
  (cmd %make-cmd sh-cmd?)
  (parent job)
  (fields arg-list) ; list of strings: program-name and args
  (nongenerative #{cmd ghm1j1xb9o5tkkhhucwauly2c-1176}))


;; Define the record type "multijob"
(define-record-type
  (multijob %make-multijob sh-multijob?)
  (parent job)
  (fields
    kind                ; symbol: one of 'sh-and 'sh-or 'sh-not 'sh-list 'sh-subshell 'sh-global
    (mutable current-child-index) ; -1 or index of currently running child job
    children)           ; span: children jobs.
  (nongenerative #{multijob ghm1j1xb9o5tkkhhucwauly2c-1177}))



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
