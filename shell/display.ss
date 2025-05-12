;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file shell/job.ss

;; if set to #f, (sh-job-display-summary) and (sh-job-display-summary*) do nothing.
;; useful in forked sub-processes
(define sh-job-display-summary?
  (sh-make-thread-parameter #t))


;; always returns (void) - useful for builtins
(define sh-job-display-summary
  (case-lambda
    ((job-or-id)
      (let ((job (sh-job job-or-id)))
        (sh-job-display-summary* job (job-last-status job) (console-output-port))))
    ((job-or-id port)
      (let ((job (sh-job job-or-id)))
        (sh-job-display-summary* job (job-last-status job) port)))
    ((job-or-id status port)
      (sh-job-display-summary* (sh-job job-or-id) status port))))


;; always returns (void) - useful for builtins
(define (sh-job-display-summary* job status port)
  (when (sh-job-display-summary?)
    (let* ((id     (or (job-id job) (job-oid job)))
           (pid    (job-pid job))
           (status (if (eq? (void) status) (ok) status)))
      (if id
        (let ((preferred-mark (if (eqv? id (sh-preferred-job-id)) #\+ #\space)))
          (if pid
            (format port "; job ~a~s~a pid ~a~s ~s \t" (pad/job-id id) id preferred-mark (pad/pid pid) pid status)
            (format port "; job ~a~s~a            ~s \t" (pad/job-id id) id preferred-mark status)))
        (if pid
          (format port "; job pid ~a~s ~s \t" pid (pad/pid pid) status)
          (format port "; job            ~s \t" status)))
      (sh-job-display* job port)
      (newline port)))
  (void))


(define (thread-display-summary id status port)
  (format port "; thread ~a~s          ~s\n"
          (pad/job-id id) id (if (eq? (void) status) (ok) status)))


;; add a job to an internal queue, and return (void)
;;
;; if called without arguments, return all queued jobs as a list
;; then clears the internal queue.
(define queue-job-display-summary
  (let ((queue '()))
    (case-lambda
      ((job)
        ; (debugf "queue-job-display-summary add job=~s id=~s" job (job-id job))
        (set! queue (cons job queue)))
      (()
        ; (debugf "queue-job-display-summary return jobs=~a" (reverse queue))
        (if (null? queue)
          queue
          (let ((ret queue))
            (set! queue '())
            ret))))))


;; return padding string to align printing job-id
(define (pad/job-id id)
  (if (< id 10) " " ""))

;; return padding string to align printing pid
(define (pad/pid pid)
  (cond
    ((< pid 10) "     ")
    ((< pid 100) "    ")
    ((< pid 1000) "   ")
    ((< pid 10000) "  ")
    ((< pid 100000) " ")
    ((< pid 1000000) "")
    ((< pid 10000000) "   ")
    ((< pid 100000000) "  ")
    ((< pid 1000000000) " ")
    (else                "")))

(define precedence-lowest  0)
(define precedence-list    1)
(define precedence-or      2)
(define precedence-and     3)
(define precedence-pipe    4)
(define precedence-highest 4)

;; display a job using terse shell syntax {foo && bar || baz ...}
(define sh-job-display
  (case-lambda
    ((job-or-id)      (sh-job-display* job-or-id (console-output-port)))
    ((job-or-id port) (sh-job-display* job-or-id port))))


;; same as (sh-job-display), except that all arguments are mandatory
(define (sh-job-display* job-or-id port)
  (let* ((job (sh-job job-or-id))
         (kind (cond
                 ((sh-multijob? job) (multijob-kind job))
                 ((sh-expr?     job) 'sh-expr)
                 (else               #f))))
    (job-display/open-paren  port kind)
    (job-display/any         job port precedence-lowest)
    (job-display/close-paren port kind)))


;; return a string describing a job, using terse shell syntax {foo && bar || baz ...}
(define (sh-job->string job-or-id)
  (if job-or-id
    (let-values (((port get-string) (open-string-output-port)))
      (sh-job-display* job-or-id port)
      (get-string))
    ""))


(define (job-display/any job port outer-precedence)
  (cond
    ((sh-cmd? job)      (job-display/cmd  job port))
    ((sh-expr? job)     (job-display/expr job port))
    ((sh-multijob? job) (job-display/multijob job port outer-precedence))
    (else               (put-string port "???"))))


(define (job-display/multijob job port outer-precedence)
  ; would be informative, but does not correspond
  ; to shell syntax: cannot be parsed back correctly
  ; (job-display/env-lazy job port)
  (let* ((kind (multijob-kind job))
         (precedence
           (case kind
             ((sh-or)   precedence-or)
             ((sh-and)  precedence-and)
             ((sh-pipe) precedence-pipe)
             (else      precedence-list)))
         (separator
           (case kind
             ((sh-or)   " || ")
             ((sh-and)  " && ")
             (else      " "))))
    (when (fx<=? precedence outer-precedence)
      (job-display/open-paren port kind))
    (span-iterate (multijob-children job)
      (lambda (i child)
        (unless (fxzero? i)
          (put-string port separator))
        (if (sh-job? child)
          (job-display/any child port precedence)
          (display child port))))
    (when (fx<=? precedence outer-precedence)
      (job-display/close-paren port kind)))
  (job-display/redirects job port))


(define (job-display/open-paren port kind)
  (case kind
    ((sh-expr)     (void))
    ((sh-subshell) (put-char   port #\[))
    (else          (put-char   port #\{))))


(define (job-display/close-paren port kind)
  (case kind
    ((sh-expr)     (void))
    ((sh-subshell) (put-char   port #\]))
    (else          (put-char   port #\}))))


(define (job-display/expr job port)
  (put-char port #\$)
  (let ((label (jexpr-label job)))
    (if label
      (put-string port label)
      (begin
        (put-char port #\()
        (put-datum port (jexpr-proc job))
        (put-char port #\))))))


(define (job-display/cmd job port)
  (job-display/env-lazy job port)
  (do ((tail (or (cmd-expanded-arg-list job) (cmd-arg-list job)) (cdr tail))
       (first? #t #f))
      ((null? tail))
    (unless first?
      (put-char port #\space))
    (let ((arg (car tail)))
      (if (string-is-shell-identifier? arg)
        (put-string port arg)
        (put-datum  port arg))))
  (job-display/redirects job port))


(define (job-display/env-lazy job port)
  (let ((env-lazy (job-env-lazy job)))
    (when env-lazy
      (do ((i 0 (fx+ i 2))
           (n (span-length env-lazy)))
          ((fx>? (fx+ i 2) n))
        (job-display/env-lazy1 env-lazy i port)))))


(define (job-display/env-lazy1 env-lazy i port)
  (put-string port (span-ref env-lazy i))
  (put-char port #\=)
  (let ((value (span-ref env-lazy (fx1+ i))))
    (if (string-is-shell-identifier? value)
      (put-string port value)
      (put-datum port value)))
  (put-char port #\space))


(define (job-display/redirects job port)
  (let* ((redirects (job-redirects job))
         (n (span-length redirects)))
    ; do not show temporary redirections
    (do ((i (job-redirects-temp-n job) (fx+ i 4)))
        ((fx>? (fx+ i 4) n))
      (job-display/redirect redirects i port))))


(define (job-display/redirect redirects i port)
  (let ((ch (span-ref redirects (fx1+ i)))
        (to (span-ref redirects (fx+ i 2))))
    (put-char port #\space)
    (put-datum port (span-ref redirects i))
    (put-string port (symbol->string (if (fixnum? to)
                                       (%sh-redirect/fd-char->symbol 'sh-job-write ch)
                                       (%sh-redirect/file-char->symbol 'sh-job-write ch))))
    (if (string-is-shell-identifier? to)
      (put-string port to)
      (put-datum port to))))


(define (string-is-shell-identifier? str)
  (and
    (string? str)
    (do ((i 0 (fx1+ i))
         (n (fx1- (string-length str))))
        ((or (fx>=? i n) (not (char-is-shell-identifier? (string-ref str i))))
         (fx>=? i n)))))


(define (char-is-shell-identifier? ch)
  (and (char? ch)
    (or (char<=? #\a ch #\z)
        (char<=? #\A ch #\Z)
        (char<=? #\0 ch #\9)
        (char<=? #\+ ch #\/)  ; i.e. one of #\+ #\, #\- #\. #\/
        (char=?  #\_ ch))))   ; i.e. #\_


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; display a job using verbose Scheme syntax (sh-or (sh-and (sh-cmd "foo") (sh-cmd "bar")) ...)
(define sh-job-write
  (case-lambda
    ((job-or-id)      (sh-job-write* job-or-id (console-output-port)))
    ((job-or-id port) (sh-job-write* job-or-id port))))


;; same as (sh-job-write), except that all arguments are mandatory
(define (sh-job-write* job-or-id port)
  (job-write/any (sh-job job-or-id) port))


;; return a string describing a job, using verbose Scheme syntax (sh-or (sh-and (sh-cmd "foo") (sh-cmd "bar")) ...)
(define (sh-job->verbose-string job-or-id)
  (if job-or-id
    (let-values (((port get-string) (open-string-output-port)))
      (sh-job-write* job-or-id port)
      (get-string))
    ""))


(define (job-write/any job port)
  (cond
    ((sh-cmd? job)      (job-write/cmd job port))
    ((sh-expr? job)     (job-write/expr job port))
    ((sh-multijob? job) (job-write/multijob job port))
    (else               (put-string port "#<unknown-job>"))))


;; return #t if job has non-temporary redirections,
;; otherwise return #f
(define (job-persistent-redirects? job)
  (fx>? (span-length (job-redirects job))
        (job-redirects-temp-n job)))


(define (job-write/multijob job port)
  (let ((kind (multijob-kind job)))
    (cond
      ; do not show temporary redirections
      ((job-persistent-redirects? job)
        (job-write/multijob* job port))
      (else
        (put-char port #\()
        (put-datum port kind)
        (case kind
          ((sh-globals)
            (void))
          ((sh-pipe)
            ;; if all odd-indexed children are the symbol '|
            ;; then write job in simplified form without '|
            ;; as (sh-pipe) function expects
           (span-iterate (multijob-children job)
             (lambda (i child)
               (unless (symbol? child)
                 (put-char  port #\space)
                 (put-datum port child)))))
          (else
            (job-write/children job port)))
        (put-char port #\))))))


(define (job-write/children job port)
  (span-iterate (multijob-children job)
    (lambda (i child)
      (if (symbol? child)
        (put-string port " '")
        (put-char   port #\space))
      (put-datum port child))))


(define (job-write/multijob* job port)
  (put-string port "(sh-redirect (")
  (display (multijob-kind job) port)
  (job-write/children job port)
  (put-string port ")")
  (job-write/redirects job port)
  (put-string port ")"))


(define (job-write/cmd job port)
  (let ((arg-list (or (cmd-expanded-arg-list job) (cmd-arg-list job))))

    (put-string port
      (if (or (job-persistent-redirects? job)
              (job-env-lazy job)
              (not (string-list? arg-list)))
        "(sh-cmd*"
        "(sh-cmd"))
    (job-write/env-lazy job port)
    (for-list ((arg arg-list))
      (put-char port #\space)
      (put-datum port arg))
    (job-write/redirects job port)
    (put-string port ")")))


(define (job-write/env-lazy job port)
  (let ((env-lazy (job-env-lazy job)))
    (when env-lazy
      (do ((i 0 (fx+ i 2))
           (n (span-length env-lazy)))
          ((fx>? (fx+ i 2) n))
      (job-write/env-lazy1 env-lazy i port)))))


(define (job-write/env-lazy1 env-lazy i port)
  (put-char port #\space)
  (put-datum port (span-ref env-lazy i))
  (put-string port " '= ")
  (put-datum port (span-ref env-lazy (fx1+ i))))


(define (job-write/redirects job port)
  (let* ((redirects (job-redirects job))
         (n (span-length redirects)))
    ; do not show temporary redirections
    (do ((i (job-redirects-temp-n job) (fx+ i 4)))
        ((fx>? (fx+ i 4) n))
      (job-write/redirect redirects i port))))


(define (job-write/redirect redirects i port)
  (let ((ch (span-ref redirects (fx1+ i)))
        (to (or (span-ref redirects (fx+ 2 i)) ; string, bytevector or procedure
                (span-ref redirects (fx+ 3 i))))) ; fd
    (put-char port #\space)
    (put-datum port (span-ref redirects i))
    (put-string port " '")
    (put-string port (symbol->string (if (fixnum? to)
                                       (%sh-redirect/fd-char->symbol 'sh-job-write ch)
                                       (%sh-redirect/file-char->symbol 'sh-job-write ch))))
    (put-char port #\space)
    (put-datum port to)))


(define (job-write/expr job port)
  (put-string port "(sh-expr ")
  (let ((label (jexpr-label job)))
    (if label
      (begin
        (put-string port "(lambda () ")
        (put-string port label)
        (put-string port ")"))
      (put-datum  port (jexpr-proc job))))
  (put-string port ")"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sh-job-display-style
  (sh-make-parameter
    'scheme
    (lambda (sym)
      (assert* 'sh-job-display-style (memq sym '(scheme shell)))
      sym)))


;; customize how "job" and subtype objects are printed
(record-writer (record-type-descriptor job)
  (lambda (obj port writer)
    (if (eq? 'scheme (sh-job-display-style))
      (sh-job-write* obj port)
      (sh-job-display* obj port))))
