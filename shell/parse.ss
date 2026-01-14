;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file shell/job.ss


;; Return truish if token is a shell command separator: ; & && || | |&
(define (cmd-separator? token)
  (and (symbol? token)
       (memq token '(& && \x3B; \x7C;\x7C; \x7C; \x7C;&
                     ))))

;; Return truish if token is a shell pipe operator: | |&
(define (pipe-sym? token)
  (and (symbol? token)
       (memq token '(\x7C; \x7C;&
                     ))))

;; Return truish if token is a shell redirection operator: < <> <& > >> >&
(define (redirection-sym? token)
  (and (symbol? token)
       (memq token '(< <> > >> <& >&))))

;; Return truish if token is a shell operator
;; i.e. the symbol '= , or a shell command separator, or a shell pipe operator,
;; or a shell redirection operator, or a shell wildcard operator
(define (operator-sym? token)
  (and (symbol? token)
       (or (eq? '= token)
           (cmd-separator? token)
           (pipe-sym? token)
           (redirection-sym? token)
           (wildcard? token))))

;; Parse args using shell syntax, and return corresponding sh-cmd or sh-multijob object.
;; Current implementation is (sh-eval (sh-parse-datum (cons 'shell args))), which uses (sh-parse-datum)
;; for converting shell commands to Scheme source forms, then (sh-eval) such forms.
;;
;; See (sh-parse) for allowed args.
(define (sh . args)
  ; implementation: use (sh-parse) for converting shell commands to Scheme forms,
  ; then (sh-eval) such forms
  (sh-eval (sh-parse-datum (cons 'shell args))))


;; Parse a list starting with 'shell or 'shell-subshell and containing a sequence
;; of shell commands separated by ; & ! && || |
;; Return parsed list, which typically consists of Scheme source forms
;; that will create sh-cmd or sh-multijob objects if evaluated.
;;
;; Each element in (cdr list) must be a symbol, string, integer or pair:
;; 1. symbols are operators. Recognized symbols are: ; & ! && || | < <> > >> <& >&
;; 2. strings stand for themselves. for example (sh-parse-datum '(shell "ls" "-l"))
;;    returns the Scheme source form '(sh-cmd "ls" "-l")
;; 3. integers are fd numbers, and must be followed by a redirection operator < <> <& > >> >&
;; 4. pairs are not parsed: they are copied verbatim into returned list.
(define (sh-parse-datum args)
  (assert* 'sh-parse-datum (pair? args))
  (let* ((saved-args args)
         (arg0       (car args))
         (args       (cdr args))
         (redirections? #f)
         (terminators? #f)
         (job-n      0)
         (ret        '())
         (ret-prefix
           (cond
             ((eq? 'shell          arg0) 'sh-list)
             ((eq? 'shell-subshell arg0) 'sh-subshell)
             (else
               (syntax-violation 'sh-parse-datum "syntax error, shell DSL form should start with 'shell or 'shell-subshell, found:"
                 saved-args arg0)))))
    ; (debugf ">   sh-parse-datum args = ~s" saved-args)
    (until (null? args)
      (let-values (((parsed tail) (parse-or args 'parse-list)))
        (unless (null? parsed)
          (set! ret (cons parsed ret)))
        (set! args tail)
        (set! job-n (fx1+ job-n))
        (let %again ()
          ;; (debugf "... sh-parse-datum ret = ~s, args = ~s" (reverse ret) args)
          (let ((arg (if (null? args) #f (car args))))
            (cond
              ((job-terminator? arg)
                (set! ret (cons arg ret))
                (set! args (cdr args))
                (set! terminators? #t)
                (%again))
              ((or (fixnum? arg) (redirection-sym? arg))
                ;; if redirections are preceded by '& or '; they must be parsed as a cmd
                ;; otherwise parse them here, they apply to the previous parsed job
                (when (or (null? ret) (not (job-terminator? (car ret))))
                  (let-values (((parsed tail) (parse-redirection args)))
                    (set! ret (append! parsed ret))
                    (set! args tail))
                  (set! redirections? #t)
                  (%again)))
              ((or (null? args) (pair? arg) (symbol? arg) (string? arg))
                #f)
              (else
                (syntax-violation 'sh-parse-datum "syntax error, unexpected shell DSL token:"
                  saved-args arg)))))))
    ; (debugf "<   sh-parse-datum ret = ~s, args = ~s, job-n = ~s, redirections? = ~s, terminators? = ~s" (reverse ret) args job-n redirections? terminators?)
    (when (and redirections? (eq? 'sh-list ret-prefix))
      (if (and (fx=? job-n 1) (not terminators?))
        (set! ret-prefix 'sh-redirect)
        (set! ret-prefix 'sh-list*)))
    (cond
      ((null? ret)
        '(sh-cmd))
      ((null? (cdr ret))
        ;; when prefix is 'sh-list, unwrap single-element ret,
        ;;
        ;; this has the annoying side effect that "{ cd PATH }" becomes "cd PATH"
        ;;   and the latter changes the current directory of (sh-globals), not of the enclosing "{ ... }",
        ;;
        ;; the alternative is worse: if we do *not* unwrap single-element ret,
        ;;   then a plain "cd PATH" is returned as "{ cd PATH }" which has no effect,
        ;;   because it only changes the current directory of the enclosing "{ ... }"
        (if (eq? 'sh-list ret-prefix)
          (car ret)
          (cons ret-prefix ret)))
      (else
       (cons ret-prefix (reverse! (list-quoteq! '(& \x3B;
                                                  ) ret)))))))


;; Parse a list starting with one redirection. Used only for redirections after a group
;; i.e. after one of {} [] () $()
;; Return two values:
;;   A list containing a single parsed redirection, in reverse order;
;;   The remaining, unparsed args.
(define (parse-redirection args)
  (let* ((arg0 (if                (null?       args)   #f (car   args)))
         (arg1 (if (or (not arg0) (null? (cdr  args))) #f (cadr  args)))
         (arg2 (if (or (not arg1) (null? (cddr args))) #f (caddr args))))
    (cond
      ((fixnum? arg0)
        (let-values (((fd dir to) (parse-redirection3 args pair?)))
          (values (list to (list 'quote dir) fd) (cdddr args))))
      ((redirection-sym? arg0)
        (let-values (((fd dir to) (parse-redirection2 args pair?)))
          (values (list to (list 'quote dir) fd) (cddr args))))
      (else
        (values '() args))))) ; no redirection found


;; if obj is a symbol, but has no special meaning in shell syntax,
;;   return (symbol->string obj).
;; Otherwise return obj.
(define (non-operator-symbol->string obj)
  (if (and (symbol? obj) (not (operator-sym? obj)))
    (symbol->string obj)
    obj))


;; parse a single redirection <PATH <>PATH <&M >PATH >>PATH >&M
;; return three values: fd direction to-fd-or-path
(define (parse-redirection2 args verbatim-proc)
  (when (null? (cdr args))
    (raise-errorf 'sh-parse-datum "missing argument after redirection: ~s" args))
  (let ((dir (car args))
        (to  (non-operator-symbol->string (cadr args))))
    (case dir
      ((< <> > >>)
        (unless (or (string? to) (verbatim-proc to))
          (raise-errorf 'sh-parse-datum "expecting string after redirection ~s, found: ~s" dir to)))
      ((<& >&)
        (set! to (parse-redirection-to-fd to))
        (unless (and (fixnum? to) (fx>=? to -1))
          (raise-errorf 'sh-parse-datum "expecting -1 or unsigned fixnum after redirection ~s, found: ~s" dir to)))
      (else
        (raise-errorf 'sh-parse-datum "invalid redirection operator: ~s" dir)))
    (values (if (memq dir '(< <> <&)) 0 1) dir to)))


;; parse a single redirection N<PATH N<>PATH N<&M N>PATH N>>PATH N>&M
;; return three values: fd direction to-fd-or-path
(define (parse-redirection3 args verbatim-proc)
  (when (or (null? (cdr args)) (null? (cddr args)))
    (raise-errorf 'sh-parse-datum "missing argument after redirection: ~s" args))
  (let ((fd  (car args))
        (dir (cadr args))
        (to  (non-operator-symbol->string (caddr args))))
    (unless (and (fixnum? fd) (fx>=? fd 0))
      (raise-errorf 'sh-parse-datum "expecting unsigned fixnum before redirection ~s ~s, found: ~s" fd dir to))
    (case dir
      ((< <> > >>)
        (unless (or (string? to) (verbatim-proc to))
          (raise-errorf 'sh-parse-datum "expecting string after redirection ~s ~s, found:  ~s" fd dir to)))
      ((<& >&)
        (set! to (parse-redirection-to-fd to))
        (unless (and (fixnum? to) (fx>=? to -1))
          (raise-errorf 'sh-parse-datum "expecting -1 or unsigned fixnum after redirection ~s ~s, found: ~s" fd dir to)))
      (else
        (raise-errorf 'sh-parse-datum "invalid redirection operator: ~s" dir)))
    (values fd dir to)))


;; convert "-" to -1
;; convert string containing only decimal digits to integer
;; return verbatim any other value
(define (parse-redirection-to-fd obj)
  (if (string? obj)
    (cond
      ((string=? "-" obj) -1)
      ((string-is-unsigned-base10-integer? obj) (string->number obj))
      (else obj))
    obj))


;; Parse list containing a sequence of shell commands separated by || && |
;; Return two values:
;;   A list containing parsed args;
;;   The remaining, unparsed args.
(define (parse-or args caller)
  (let ((ret '())
        (done? (null? args)))
    (until done?
      (let-values (((parsed tail) (parse-and args (if (null? ret) caller 'parse-or))))
        (unless (null? parsed)
          (set! ret (cons parsed ret)))
        (set! args tail))
      ; (debugf "parse-or  iterate: ret = ~s, args = ~s" (reverse ret) args)
      (cond
        ((null? args) (set! done? #t))
        ((eqv? (car args) '\x7C;\x7C;)
          (set! args  (cdr args))
          (set! done? (null? args)))
        (else (set! done? #t)))) ; unhandled token => exit loop
    ; (debugf "parse-or   return: ret = ~s, args = ~s" (reverse ret) args)
    (values
      (cond
        ((null? ret)       ret)
        ((null? (cdr ret)) (car ret))
        (else              (cons 'sh-or (reverse! ret))))
      args)))


;; Parse list containing a sequence of shell commands separated by && |
;; Return two values:
;;   A list containing parsed args;
;;   The remaining, unparsed args.
;;
(define (parse-and args caller)
  (let ((ret '())
        (done? (null? args)))
    (until done?
      (let-values (((parsed tail) (parse-pipe args (if (null? ret) caller 'parse-and))))
        (unless (null? parsed)
          (set! ret (cons parsed ret)))
        (set! args tail))
      ; (debugf "parse-and iterate: ret = ~s, args = ~s" (reverse ret) args)
      (cond
        ((null? args) (set! done? #t))
        ((eqv? (car args) '&&)
          (set! args  (cdr args))
          (set! done? (null? args)))
        (else (set! done? #t)))) ; unhandled token => exit loop
    ; (debugf "parse-and  return: ret = ~s, args = ~s" (reverse ret) args)
    (values
      (cond
        ((null? ret)       ret)
        ((null? (cdr ret)) (car ret))
        (else              (cons 'sh-and (reverse! ret))))
      args)))

;; Parse list containing a sequence of shell commands separated by |
;; Return two values:
;;   A list containing parsed args;
;;   The remaining, unparsed args.
;;
(define (parse-pipe args caller)
  (let ((ret '())
        (done? (null? args)))
    (until done?
      (let-values (((parsed tail) (parse-not args (if (null? ret) caller 'parse-pipe))))
        (unless (null? parsed)
          (set! ret (cons parsed ret)))
        (set! args tail))
      ; (debugf "parse-pipe iterate: ret = ~s, args = ~s" (reverse ret) args)
      (cond
        ((null? args) (set! done? #t))
        ((memq (car args) '(\x7C; \x7C;&
                            ))
          (set! ret (cons (car args) ret))
          (set! args (cdr args))
          (set! done? (null? args)))
        (else (set! done? #t)))) ; unhandled token => exit loop
    ; (debugf "parse-pipe  return: ret = ~s, args = ~s" (reverse ret) args)
    (values
      (cond
        ((null? ret) ret)
        ((null? (cdr ret)) (car ret))
        (else (cons 'sh-pipe* (reverse! (list-quoteq! '(\x7C; \x7C;&
                                                      ) ret)))))
      args)))


;; Parse a shell command prefixed by one or more !
;; Return two values:
;;   A list containing parsed args;
;;   The remaining, unparsed args.
;;
(define (parse-not args caller)
  (let %again ((negate? #f)
               (args args)
               (caller caller))
    ; (debugf "parse-not iterate: negate? = ~s, args = ~s" negate? args)
    (cond
      ((and (not (null? args)) (eq? '! (car args)))
        (%again (not negate?) (cdr args) 'parse-not))
      (negate?
        (let-values (((parsed tail) (parse-cmd args caller)))
          (values (list 'sh-not parsed) tail)))
      (else
        (parse-cmd args caller)))))


;; Parse args for a single shell command, i.e. everything before the first ; & && || |
;; Return two values:
;;   A list containing parsed args;
;;   The remaining, unparsed args.
;
(define (parse-cmd args caller)
  (let ((saved-args args)
        (ret '())
        (prefix 'sh-cmd)
        (done? (null? args)))
    (until (or done? (null? args))
      ; (debugf "parse-cmd iterate: ret = ~s, args = ~s" (reverse ret) args)
      (let ((arg (car args)))
        ; (debugf "parse-cmd iterate: ret = ~s, arg = ~s, args = ~s" (reverse ret) arg (cdr args))
        (cond
          ((cmd-separator? arg)
            (set! done? #t)) ; separator => exit loop without consuming it
          ((and (null? ret) (pair? arg) (not (%cmd-subform? arg)))
            ;; shell command starts with a Scheme or shell subform
            ;; => return it as-is, without wrapping in (sh-cmd ...)
            ;; but parse trailing redirections
            (let-values (((form rest) (parse-cmd-scheme args caller)))
              (set! ret form)
              (set! args rest))
            (set! done? #t)
            (set! prefix #f))
          ((or (fixnum? arg) (pair? arg) (symbol? arg) (string? arg))

            (when (and (symbol? arg) (not (operator-sym? arg)))
              ;; relaxed syntax: convert non-operator symbols to strings.
              ;; converts (shell ls -l) to (shell "ls" "-l")
              (set! arg (symbol->string arg)))

            (unless (string? arg)
              ;; (sh-cmd) does not support env assignment, redirections and closures
              ;; => use (sh-cmd*)
              (set! prefix 'sh-cmd*))

            (cond
              ((fixnum? arg)
                (let-values (((fd dir to) (parse-redirection3 args pair?)))
                  (set! ret (cons to (cons (list 'quote dir) (cons fd ret))))
                  (set! args (cdddr args))))
              ((redirection-sym? arg)
                (let-values (((fd dir to) (parse-redirection2 args pair?)))
                  (set! ret (cons to (cons (list 'quote dir) (cons fd ret))))
                  (set! args (cddr args))))
              (else
                (set! ret (cons (if (symbol? arg) (list 'quote arg) arg)
                                ret))
                (set! args (cdr args)))))
          (else
            (syntax-violation 'sh-parse
              "syntax error, shell DSL atom must be a string, fixnum, pair, = or redirection operator, found:"
              saved-args arg)))))
    ; (debugf "parse-cmd  return: prefix=~s ret=~s, args=~s" prefix (reverse ret) args)
    (values
      ; optimize away empty (sh-cmd)
      (if (and prefix (not (and (null? ret) (eq? 'sh-cmd prefix))))
        (cons prefix (reverse! ret))
        ret)
      args)))


(define (%cmd-subform? form)
  (and (pair? form)
       ;; fragile, recognizes known macros by name and treats them specially
       (memq (car form) '(shell-backquote shell-wildcard))))


;; Parse a single shell command starting with a cons, as for example ( ... ) or $( ... )
;; possibly followed by redirections.
;; Return two values:
;;   A list containing parsed args;
;;   The remaining, unparsed args.
(define (parse-cmd-scheme args caller)
  (let ((ret  (list (car args)))
        (args (cdr args))
        ;; if caller is sh-parse-datum, don't parse redirections: caller will parse them
        (done? (eq? caller 'parse-list))
        (reverse? #f))
    (until (or done? (null? args))
      (let ((arg (car args)))
        (cond
          ((fixnum? arg)
            (let-values (((fd dir to) (parse-redirection3 args pair?)))
              (set! ret (cons to (cons (list 'quote dir) (cons fd ret))))
              (set! args (cdddr args))
              (set! reverse? #t)))
          ((redirection-sym? arg)
            (let-values (((fd dir to) (parse-redirection2 args pair?)))
              (set! ret (cons to (cons (list 'quote dir) (cons fd ret))))
              (set! args (cddr args))
              (set! reverse? #t)))
          (else
            (set! done? #t)))))
    (values
      (if reverse? (cons 'sh-redirect (reverse! ret)) (car ret))
      args)))


;; Create a cmd to later spawn it.
;; Each argument must be a string, symbol, fixnum, procedure, sh-expr or (void)
;;
;; Symbol '= indicates an environment variable assignment, and must be preceded by
;; the variable name (a non-empty string) and followed by its value (a string or closure that returns a string).
;;
;; Symbols '< '<> '> '>> indicate a file redirection and must be followed by a string or closure that returns a string.
;;
;; Symbols '<& '>& indicate an fd redirection and must be followed by -1 or and unsigned fixnum.
;;
;; All redirection symbols '< '<> '> '>> '<& '>& can optionally be preceded by an unsigned fixnum,
;; indicating which file descriptor should be redirected.
;;
;; Each argument that is not part of an assignment or a redirection must be one of:
;;   (void) - it will be ignored
;;   a string
;;   a string list
;;   a procedure that accepts 0 or 1 arguments (the job)
;;     and it must return a single value: a string, a list of strings, or (void)
;;   a sh-expr job that, when executed,
;;     must finish with a single value: a string, a list of strings, or (void)
(define (sh-cmd* . args)
  (let-values (((program-and-args assignments redirections)
                  (cmd-parse-assignments-and-redirections
                    (cmd-expand-string-lists args))))
    (let ((cmd (make-sh-cmd program-and-args)))
      (for-list ((assignment assignments))
        (let ((name (car assignment))
              (value (cdr assignment)))
          (sh-env-set/lazy! cmd name value)))
      (for-list ((redirection redirections))
        (sh-redirect cmd (car redirection) (cadr redirection) (caddr redirection)))
      cmd)))


;; if some element in args is a list of strings, splice it into the returned list.
;; if some element in args is (void), remove it.
;;
;; these are intentionally the same rules used by (cmd-arg-list-call-closures)
;; to expand the results of closures.
(define (cmd-expand-string-lists args)
  (let %loop ((args args) (ret '()))
    (if (null? args)
      (reverse! ret)
      (let ((arg (car args)))
        (cond
          ((or (null? arg) (eq? (void) arg))
            (%loop (cdr args) ret))
          ((pair? arg)
            (assert-string-list? 'sh-cmd* arg)
            (%loop (cdr args) (append! (reverse arg) ret)))
          (else
            (%loop (cdr args) (cons arg ret))))))))



;; parse environment variable assignments NAME = VALUE
;; parse redirections [N]<PATH [N]<>PATH [N]<&M [N]>PATH [N]>>PATH [N]>&M
;; skip arguments equal to (void)
(define (cmd-parse-assignments-and-redirections program-and-args)
  (let %again ((args program-and-args)
               (rets '())
               (assignments '())
               (redirections '()))
    (cond
      ((null? args)
        (values (reverse! rets) (reverse! assignments) (reverse! redirections)))
      ((and (not (null? (cdr args))) (eq? (cadr args) '=))
        (unless (null? rets)
          (raise-errorf 'sh-cmd* "env assignments are not allowed after arguments, found ~s after ~s" args (reverse! rets)))
        (unless (null? redirections)
          (raise-errorf 'sh-cmd* "env assignments are not allowed after redirections, found ~s after ~s" args (car redirections)))
        (let ((assignment (cmd-parse-assignment args)))
          (%again (cdddr args) rets (cons assignment assignments) redirections)))
      ((or (string? (car args)) (procedure? (car args)) (sh-expr? (car args)))
        (%again (cdr args) (cons (car args) rets) assignments redirections))
      ((redirection-sym? (car args))
        (let-values (((fd dir to) (parse-redirection2 args procedure?)))
          (%again (cddr args) rets assignments (cons (list fd dir to) redirections))))
      ((fixnum? (car args))
        (let-values (((fd dir to) (parse-redirection3 args procedure?)))
          (%again (cdddr args) rets assignments (cons (list fd dir to) redirections))))
      ((eq? (void) (car args))
        (%again (cdr args) rets assignments redirections))
      (else
        (raise-errorf 'sh-cmd* "expecting assignment, argument or redirection, found: ~s" args)))))




;; parse a single environment variable assignment NAME = VALUE
;; and return it as a (cons NAME VALUE)
(define (cmd-parse-assignment args)
  (when (or (null? (cdr args)) (null? (cddr args)))
    (raise-errorf 'sh-cmd* "missing value after assignment: ~s" args))
  (let ((name  (car args))
        (op    (cadr args))
        (value (caddr args)))
    (unless (eq? op '=)
      (raise-errorf 'sh-cmd* "invalid assignment operator: ~s" op))
    (unless (and (string? name) (not (fxzero? (string-length name))))
      (raise-errorf 'sh-cmd* "expecting non-empty string before assignment, found: ~s ~s ~s" name op value))
    (unless (or (string? value) (procedure? value))
      (raise-errorf 'sh-cmd* "expecting string or closure after assignment, found: ~s ~s ~s" name op value))
    (cons name value)))



;; Create a list of jobs with redirections.
;;
;; Arguments must be jobs,
;; each possibly followed by one or more redirections,
;; each possibly followed by a terminator ; &
;; Redirections are applied to the last preceding job.
(define (sh-list* . children-jobs-with-redirections-colon-ampersand)
  (let %again ((jobs '())
               (args children-jobs-with-redirections-colon-ampersand))
    (let ((arg (if (null? args) #f (car args))))
      ; (debugf "sh-list* jobs = ~s, arg = ~s, args = ~s" (reverse jobs) arg args)
      (cond
        ((null? args)
          (apply sh-list (reverse! jobs)))
        ((or (job-terminator? arg) (sh-job? arg))
          (%again (cons arg jobs) (cdr args)))
        ((redirection-sym? arg)
          (%assert-last-is-job jobs arg children-jobs-with-redirections-colon-ampersand)
          (let-values (((fd dir to) (parse-redirection2 args procedure?)))
            ; modify last job in-place
            (sh-redirect (car jobs) fd dir to)
            (%again jobs (cddr args))))
        ((fixnum? arg)
          (%assert-last-is-job jobs arg children-jobs-with-redirections-colon-ampersand)
          (let-values (((fd dir to) (parse-redirection3 args procedure?)))
            ; modify last job in-place
            (sh-redirect (car jobs) fd dir to)
            (%again jobs (cdddr args))))
        (else
          (raise-errorf 'sh-list* "expecting job, redirection or ; &, found:"
            arg children-jobs-with-redirections-colon-ampersand))))))


(define (%assert-last-is-job jobs arg all-args)
  (when (or (null? jobs) (not (sh-job? (car jobs))))
    (raise-errorf 'sh-list* "redirections are allowed only after a job, found:"
      arg all-args)))
