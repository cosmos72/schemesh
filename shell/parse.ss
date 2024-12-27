;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh shell parse (0 1))
  (export sh sh-parse sh-cmd* sh-list*)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) append! eval expand fx1+ reverse!)
    (only (schemesh bootstrap)      assert* debugf raise-errorf until)
    (only (schemesh containers misc)   list-iterate list-quoteq! string-contains-only-decimal-digits?)
    (only (schemesh containers hashtable) eq-hashtable)
    (schemesh shell jobs))

;; Return #t if token is a shell job terminator: ; &
(define (job-terminator? token)
  (and (symbol? token)
       (or (eq? token '&) (eq? token '\x3b;))))

;; Return #t if token is a shell command separator: ; & && || |
(define (cmd-separator? token)
  (and (symbol? token)
       (memq token '(& && \x3b; \x7c;\x7c; \x7c; \x7c;&
                     ))))

;; Return #t if token is a shell redirection operator: < <> <& > >> >&
(define (redirection-sym? token)
  (and (symbol? token)
       (memq token '(< <> > >> <& >&))))


;; Parse args using shell syntax, and return corresponding sh-cmd or sh-multijob object.
;; Current implementation is (eval (sh-parse (cons 'shell args))), which uses (sh-parse)
;; for converting shell commands to Scheme source forms, then (eval) such forms.
;;
;; See (sh-parse) for allowed args.
(define (sh . args)
  ; implementation: use (sh-parse) for converting shell commands to Scheme forms,
  ; then (eval) such forms
  (eval (sh-parse (cons 'shell args))))


;; Parse a list starting with 'shell or 'shell-subshell and containing a sequence
;; of shell commands separated by ; & ! && || |
;; Return parsed list, which typically consists of Scheme source forms
;; that will create sh-cmd or sh-multijob objects if evaluated.
;;
;; Each element in (cdr list) must be a symbol, string, integer or pair:
;; 1. symbols are operators. Recognized symbols are: ; & ! && || | < <> > >> <& >&
;; 2. strings stand for themselves. for example (sh-parse '(shell "ls" "-l"))
;;    returns the Scheme source form '(sh-cmd "ls" "-l")
;; 3. integers are fd numbers, and must be followed by a redirection operator < <> <& > >> >&
;; 4. pairs are not parsed: they are copied verbatim into returned list.
(define (sh-parse args)
  (assert* 'sh-parse (pair? args))
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
             (#t
               (syntax-violation 'sh-parse "syntax error, shell DSL form should start with 'shell or 'shell-subshell, found:"
                 args arg0)))))
    (validate args)
    (until (null? args)
      (let-values (((parsed tail) (parse-or args)))
        (unless (null? parsed)
          (set! ret (cons parsed ret)))
        (set! args tail)
        (set! job-n (fx1+ job-n))
        ; (debugf "sh-parse           iterate: ret = ~s, args = ~s~%" (reverse ret) args)
        (let %again ()
          (let ((arg (if (null? args) #f (car args))))
            (cond
              ((or (null? args) (string? arg) (pair? arg))
                #f)
              ((job-terminator? arg)
                (set! ret (cons arg ret))
                (set! args (cdr args))
                (set! terminators? #t)
                (%again))
              ((or (fixnum? arg) (redirection-sym? arg))
                (let-values (((parsed tail) (parse-redirection args)))
                  (set! ret (append! parsed ret))
                  (set! args tail))
                (set! redirections? #t)
                (%again))
              (#t
                (syntax-violation 'sh-parse "syntax error, unknown shell DSL operator:"
                  saved-args arg)))))))
    ; (debugf "sh-parse            return: ret = ~s, args = ~s, job-n = ~s, redirections? = ~s, terminators? = ~s~%" (reverse ret) args job-n redirections? terminators?)
    (when (and redirections? (eq? 'sh-list ret-prefix))
      (if (and (fx=? job-n 1) (not terminators?))
        (set! ret-prefix 'sh-redirect!)
        (set! ret-prefix 'sh-list*)))
    (cond
      ((null? ret) '(sh-cmd))
      ((null? (cdr ret))
        (if (eq? 'sh-list ret-prefix)
          (car ret)
          (cons ret-prefix ret)))
      (#t
       (cons ret-prefix (reverse! (list-quoteq! '(& \x3b;
                                                  ) ret)))))))

;; validate list containing a sequence of shell commands separated by ; & ! && || |
(define (validate args)
  (unless (or (null? args) (null? (cdr args)))
    (let ((arg1 (car args))
          (arg2 (cadr args)))
      (when (and (symbol? arg1) (symbol? arg2) (not (eq? '! arg2)))
        (unless (and (eq? arg1 '\x3b;) (job-terminator? arg2))
          (syntax-violation 'sh-parse "syntax error, invalid consecutive shell DSL operators:"
              args arg2))))
    (validate (cdr args))))



;; Parse a list starting with one redirection. Used only for redirections after a group
;; i.e. { ... } REDIRECTIONS or [ ... ] REDIRECTIONS
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
      (#t
        (values '() args))))) ; no redirection found



;; parse a single redirection <PATH <>PATH <&M >PATH >>PATH >&M
;; return three values: fd direction to-fd-or-path
(define (parse-redirection2 args verbatim-proc)
  (when (null? (cdr args))
    (raise-errorf 'sh-parse "missing argument after redirection: ~s" args))
  (let ((dir (car args))
        (to  (cadr args)))
    (case dir
      ((< <> > >>)
        (unless (or (string? to) (verbatim-proc to))
          (raise-errorf 'sh-parse "expecting string after redirection, found: ~s ~s" dir to)))
      ((<& >&)
        (set! to (parse-redirection-to-fd to))
        (unless (and (fixnum? to) (fx>=? to -1))
          (raise-errorf 'sh-parse "expecting -1 or unsigned fixnum after redirection, found: ~s ~s" dir to)))
      (else
        (raise-errorf 'sh-parse "invalid redirection operator: ~s" dir)))
    (values (if (memq dir '(< <> <&)) 0 1) dir to)))


;; parse a single redirection N<PATH N<>PATH N<&M N>PATH N>>PATH N>&M
;; return three values: fd direction to-fd-or-path
(define (parse-redirection3 args verbatim-proc)
  (when (or (null? (cdr args)) (null? (cddr args)))
    (raise-errorf 'sh-parse "missing argument after redirection: ~s" args))
  (let ((fd  (car args))
        (dir (cadr args))
        (to  (caddr args)))
    (unless (and (fixnum? fd) (fx>=? fd 0))
      (raise-errorf 'sh-parse "expecting unsigned fixnum before redirection, found: ~s ~s ~s" fd dir to))
    (case dir
      ((< <> > >>)
        (unless (or (string? to) (verbatim-proc to))
          (raise-errorf 'sh-parse "expecting string after redirection, found: ~s ~s ~s" fd dir to)))
      ((<& >&)
        (set! to (parse-redirection-to-fd to))
        (unless (and (fixnum? to) (fx>=? to -1))
          (raise-errorf 'sh-parse "expecting -1 or unsigned fixnum after redirection, found: ~s ~s ~s" fd dir to)))
      (else
        (raise-errorf 'sh-parse "invalid redirection operator: ~s" dir)))
    (values fd dir to)))


;; convert "-" to -1
;; convert string containing only decimal digits to integer
;; return verbatim any other value
(define (parse-redirection-to-fd obj)
  (if (string? obj)
    (cond
      ((string=? "-" obj) -1)
      ((string-contains-only-decimal-digits? obj) (string->number obj))
      (#t obj))
    obj))


;; Parse list containing a sequence of shell commands separated by || && |
;; Return two values:
;;   A list containing parsed args;
;;   The remaining, unparsed args.
(define (parse-or args)
  (let ((ret '())
        (done? (null? args)))
    (until done?
      (let-values (((parsed tail) (parse-and args)))
        (unless (null? parsed)
          (set! ret (cons parsed ret)))
        (set! args tail))
      ; (debugf "parse-or  iterate: ret = ~s, args = ~s~%" (reverse ret) args)
      (cond
        ((null? args) (set! done? #t))
        ((eqv? (car args) '\x7c;\x7c;)
          (set! args  (cdr args))
          (set! done? (null? args)))
        (#t   (set! done? #t)))) ; unhandled token => exit loop
    ; (debugf "parse-or   return: ret = ~s, args = ~s~%" (reverse ret) args)
    (values
      (cond
        ((null? ret)       ret)
        ((null? (cdr ret)) (car ret))
        (#t                (cons 'sh-or (reverse! ret))))
      args)))


;; Parse list containing a sequence of shell commands separated by && |
;; Return two values:
;;   A list containing parsed args;
;;   The remaining, unparsed args.
;;
(define (parse-and args)
  (let ((ret '())
        (done? (null? args)))
    (until done?
      (let-values (((parsed tail) (parse-pipe args)))
        (unless (null? parsed)
          (set! ret (cons parsed ret)))
        (set! args tail))
      ; (debugf "parse-and iterate: ret = ~s, args = ~s~%" (reverse ret) args)
      (cond
        ((null? args) (set! done? #t))
        ((eqv? (car args) '&&)
          (set! args  (cdr args))
          (set! done? (null? args)))
        (#t   (set! done? #t)))) ; unhandled token => exit loop
    ; (debugf "parse-and  return: ret = ~s, args = ~s~%" (reverse ret) args)
    (values
      (cond
        ((null? ret)       ret)
        ((null? (cdr ret)) (car ret))
        (#t                (cons 'sh-and (reverse! ret))))
      args)))

;; Parse list containing a sequence of shell commands separated by |
;; Return two values:
;;   A list containing parsed args;
;;   The remaining, unparsed args.
;;
(define (parse-pipe args)
  (let ((ret '())
        (done? (null? args)))
    (until done?
      (let-values (((parsed tail) (parse-not args)))
        (unless (null? parsed)
          (set! ret (cons parsed ret)))
        (set! args tail))
      ; (debugf "parse-pipe iterate: ret = ~s, args = ~s~%" (reverse ret) args)
      (cond
        ((null? args) (set! done? #t))
        ((memq (car args) '(\x7c; \x7c;&
                            ))
          (set! ret (cons (car args) ret))
          (set! args (cdr args))
          (set! done? (null? args)))
        (#t   (set! done? #t)))) ; unhandled token => exit loop
    ; (debugf "parse-pipe  return: ret = ~s, args = ~s~%" (reverse ret) args)
    (values
      (cond
        ((null? ret) ret)
        ((null? (cdr ret)) (car ret))
        (#t (cons 'sh-pipe* (reverse! (list-quoteq! '(\x7c; \x7c;&
                                                      ) ret)))))
      args)))


;; Parse a shell command prefixed by one or more !
;; Return two values:
;;   A list containing parsed args;
;;   The remaining, unparsed args.
;;
(define (parse-not args)
  (let %again ((negate? #f)
               (args args))
    ; (debugf "parse-not iterate: negate? = ~s, args = ~s~%" negate? args)
    (cond
      ((and (not (null? args)) (eq? '! (car args)))
        (%again (not negate?) (cdr args)))
      (negate?
        (let-values (((parsed tail) (parse-cmd args)))
          (values (list 'sh-not parsed) tail)))
      (#t
        (parse-cmd args)))))


;; Parse args for a single shell command, i.e. everything before the first ; & && || |
;; Return two values:
;;   A list containing parsed args;
;;   The remaining, unparsed args.
;
(define (parse-cmd args)
  (let ((saved-args args)
        (ret '())
        (prefix 'sh-cmd)
        (done? (null? args)))
    (until (or done? (null? args))
      ; (debugf "parse-cmd iterate: ret = ~s, args = ~s~%" (reverse ret) args)
      (let ((arg (car args)))
        ; (debugf "parse-cmd iterate: ret = ~s, arg = ~s, args = ~s~%" (reverse ret) arg (cdr args))
        (cond
          ((cmd-separator? arg)
            (set! done? #t)) ; separator => exit loop without consuming it
          ((and (pair? arg) (null? ret))
            ; shell command starts with a Scheme or shell subform
            ; => return it as-is, without wrapping in (sh-cmd ...)
            (set! ret arg)
            (set! args (cdr args))
            (set! done? #t)
            (set! prefix #f))
          ((or (string? arg) (fixnum? arg) (eq? '= arg) (pair? arg) (redirection-sym? arg))
            (when (or (fixnum? arg) (symbol? arg) (pair? arg))
              ; (sh-cmd) does not support env assignment, redirections and closures, use (sh-cmd*)
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
              (#t
                (set! ret (cons (if (symbol? arg) (list 'quote arg) arg)
                                ret))
                (set! args (cdr args)))))
          (#t
            (syntax-violation 'sh-parse
              "syntax error, shell DSL atom must be a string, fixnum, pair, = or redirection operator, found:"
              saved-args arg)))))
    ; (debugf "parse-cmd  return: prefix=~s ret=~s, args=~s~%" prefix (reverse ret) args)
    (values
      ; optimize away empty (sh-cmd)
      (if (and prefix (not (and (null? ret) (eq? 'sh-cmd prefix))))
        (cons prefix (reverse! ret))
        ret)
      args)))



;; Create a cmd to later spawn it.
;; Each argument must be a string, symbol, fixnum or closure that return a string.
;;
;; Symbol '= indicates an environment variable assignment, and must be preceded by
;; the variable name (a non-empty string) and followed by its value (a string).
;;
;; Symbols '< '<> '> '>> indicate a file redirection and must be followed by a string or closure that return a string.
;;
;; Symbols '<& '>& indicate an fd redirection and must be followed by -1 or and unsigned fixnum.
;;
;; All redirection symbols '< '<> '> '>> '<& '>& can optionally be preceded by an unsigned fixnum,
;; indicating which file descriptor should be redirected.
;;
;; Arguments that are not part of an assignment or a redirection
;; must be strings or closures that return a string.
(define (sh-cmd* . program-and-args)
  (let-values (((program-and-args assignments redirections)
                  (cmd-parse-assignments-and-redirections program-and-args)))
    (let ((cmd (sh-make-cmd program-and-args)))
      ;; FIXME: apply parsed assignments NAME = VALUE
      (list-iterate redirections
        (lambda (redirection)
          (sh-redirect! cmd (car redirection) (cadr redirection) (caddr redirection))))
      cmd)))



;; parse environment variable assignments NAME = VALUE
;; parse redirections [N]<PATH [N]<>PATH [N]<&M [N]>PATH [N]>>PATH [N]>&M
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
      ((or (string? (car args)) (procedure? (car args)))
        (%again (cdr args) (cons (car args) rets) assignments redirections))
      ((redirection-sym? (car args))
        (let-values (((fd dir to) (parse-redirection2 args procedure?)))
          (%again (cddr args) rets assignments (cons (list fd dir to) redirections))))
      ((fixnum? (car args))
        (let-values (((fd dir to) (parse-redirection3 args procedure?)))
          (%again (cdddr args) rets assignments (cons (list fd dir to) redirections))))
      (#t
        (raise-errorf 'sh-cmd* "expecting assignment, argument or redirection, found: ~s" args)))))




;; parse a single environment variable assignment NAME = VALUE
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
    (list name op value)))



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
      ; (debugf "sh-list* jobs = ~s, arg = ~s, args = ~s~%" (reverse jobs) arg args)
      (cond
        ((null? args)
          (apply sh-list (reverse! jobs)))
        ((or (job-terminator? arg) (sh-job? arg))
          (%again (cons arg jobs) (cdr args)))
        ((redirection-sym? arg)
          (%assert-last-is-job jobs arg children-jobs-with-redirections-colon-ampersand)
          (let-values (((fd dir to) (parse-redirection2 args procedure?)))
            ; modify last job in-place
            (sh-redirect! (car jobs) fd dir to)
            (%again jobs (cddr args))))
        ((fixnum? arg)
          (%assert-last-is-job jobs arg children-jobs-with-redirections-colon-ampersand)
          (let-values (((fd dir to) (parse-redirection3 args procedure?)))
            ; modify last job in-place
            (sh-redirect! (car jobs) fd dir to)
            (%again jobs (cdddr args))))
        (#t
          (raise-errorf 'sh-list* "expecting job, redirection or ; &, found:"
            arg children-jobs-with-redirections-colon-ampersand))))))


(define (%assert-last-is-job jobs arg all-args)
  (when (or (null? jobs) (not (sh-job? (car jobs))))
    (raise-errorf 'sh-list* "redirections are allowed only after a job, found:"
      arg all-args)))


) ; close library
