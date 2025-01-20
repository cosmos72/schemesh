;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh bootstrap)
  (export
     assert* catch define-macro debugf debugf-port first-value first-value-or-void
     let-macro raise-assertv raise-assertf raise-errorf repeat
     sh-eval sh-eval-string sh-scheme-environment sh-scheme-eval
     while until throws? trace-call try list->values values->list -> ^)
  (import
    (rnrs)
    (rnrs base)
    (rnrs exceptions)
    (only (chezscheme) current-time environment? environment-mutable? eval format fx1- fx/ gensym
                       interaction-environment logbit? make-format-condition make-thread-parameter
                       meta procedure-arity-mask reverse! syntax-error
                       time-second time-nanosecond void))


(define (sh-eval-string str)
  (sh-eval (read (open-string-input-port str))))

(define sh-eval
  (case-lambda
    ((form)     ((sh-scheme-eval) form (sh-scheme-environment)))
    ((form env) ((sh-scheme-eval) form env))))


;; Thread parameter containing the eval function to use.
;; Will be called as ((sh-scheme-eval) obj environment).
;; Initially set to Chez Scheme's eval, because it can also create definitions.
(define sh-scheme-eval
  (make-thread-parameter
    eval
    (lambda (proc)
      (unless (procedure? proc)
        (raise-errorf 'sh-scheme-eval "invalid value, must be a procedure: ~s" proc))
      (unless (logbit? 1 (procedure-arity-mask proc))
        (raise-errorf 'sh-scheme-eval "invalid procedure, must accept 2 arguments: ~s" proc))
      proc)))


;; Thread parameter containing the scheme enviroment where to eval forms - usually with (sh-eval)
;;
;; Initially set to Chez Scheme's (interaction-environment), because it's mutable
;; and contains all r6rs and chezscheme bindings.
(define sh-scheme-environment
  (make-thread-parameter
    (interaction-environment)
    (lambda (env)
      (unless (environment? env)
        (raise-errorf 'sh-scheme-environment "invalid value, must be an environment: ~s" env))
      (unless (environment-mutable? env)
        (raise-errorf 'sh-scheme-environment "invalid environment, must be mutable: ~s" env))
      env)))


;; convert a list to multiple values
(define (list->values l)
  (apply values l))

;; evaluate expr, which may return multiple values, and return a list containing such values.
(define-syntax values->list
  (syntax-rules ()
    ((_ expr) (call-with-values (lambda () expr) list))))

;; evaluate expr, which may return multiple values, and return the first of such values.
(define-syntax first-value
  (syntax-rules ()
    ((_ expr) (call-with-values (lambda () expr) (lambda args (car args))))))

;; evaluate expr, which may return multiple values, and return the first of such values.
;; If expr returns no values, return (void)
(define-syntax first-value-or-void
  (syntax-rules ()
    ((_ expr) (call-with-values
                (lambda () expr)
                (lambda args (if (null? args) (void) (car args)))))))


;; port where to write debug messages with (debugf).
;; lazily initialized to a file output port that writes to device /dev/pts/1
(define debugf-port
  (let ((pts1 #f))
    (lambda ()
      (unless pts1
        (set! pts1 (open-file-output-port
                     "/dev/pts/1"
                     (file-options no-create no-truncate)
                     (buffer-mode line)
                     (make-transcoder (utf-8-codec) (eol-style lf)
                                      (error-handling-mode raise)))))
      pts1)))


;; write a debug message to (debugf-port)
(define (debugf format-string . args)
  (let* ((out (debugf-port))
         (t (current-time 'time-monotonic))
         (us-str (number->string (fx/ (time-nanosecond t) 1000)))
         (us-str-len (string-length us-str))
         (zeropad (if (fx<? us-str-len 6) (make-string (fx- 6 us-str-len) #\0) "")))
    (apply format out (string-append "; ~a.~a~a " format-string "\n")
      (time-second t) zeropad us-str args)))


;; wrap a procedure call, and write two debug messages to (debugf-port):
;; the first before calling the procedure, showing the arguments values
;; the second after the procedure returned, showing the return values
(define-syntax trace-call
  (lambda (stx)
    (syntax-case stx ()
      ((_ (proc args ...))
        #'(let ((tproc proc) ; proc must be evaluated before args
                (targs (list args ...)))
            (begin (debugf "> ~s call ~s" 'proc (cons 'proc targs)))
            (let ((ret (values->list (apply tproc targs))))
              (begin (debugf "< ~s rets ~s" 'proc ret))
              (list->values ret)))))))


(define-syntax repeat
  (syntax-rules ()
    ((_ n body ...) (do ((i n (fx1- i))) ((fx<=? i 0)) body ...))))

(define-syntax while
  (syntax-rules ()
    ((_ pred)          (do () ((not pred))))
    ((_ pred body ...) (do () ((not pred)) body ...))))

(define-syntax until
  (syntax-rules ()
    ((_ pred)          (do () (pred)))
    ((_ pred body ...) (do () (pred) body ...))))

;; Raise a condition describing an assertion violation.
;; Condition format message and its arguments must be provided by caller.
(define (raise-assertf who format-string . format-args)
  (raise
    (condition
      (make-assertion-violation)
      (make-who-condition who)
      (make-format-condition)
      (make-message-condition format-string)
      (make-irritants-condition format-args))))


;; Raise a condition describing an assertion violation evaluating a form.
;; Condition format message is hardcoded, caller needs to provide:
;; * form - a string containing source code of the failed assertion
;; * form-values - values of each subform in form
(define (raise-assertv who form . form-values)
  (raise-assertf who "failed assertion ~a with arguments ~s" form form-values))


;; Raise a condition describing an error.
;; Condition format message and its arguments must be provided by caller.
(define (raise-errorf who format-string . format-args)
  (raise
    (condition
      (make-error)
      (make-who-condition who)
      (make-format-condition)
      (make-message-condition format-string)
      (make-irritants-condition format-args))))


;; alternative implementation of (assert (proc arg ...))
;; producing more detailed error messages.
;; requires proc to be a procedure, NOT a syntax or macro
(define-syntax assert*
  (lambda (stx)
    (let ((form (lambda ()
                  (format #f "~s" (caddr (syntax->datum stx))))))
      (syntax-case stx ()
        ((_ caller (proc))
          #`(let ((tproc proc))
              (or (tproc)
                  (raise-assertv caller #,(form) tproc))))
        ((_ caller (proc arg1))
          #`(let ((tproc proc)
                  (targ1 arg1))
              (or (tproc targ1)
                  (raise-assertv caller #,(form) tproc targ1))))
        ((_ caller (proc arg1 arg2))
          #`(let ((tproc proc)
                  (targ1 arg1)
                  (targ2 arg2))
              (or (tproc targ1 targ2)
                  (raise-assertv caller #,(form) tproc targ1 targ2))))
        ((_ caller (proc arg1 arg2 arg3))
          #`(let ((tproc proc)
                  (targ1 arg1)
                  (targ2 arg2)
                  (targ3 arg3))
              (or (tproc targ1 targ2 targ3)
                  (raise-assertv caller #,(form) tproc targ1 targ2 targ3))))
        ((_ caller (proc arg1 arg2 arg3 arg4))
          #`(let ((tproc proc)
                  (targ1 arg1)
                  (targ2 arg2)
                  (targ3 arg3)
                  (targ4 arg4))
              (or (tproc targ1 targ2 targ3 targ4)
                  (raise-assertv caller #,(form) tproc targ1 targ2 targ3 targ4))))
        ((_ caller (proc arg ...))
          #`(let ((tproc proc)
                  (targs (list arg ...)))
              (or (apply tproc targs)
                  (apply raise-assertv caller #,(form) tproc targs))))
        ((_ caller expr)
          #`(let ((texpr expr))
              (or texpr (raise-assertv caller #,(form) texpr))))))))

(define-syntax try
  (syntax-rules (catch)
    ((_ try-body1 try-body2 ... (catch (exception) catch-body1 catch-body2 ...))
      (call/cc
        (lambda (k-exit)
          (with-exception-handler
            (lambda (exception)
              (k-exit (begin catch-body1 catch-body2 ...)))
            (lambda ()
              try-body1 try-body2 ...)))))
    ((_ bad-body ...)
      (syntax-violation "" "invalid syntax, expecting (try EXPR ... (catch (IDENT) ...)) in"
        (list 'try (quote bad-body) ...)))))

;; export aux keyword catch, needed by try
(define-syntax catch
  (lambda (arg)
    (syntax-violation "" "misplaced auxiliary keyword" arg)))


(define-syntax throws?
  (syntax-rules ()
    ((_ expr)
      (try
        expr
        #f
        (catch (ex)
          (or ex #t))))))




;; Scheme implementation of Common Lisp defmacro, defines a global macro.
;; Usage:
;;   (define-macro (name arg ...) body)
;; or:
;;   (define-macro (name . args) body)
;; or:
;;   (define-macro name (lambda (arg ...) body))
;; or:
;;   (define-macro name (lambda args body))
;; then, each subsequent occurrence of (name expr ...)
;; evaluates body at macroexpansion time,
;; with each arg bound to the corresponding UNEVALUATED expr,
;; and returned value must be a list containing source code
;; that will be compiled in place of (name expr ...)
(define-syntax define-macro
  (syntax-rules ()
    ((_ (name . args) body ...)
     (define-macro name (lambda args body ...)))
    ((_ name transformer)
     (define-syntax name
       (lambda (stx)
         (syntax-case stx ()
           ((xname . args)
             (datum->syntax (syntax xname)
               (apply transformer (syntax->datum (syntax args)))))))))))


;; Scheme implementation of Common Lisp macrolet, defines a local macro.
;; Usage:
;;   (let-macro ((name arg ...) body)
;;      forms ...)
;; or:
;;   (let-macro ((name . args) body)
;;      forms ...)
;; or:
;;   (let-macro name (lambda (arg ...) body)
;;      forms ...)
;; or:
;;   (let-macro name (lambda args body)
;;      forms ...)
;; then, each occurrence of (name expr ...) inside forms - and only there -
;; evaluates body at macroexpansion time,
;; with each arg bound to the corresponding UNEVALUATED expr,
;; and returned value must be a list containing source code
;; that will be compiled in place of (name expr ...)
(define-syntax let-macro
  (syntax-rules ()
    ((_ ((name . args) body1 body2 ...) form1 form2 ...)
     (let-macro name (lambda args body1 body2 ...) form1 form2 ...))
    ((_ name transformer form1 form2 ...)
     (let-syntax ((name
       (lambda (stx)
         (syntax-case stx ()
           ((xname . args)
             (datum->syntax (syntax xname)
               (apply transformer (syntax->datum (syntax args)))))))))
       form1 form2 ...))))



(meta begin
  ;; helper function used by ->expand
  ;; traverse list, replacing the first object eq? to old with the object new.
  ;; does NOT traverse sublists.
  (define (replace-first new old l)
    (let %again ((tail l)
                 (ret  '())
                 (replace? #t))
      (cond
        ((null? tail)
          (reverse! ret))
        ((and replace? (eq? old (car tail)))
          (%again (cdr tail) (cons new ret) #f))
        (#t
          (%again (cdr tail) (cons (car tail) ret) replace?)))))


  ;; helper function used by -->
  (define (->expand obj accessor)
    (unless (and (pair? accessor) (memq '^ accessor))
      (syntax-violation "" "invalid syntax, missing ^ in nested list after" accessor '->))
    (replace-first obj '^ accessor))


) ; close meta


;; symplify accessors chaining, allows writing (-> obj accessor1 accessor2 ...)
;; instead of (... (accessor2 (accessor1 obj)))
(define-macro (-> . args)
  (when (or (null? args) (not (pair? (cdr args))))
    (syntax-violation "" "invalid syntax, need at least two arguments after" (cons '-> args) '->))
  (let ((obj (car args))
        (accessor0 (cadr args))
        (accessors (cddr args)))
    (if (null? accessors)
      (if (pair? accessor0)
        (->expand obj accessor0)
        (list accessor0 obj))
      `(-> (-> ,obj ,accessor0) . ,accessors))))


;; export aux keyword ^, needed by ->
(define-syntax ^
  (lambda (arg)
    (syntax-violation "" "misplaced auxiliary keyword" arg)))

#|
;; redefine obj as a local macro, simplifying repeated calls to verbose functions
;; with obj as first argument.
;; Usage:
;;   (with-object x function-prefix
;;      forms ...)
;; where x and function-prefix are symbols.
;; then, each occurrence of (x function-suffix args ...) inside forms - and only there -
;; is expanded to (function-name x args ... )
;; where function-name is the concatenation of function-prefix, a literal "-", and function-suffix.
;; Note: x is evaluated only once, immediately before forms ...
;;
;; Example:
;;   (let ((str "foo"))
;;     (string-append str "bar"))
;; can be rewritten as follows using with-object:
;;   (let ((str "foo"))
;;     (with-object str string
;;       (str append "bar")))
;; which is shorter when calling lots of functions whose name starts with "string-"
;; and their first argument is str.
(define-macro (with-object obj type . body)
  (let ((sym (gensym (symbol->string obj))))
    `(let ((,sym ,obj))
       (let-macro ((,obj method . args)
                       (cons* (string->symbol (string-append (symbol->string ',type) "-" (symbol->string method)))
                              ',sym args))
        ,@body))))
|#

) ; close library
