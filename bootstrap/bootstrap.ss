;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k bootstrap (0 9 2))
  (export
      ;; bootstrap.ss
      ==> ;; _ is already exported by (rnrs)
      assert* assert-not* catch check check-not define-macro debugf debugf-port
      first-value first-value-or-void forever let-macro raise-assert* repeat second-value
      with-locked-objects while until with-while-until
      throws? trace-call trace-define try list->values values->list

      ;; functions.ss
      check-interrupts eval-form fx<=?* nop parameter-swapper
      generate-pretty-temporaries generate-pretty-temporary gensym-pretty

      raise-assert0 raise-assert1 raise-assert2 raise-assert3
      raise-assert4 raise-assert5 raise-assertf raise-assertl raise-errorf

      warn-check-failed0 warn-check-failed1 warn-check-failed2 warn-check-failed3
      warn-check-failed4 warn-check-failed5 warnf warn-check-failedl

      sh-make-parameter sh-make-thread-parameter sh-make-volatile-parameter sh-version sh-version-number)
  (import
    (rnrs)
    (rnrs exceptions)
    (rnrs mutable-pairs)
    (only (chezscheme) append! console-error-port current-time disable-interrupts enable-interrupts
                       format foreign-procedure fx1+ fx1- fx/ gensym list-copy list-head lock-object
                       meta pariah reverse! time-second time-nanosecond unlock-object void)
    (scheme2k bootstrap arrow)
    (scheme2k bootstrap functions))



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

;; evaluate expr, which may return multiple values, and return the second of such values.
(define-syntax second-value
  (syntax-rules ()
    ((_ expr)
      (call-with-values (lambda () expr) (lambda args (cdr args))))))


;; port where to write debug messages with (debugf).
(define (debugf-port)
  (console-error-port))

#|
(define debugf-port
  (let ((port #f))
    (lambda ()
      (unless port
        ; works, but leaks into child processes :(
        (set! port (open-file-output-port
                     "/dev/pts/1"
                     (file-options no-create no-truncate)
                     (buffer-mode line)
                     (make-transcoder (utf-8-codec) (eol-style lf)
                                      (error-handling-mode raise)))))
      port)))
|#

(define c-pid-get (foreign-procedure "c_pid_get" () int))

;; write a debug message to (debugf-port)
(define (debugf format-string . args)
  (let* ((out (debugf-port))
         (t (current-time 'time-monotonic))
         (us-str (number->string (fx/ (time-nanosecond t) 1000)))
         (us-str-len (string-length us-str))
         (zeropad-time (if (fx<? us-str-len 6) (make-string (fx- 6 us-str-len) #\0) "")))
    (apply format out (string-append "; [pid ~a] ~a.~a~a " format-string "\n")
      (c-pid-get) (time-second t) zeropad-time us-str args)
    (flush-output-port out)))


;; Expands to the correct (raise-assert...) depending on the number of arguments
(define-syntax raise-assert*
  (syntax-rules ()
    ((_ caller form)
      (pariah (raise-assert0 caller form) (void)))
    ((_ caller form arg1)
      (pariah (raise-assert1 caller form arg1) (void)))
    ((_ caller form arg1 arg2)
      (pariah (raise-assert2 caller form arg1 arg2) (void)))
    ((_ caller form arg1 arg2 arg3)
      (pariah (raise-assert3 caller form arg1 arg2 arg3) (void)))
    ((_ caller form arg1 arg2 arg3 arg4)
      (pariah (raise-assert4 caller form arg1 arg2 arg3 arg4) (void)))
    ((_ caller form arg1 arg2 arg3 arg4 arg5)
      (pariah (raise-assert5 caller form arg1 arg2 arg3 arg4 arg5) (void)))
    ((_ caller form args ...)
      (pariah (raise-assertl caller form (list args ...) (void))))))


;; alternative implementation of (assert (proc arg ...))
;; producing more detailed error messages.
;; requires proc to be a procedure, NOT a syntax or macro
(define-syntax assert*
  (lambda (stx)
    (let ((form (format #f "~s" (caddr (syntax->datum stx)))))
      (syntax-case stx ()
        ((_ caller (proc args ...))
          (with-syntax (((targs ...) (generate-pretty-temporaries #'(args ...))))
            (if (symbol? (syntax->datum #'proc))
              #`(let ((targs args) ...) ;; proc is a symbol, no need to save its value into a local variable
                  (if (proc targs ...)
                    (void)
                    (raise-assert* caller #,form targs ...)))
              #`(let ((tproc proc) (targs args) ...)
                  (if (tproc targs ...)
                    (void)
                    (raise-assert* caller #,form targs ...))))))
        ((_ caller expr)
          #`(let ((texpr expr))
              (if texpr
                  (void)
                  (raise-assert* caller #,form texpr))))))))


;; alternative implementation of (assert (not (proc arg ...)))
;; producing more detailed error messages.
;; requires proc to be a procedure, NOT a syntax or macro
(define-syntax assert-not*
  (lambda (stx)
    (let ((form (format #f "(not ~s)" (caddr (syntax->datum stx)))))
      (syntax-case stx ()
        ((_ caller (proc args ...))
          (with-syntax (((targs ...) (generate-pretty-temporaries #'(args ...))))
            #`(let ((tproc proc) (targs args) ...)
                (if (tproc targs ...)
                  (raise-assert* caller #,form targs ...)
                  (void)))))
        ((_ caller expr)
          #`(let ((texpr expr))
              (if texpr
                  (raise-assert* caller #,form texpr)
                  (void))))))))



;; Expands to the correct (warn-check-failed...) depending on the number of arguments
(define-syntax warn-check-failed
  (syntax-rules ()
    ((_ caller form)
      (pariah (warn-check-failed0 caller form) (void)))
    ((_ caller form arg1)
      (pariah (warn-check-failed1 caller form arg1) (void)))
    ((_ caller form arg1 arg2)
      (pariah (warn-check-failed2 caller form arg1 arg2) (void)))
    ((_ caller form arg1 arg2 arg3)
      (pariah (warn-check-failed3 caller form arg1 arg2 arg3) (void)))
    ((_ caller form arg1 arg2 arg3 arg4)
      (pariah (warn-check-failed4 caller form arg1 arg2 arg3 arg4) (void)))
    ((_ caller form arg1 arg2 arg3 arg4 arg5)
      (pariah (warn-check-failed5 caller form arg1 arg2 arg3 arg4 arg5) (void)))
    ((_ caller form args ...)
      (pariah (warn-check-failedl caller form (list args ...) (void))))))


(define-syntax check
  (lambda (stx)
    (syntax-case stx ()
      ((_ caller expr)
       #'(void)))))

(define-syntax check-not
  (lambda (stx)
    (syntax-case stx ()
      ((_ caller expr)
       #'(void)))))


;; display a warning message if (proc arg ...) evaluates to #f
;; requires proc to be a procedure, NOT a syntax or macro
(define-syntax check.saved
  (lambda (stx)
    (let ((form (format #f "~s" (caddr (syntax->datum stx)))))
      (syntax-case stx ()
        ((_ caller (proc args ...))
          (with-syntax (((targs ...) (generate-pretty-temporaries #'(args ...))))
            #`(let ((tproc proc) (targs args) ...)
                (if (tproc targs ...)
                  (void)
                  (warn-check-failed caller #,form targs ...)))))
        ((_ caller expr)
          #`(let ((texpr expr))
              (if texpr
                  (void)
                  (warn-check-failed caller #,form texpr))))))))


;; display a warning message if (proc arg ...) evaluates to truish
;; requires proc to be a procedure, NOT a syntax or macro
(define-syntax check-not.saved
  (lambda (stx)
    (let ((form (format #f "(not ~s)" (caddr (syntax->datum stx)))))
      (syntax-case stx ()
        ((_ caller (proc args ...))
          (with-syntax (((targs ...) (generate-pretty-temporaries #'(args ...))))
            #`(let ((tproc proc) (targs args) ...)
                (when (tproc targs ...)
                  (warn-check-failed caller #,form targs ...)))))
        ((_ caller expr)
          #`(let ((texpr expr))
              (when texpr
                  (warn-check-failed caller #,form texpr))))))))


;; wrap a procedure call, and write two debug messages to (debugf-port):
;; the first before calling the procedure, showing the arguments values
;; the second after the procedure returned, showing the return values
(define-syntax trace-call
  (syntax-rules ()
    ((_ (proc args ...))
       (let ((tproc proc) ; proc must be evaluated before args
             (targs (list args ...)))
         (begin (debugf "-> ~s args ~s" 'proc targs))
         (let-values ((rets (apply tproc targs)))
           (begin (debugf "<- ~s rets ~s args ~s" 'proc rets targs))
           (list->values rets))))))


;; wrap a procedure definition, and write two debug messages to (debugf-port)
;; each time the procedure is calles:
;; the first when entering the procedure, showing the arguments values
;; the second when returning from the procedure, showing the return values
(define-syntax trace-define
  (syntax-rules ()
    ((_ (name arg ...) body1 body2 ...)
      (define name
        (lambda (arg ...)
          (begin (debugf "-> ~s        \targs ~s" 'name (list arg ...)))
          (let ((rets (values->list (begin body1 body2 ...))))
            (begin (debugf "<- ~s rets ~s\targs ~s" 'name rets (list arg ...)))
            (list->values rets)))))))


;; version of (begin) that also accepts empty body
(define-syntax begin^
  (syntax-rules ()
    ((_)                 (void))
    ((_ body)            body)
    ((_ body1 body2 ...) (begin body1 body2 ...))))


(define-syntax with-while-until
  (syntax-rules (while until)
    ((_)
      (void))
    ((_ body1)
      body1)
    ((_ body1 body2)
      (begin body1 body2))
    ((_ while pred body1 body2 ...)
      (when pred (with-while-until body1 body2 ...)))
    ((_ until pred body1 body2 ...)
      (unless pred (with-while-until body1 body2 ...)))
    ((_ body1 body2 body3 ...)
      (begin body1 (with-while-until body2 body3 ...)))))


(define-syntax forever
  (syntax-rules ()
    ((_ body ...)  (let %forever ()
                     (with-while-until
                       body ... (%forever))))))


(define-syntax repeat
  (syntax-rules ()
    ((_ n body ...) (let %repeat ((i (fxmax 0 n)))
                      (unless (fxzero? i)
                        (with-while-until
                          body ... (%repeat (fx1- i))))))))


(define-syntax while
  (syntax-rules ()
    ((_ pred body ...) (let %while ()
                         (when pred
                           (with-while-until
                             body ... (%while)))))))


(define-syntax until
  (syntax-rules ()
    ((_ pred body ...) (let %until ()
                         (unless pred
                           (with-while-until
                             body ... (%until)))))))


(define-syntax try
  (syntax-rules (catch)
    ((_ try-body1 try-body2 ... (catch (exception) catch-body ...))
      (call/cc
        (lambda (k-exit)
          (with-exception-handler
            (lambda (exception)
              (k-exit (begin^ catch-body ...)))
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


(define-syntax with-locked-objects
  (syntax-rules ()
    ((_ (obj1 obj2 ...) body1 body2 ...)
      (dynamic-wind
        (lambda () (lock-object obj1) (lock-object obj2) ...)
        (lambda () body1 body2 ...)
        (lambda () (unlock-object obj1) (unlock-object obj2) ...)))))




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


;; symplify procedure chaining, allows writing (==> proc1 a => proc2 _ b c => proc3 d ...)
;; instead of (proc3 d ... (proc2 (proc1 a) b c))
(define-syntax ==>
  (lambda (stx)
    (syntax-case stx ()
      ((xname . args)
        (datum->syntax #'xname (expand==> (syntax->datum #'args)))))))



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
