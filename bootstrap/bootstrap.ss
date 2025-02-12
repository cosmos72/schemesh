;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh bootstrap (0 7 4))
  (export
      ;; assert.ss
      raise-assert0 raise-assert1 raise-assert2 raise-assert3 raise-assert4 raise-assert5
      raise-assertf raise-assertl raise-errorf

      ;; parameters.ss
      sh-make-parameter sh-make-thread-parameter sh-version

      ;; bootstrap.ss
      assert* catch define-macro debugf debugf-port first-value first-value-or-void let-macro
      raise-assert* repeat second-value while until throws? trace-call try list->values values->list -> ^)

  (import
    (rnrs)
    (rnrs base)
    (rnrs exceptions)
    (only (chezscheme) current-time eval-when format foreign-procedure fx1- fx/ gensym
                       meta pariah reverse! time-second time-nanosecond void)
    (schemesh bootstrap raise)
    (schemesh bootstrap parameters))



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
    ((_ expr) (call-with-values (lambda () expr) (lambda args (cdr args))))))

;; port where to write debug messages with (debugf).
;; lazily initialized to a file output port that writes to device /dev/pts/1
(define debugf-port
  (let ((pts1 #f))
    (lambda ()
      (unless pts1
        ; works, but leaks into child processes :(
        (set! pts1 (open-file-output-port
                     "/dev/pts/1"
                     (file-options no-create no-truncate)
                     (buffer-mode line)
                     (make-transcoder (utf-8-codec) (eol-style lf)
                                      (error-handling-mode raise)))))
      pts1)))

(define c-pid-get (foreign-procedure "c_pid_get" () int))

;; write a debug message to (debugf-port)
(define (debugf format-string . args)
  (let* ((out (debugf-port))
         (t (current-time 'time-monotonic))
         (us-str (number->string (fx/ (time-nanosecond t) 1000)))
         (us-str-len (string-length us-str))
         (zeropad-time (if (fx<? us-str-len 6) (make-string (fx- 6 us-str-len) #\0) "")))
    (apply format out (string-append "; [pid ~a] ~a.~a~a " format-string "\n")
      (c-pid-get) (time-second t) zeropad-time us-str args)))


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
          (with-syntax (((targs ...) (generate-temporaries #'(args ...))))
            #`(let ((tproc proc) (targs args) ...)
                (if (tproc targs ...)
                  (void)
                  (raise-assert* caller #,form targs ...)))))
        ((_ caller expr)
          #`(let ((texpr expr))
              (if texpr
                  (void)
                  (raise-assert* caller #,form texpr))))))))


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
