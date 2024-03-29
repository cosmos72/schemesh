;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh bootstrap)
  (export
     assert* catch debugf eval-string repeat while until try list->values values->list
     define-macro let-macro)
  (import
    (rnrs)
    (rnrs base)
    (rnrs exceptions)
    ; Unlike R6RS (eval obj environment), Chez Scheme's (eval obj)
    ; uses interaction-environment and can modify it
    (only (chezscheme) eval format gensym syntax-error void))


(define debugf
  (let ((pts1 #f))
    (lambda (format-string . args)
      (unless pts1
        (set! pts1 (open-file-output-port
                     "/dev/pts/1"
                     (file-options no-create no-truncate)
                     (buffer-mode none)
                     (make-transcoder (utf-8-codec) (eol-style lf)
                                      (error-handling-mode raise)))))
      (apply format pts1 format-string args)
      (flush-output-port pts1))))


(define (eval-string str)
  (eval (read (open-string-input-port str))))

(define-syntax repeat
  (syntax-rules ()
    ((_ n body ...) (do ((i n (fx- i 1))) ((fx<=? i 0)) body ...))))

(define-syntax while
  (syntax-rules ()
    ((_ pred)          (do () ((not pred))))
    ((_ pred body ...) (do () ((not pred)) body ...))))

(define-syntax until
  (syntax-rules ()
    ((_ pred)          (do () (pred)))
    ((_ pred body ...) (do () (pred) body ...))))

;; alternative implementation of (assert (proc arg ...))
;; requires proc to be a procedure, NOT a syntax or macro
(define-syntax assert*
  (lambda (x)
    (let ((msg (lambda ()
                 (format #f "failed assertion ~s" (cadr (syntax->datum x))))))
      (syntax-case x ()
        ((_ (proc))
          #`(let ((tproc proc))
              (or (tproc)
                  (assertion-violation #f #,(msg) tproc))))
        ((_ (proc arg1))
          #`(let ((tproc proc)
                  (targ1 arg1))
              (or (tproc targ1)
                  (assertion-violation #f #,(msg) tproc targ1))))
        ((_ (proc arg1 arg2))
          #`(let ((tproc proc)
                  (targ1 arg1)
                  (targ2 arg2))
              (or (tproc targ1 targ2)
                  (assertion-violation #f #,(msg) tproc targ1 targ2))))
        ((_ (proc arg1 arg2 arg3))
          #`(let ((tproc proc)
                  (targ1 arg1)
                  (targ2 arg2)
                  (targ3 arg3))
              (or (tproc targ1 targ2 targ3)
                  (assertion-violation #f #,(msg) tproc targ1 targ2 targ3))))
        ((_ (proc arg ...))
          #`(let ((tproc proc)
                  (targs (list arg ...)))
              (or (apply tproc targs)
                  (apply assertion-violation #f #,(msg) tproc targs))))
        ((_ expr)
          #`(or expr (assertion-violation #f #,(msg))))))))

(define-syntax try
  (syntax-rules (catch)
    ((_ try-body (catch (exception) catcher-form1 catcher-form2 ...))
      (call/cc
        (lambda (k-exit)
          (with-exception-handler
            (lambda (exception)
              (k-exit (begin catcher-form1 catcher-form2 ...)))
            (lambda ()
              try-body)))))
    ((_ bad-form ...)
      (syntax-violation "" "invalid syntax, expecting (try EXPR (catch (IDENT) ...)) in"
        (list 'try (quote bad-form) ...)))))

;; export aux keyword catch, needed by try
(define-syntax catch
  (lambda (arg)
    (syntax-violation "" "misplaced auxiliary keyword" arg)))

(define (list->values l)
  (apply values l))

(define-syntax values->list
  (syntax-rules ()
    ((_ pred)    (call-with-values (lambda () pred) list))))

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
           ((l . sv)
             (let* ((v (syntax->datum (syntax sv)))
                    (e (apply transformer v)))
               (if (eq? (void) e)
                   (syntax (void))
                   (datum->syntax (syntax l) e))))))))))

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
    ((_ ((name . args) body ...) form1 form2 ...)
     (let-macro name (lambda args body ...) form1 form2 ...))
    ((_ name transformer form1 form2 ...)
     (let-syntax ((name
       (lambda (stx)
         (syntax-case stx ()
           ((l . sv)
             (let* ((v (syntax->datum (syntax sv)))
                    (e (apply transformer v)))
               (if (eq? (void) e)
                   (syntax (void))
                   (datum->syntax (syntax l) e))))))))
       form1 form2 ...))))

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
