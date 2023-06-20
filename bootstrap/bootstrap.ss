;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh bootstrap)
  (export eval-string repeat while until try list->values values->list define-macro)
  (import
    (rnrs)
    ; Unlike R6RS (eval obj environment), Chez Scheme's (eval obj)
    ; uses interaction-environment and can modify it
    (only (chezscheme) eval void))

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

(define-syntax try
  (syntax-rules (catch)
    ((_ try-body (catch (cond) handler-form ...))
      (call/cc
        (lambda (k-exit)
          (with-exception-handler
            (lambda (cond)
              (k-exit (begin handler-form ...)))
            (lambda ()
              try-body)))))))

(define (list->values l)
  (apply values l))

(define-syntax values->list
  (syntax-rules ()
    ((_ pred)    (call-with-values (lambda () pred) list))))

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

) ; close library
