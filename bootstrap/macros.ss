;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs


;; extended (begin) that also accepts empty body
(define-syntax begin0
  (syntax-rules ()
    ((_)                 (void))
    ((_ body)            body)
    ((_ body1 body2 ...) (begin body1 body2 ...))))


;; extended (unless expr body ...) that also accepts empty body
(define-syntax unless0
  (syntax-rules ()
    ((_ expr)          (begin expr (void)))
    ((_ expr body ...) (if expr (void) (begin0 body ...)))))


;; extended (when expr body ...) that also accepts empty body
(define-syntax when0
  (syntax-rules ()
    ((_ expr)          (begin expr (void)))
    ((_ expr body ...) (if expr (begin0 body ...) (void)))))


;; evaluate body ... n times
(define-syntax repeat
  (syntax-rules ()
    ((_ n body ...) (let %repeat ((i 0) (end n))
                      (when (fx<? i end)
                        body ... (%repeat (fx1+ i) end))))))


;; if pred evaluates to #f, then evaluate body ... and repeat
(define-syntax until
  (syntax-rules ()
    ((_ pred body ...) (let %until ()
                         (unless pred
                           body ... (%until))))))


;; if pred evaluates to truish, then evaluate body ... and repeat
(define-syntax while
  (syntax-rules ()
    ((_ pred body ...) (let %while ()
                         (when pred
                           body ... (%while))))))


;; helper macro, introduces optional early termination if "while expr" or "until expr"
;; appear at top level without parentheses (and without quotes)
(define-syntax with-while-until
  (syntax-rules (while until)
    ((_)
      (begin0))
    ((_ body)
      body)
    ((_ while pred body ...)
      (when0 pred (with-while-until body ...)))
    ((_ until pred body ...)
      (unless0 pred (with-while-until body ...)))
    ((_ body1 body2 ...)
      (begin body1 (with-while-until body2 ...)))))


(define-syntax for-inner-part
  (syntax-rules ()
    ((_ ret () body ...)
      (begin0 body ...))
    ((_ ret ((vars ... flag iter)) body ...)
      (let-values0 (((vars ... flag) (iter)))
        (if flag
          (begin0 body ...)
          ret)))
    ((_ ret ((vars ... flag iter) (vars2 ... flag2 iter2) ...) body ...)
      (let-values0 (((vars ... flag) (iter)))
        (if flag
          (for-inner-part ret ((vars2 ... flag2 iter2) ...)
            body ...)
          ret)))))


;;; Loop in parallel on elements returned by zero or more iterators,
;;; and execute body ... at each iteration, with vars bound to elements returned by iterators.
;;;
;;; If body ... evaluates to #f, the loop finishes and returns #f
;;; Otherwise the loop finishes when some iterator is exhausted,
;;     and returns the value of last body ... evaluation (or #t if body ... was never evaluated).
;;; In such case, the remaining iterators are not called again.
;;;
;;; If no iterators are specified, the loop finishes when body ... evaluates to #f, and returns #f
;;;
;;; Typical iterators expressions are (in-list ...) (in-vector ...) (in-hash ...) etc.
(define-syntax for
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #`(let for-loop0 ()
             (and (begin0 body ...) (for-loop0))))
      ((_ var iterator body ...)
        (identifier? #'var)
        #`(iterate iterator (lambda0 (_ var) body ...)))
      ((_ ((var iterator)) body ...)
        #`(iterate iterator (lambda0 (_ var) body ...)))
      ((_ ((vars ... iterator) ...) body ...)
        (with-syntax (((flag ...) (generate-pretty-temporaries #'(iterator ...)))
                      ((iter ...) (generate-pretty-temporaries #'(iterator ...))))
          #`(let* ((iter iterator) ...)
              (let for-loop ((ret #t))
                (for-inner-part ret ((vars ... flag iter) ...)
                  (let ((ret (begin0 body ...)))
                    (and ret (for-loop ret)))))))))))


;; evaluate body ... repeatedly,
;; with optional early termination if "while expr" or "until expr"
;; appear at top level without parentheses (and without quotes)
(define-syntax forever
  (syntax-rules ()
    ((_ body ...)  (let %forever ()
                     (with-while-until
                       body ... (%forever))))))


;; extended (if expr then else) that also accepts empty then and else
(define-syntax if0
  (syntax-rules ()
    ((_ expr)           (begin expr (void)))
    ((_ expr then)      (if expr then (void)))
    ((_ expr then else) (if expr then else))))


;; extended (lambda args body ...) that also accepts empty body
(define-syntax lambda0
  (syntax-rules ()
    ((_ args)                 (lambda args (void)))
    ((_ args body1 body2 ...) (lambda args body1 body2 ...))))


;; extended (let ((var expr) ...) body ...) that also accepts empty body
(define-syntax let0
  (syntax-rules ()
    ((_ () body ...)
      (begin0 body ...))
    ((_ ((var expr) ...))
      (let ((var expr) ...)
        (void)))
    ((_ ((var expr) ...) body ...)
      (let ((var expr) ...)
        body ...))))


;; specialized (let ((var expr) ...) body ...) that accepts only a single variable
(define-syntax let1
  (syntax-rules ()
    ((_ var expr body ...)
      (let ((var expr))
        body ...))))


;; (let*-pairs0 ((var1 expr1 var2 expr2) (var3 expr3 var4 expr4) ...) body ...)
;; expands to
;; (let* ((var1 expr1)
;;        (var2 expr2))
;;   (let*-pairs0 ((var3 expr3 var4 expr4)...)
;;     body ...))
(define-syntax let*-pairs0
  (syntax-rules ()
    ((_ () body ...)
      (begin0 body ...))
    ((_ ((var1 expr1 var2 expr2) (var3 expr3 var4 expr4) ...) body ...)
      (let* ((var1 expr1)
             (var2 expr2))
        (let*-pairs0 ((var3 expr3 var4 expr4) ...)
          body ...)))))


;; extended (let-values (((var ...) expr) ...) body ...)
;; that optimizes single-value bindings and also accepts empty body
(define-syntax let-values0
  (syntax-rules ()
    ((_ () body ...)
      (begin0 body ...))
    ((_ (((var) expr) more-vars ...) body ...)
      (let ((var expr))
        (let-values0 (more-vars ...)
          body ...)))
    ((_ (((var vars ...) expr) more-vars ...) body ...)
      (let-values (((var vars ...) expr))
        (let-values0 (more-vars ...)
          body ...)))))
