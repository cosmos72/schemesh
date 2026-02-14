;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k containers macros (0 9 3))
  (export
    begin0 for for* if0 lambda0 let0 let*-pairs0 let-values0 unless0 when0)
  (import
    (rnrs)
    (only (chezscheme) void)
    (only (scheme2k bootstrap) generate-pretty-temporaries with-while-until))


;; extended (begin body ...) that also accepts empty body
(define-syntax begin0
  (syntax-rules ()
    ((_)          (void))
    ((_ body)     body)
    ((_ body ...) (begin body ...))))


;; extended (lambda args body ...) that also accepts empty body
(define-syntax lambda0
  (syntax-rules ()
    ((_ args)                 (lambda args (void)))
    ((_ args body1 body2 ...) (lambda args body1 body2 ...))))


;; extended (if expr then else) that also accepts empty then and else
(define-syntax if0
  (syntax-rules ()
    ((_ expr)           (begin expr (void)))
    ((_ expr then)      (if expr then (void)))
    ((_ expr then else) (if expr then else))))


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


(define-syntax %for-body
  (syntax-rules ()
    ((_ for-loop () body ...)
      (with-while-until body ... (for-loop)))
    ((_ for-loop ((vars ... flag iter) more-vars ...) body ...)
      (let-values0 (((vars ... flag) (iter)))
        (when0 flag
          (%for-body for-loop (more-vars ...)
            body ...))))))


(define-syntax %for-iterator
  (syntax-rules ()
    ((_ () (bind ...) body ...)
      (let for-loop ()
        (%for-body for-loop (bind ...) body ...)))
    ;; Racket-compatible syntax: (for (((var ...) iterator)) body ...)
    ((_ (((var ...) iterator) clause2 ...) (bind ...) body ...)
      (%for-iterator ((var ... iterator)) (bind ...) body ...))
    ;; Simplified syntax: (for ((var ... iterator)) body ...)
    ((_ ((var ... iterator) clause2 ...) (bind ...) body ...)
      (let ((iter iterator))
        (%for-iterator (clause2 ...) (bind ... (var ... flag iter)) body ...)))))


;;; Loop in parallel on elements returned by zero or more iterators,
;;; and execute body ... at each iteration, with vars bound to elements returned by the iterators.
;;;
;;; The loop finishes when some iterator is exhausted, and returns unspecified value.
;;;
;;; If no iterators are specified, behave as (forever body ...)
;;;
;;; Typical iterators expressions are (in-list ...) (in-vector ...) (in-hash ...) etc.
;;;
;;; The only difference between (for) and (for*) is:
;;;   (for) evaluates all (iterator) in parallel, then checks if some of them reached their end.
;;;   (for*) evaluates each (iterator) one by one, and immediately checks if it reached its end:
;;;          in such case, the remaining iterators are not evaluated.
(define-syntax for
  (syntax-rules ()
    ((_ (clause ...) body ...)
      (%for-iterator (clause ...) () body ...))))



(define-syntax %for*-inner-part
  (syntax-rules ()
    ((_ () body ...)
      (begin0 body ...))
    ((_ ((vars ... flag iter)) body ...)
      (let-values0 (((vars ... flag) (iter)))
        (when0 flag
          body ...)))
    ((_ ((vars ... flag iter) (vars2 ... flag2 iter2) ...) body ...)
      (let-values0 (((vars ... flag) (iter)))
        (when0 flag
          (%for*-inner-part ((vars2 ... flag2 iter2) ...)
            body ...))))))


;;; Loop in parallel on elements returned by zero or more iterators,
;;; and execute body ... at each iteration, with vars bound to elements returned by iterators.
;;;
;;; The loop finishes when some iterator is exhausted, and returns unspecified value.
;;;
;;; If no iterators are specified, behave as (forever body ...)
;;;
;;; Typical iterators expressions are (in-list ...) (in-vector ...) (in-hash ...) etc.
;;;
;;; The only difference between (for) and (for*) is:
;;;   (for) evaluates all (iterator) in parallel, then checks if some of them reached their end.
;;;   (for*) evaluates each (iterator) one by one, and immediately checks if it reached its end:
;;;          in such case, the remaining iterators are not evaluated.
(define-syntax for*
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((vars ... iterator) ...) body ...)
        (with-syntax (((flag ...) (generate-pretty-temporaries #'(iterator ...)))
                      ((iter ...) (generate-pretty-temporaries #'(iterator ...))))
          #`(let ((iter iterator) ...)
              (let for*-loop ()
                (%for*-inner-part ((vars ... flag iter) ...)
                  (with-while-until
                    body ...
                    (for*-loop))))))))))

) ; close library
