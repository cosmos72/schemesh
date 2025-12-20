;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k containers macros (0 9 2))
  (export
    begin^ for for* if^ let^ let-values^ unless^ when^)
  (import
    (rnrs)
    (only (chezscheme) void)
    (only (scheme2k bootstrap) generate-pretty-temporaries with-while-until))


;; extended (begin body ...) that also accepts empty body
(define-syntax begin^
  (syntax-rules ()
    ((_)          (void))
    ((_ body)     body)
    ((_ body ...) (begin body ...))))


;; extended (if expr then else) that also accepts empty then and else
(define-syntax if^
  (syntax-rules ()
    ((_ expr)           (begin expr (void)))
    ((_ expr then)      (if expr then (void)))
    ((_ expr then else) (if expr then else))))


;; extended (unless expr body ...) that also accepts empty body
(define-syntax unless^
  (syntax-rules ()
    ((_ expr)          (begin expr (void)))
    ((_ expr body ...) (if expr (void) (begin^ body ...)))))


;; extended (when expr body ...) that also accepts empty body
(define-syntax when^
  (syntax-rules ()
    ((_ expr)          (begin expr (void)))
    ((_ expr body ...) (if expr (begin^ body ...) (void)))))


;; extended (let ((var expr) ...) body ...) that also accepts empty body
(define-syntax let^
  (syntax-rules ()
    ((_ () body ...)
      (begin^ body ...))
    ((_ ((var expr) ...))
      (let ((var expr) ...)
        (void)))
    ((_ ((var expr) ...) body ...)
      (let ((var expr) ...)
        body ...))))


;; extended (let-values (((var ...) expr) ...) body ...)
;; that optimizes single-value bindings and also accepts empty body
(define-syntax let-values^
  (syntax-rules ()
    ((_ () body ...)
      (begin^ body ...))
    ((_ (((var) expr) more-vars ...) body ...)
      (let ((var expr))
        (let-values^ (more-vars ...)
          body ...)))
    ((_ (((var vars ...) expr) more-vars ...) body ...)
      (let-values (((var vars ...) expr))
        (let-values^ (more-vars ...)
          body ...)))))


(define-syntax %for-body
  (syntax-rules ()
    ((_ for-loop () body ...)
      (with-while-until body ... (for-loop)))
    ((_ for-loop ((vars ... flag iter) more-vars ...) body ...)
      (let-values^ (((vars ... flag) (iter)))
        (when^ flag
          (%for-body for-loop (more-vars ...)
            body ...))))))


(define-syntax %for-sequence
  (syntax-rules ()
    ((_ () (bind ...) body ...)
      (let for-loop ()
        (%for-body for-loop (bind ...) body ...)))
    ;; Racket-compatible syntax: (for (((var ...) sequence)) body ...)
    ((_ (((var ...) sequence) clause2 ...) (bind ...) body ...)
      (%for-sequence ((var ... sequence)) (bind ...) body ...))
    ;; Simplified syntax: (for ((var ... sequence)) body ...)
    ((_ ((var ... sequence) clause2 ...) (bind ...) body ...)
      (let ((iter sequence))
        (%for-sequence (clause2 ...) (bind ... (var ... flag iter)) body ...)))))


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
      (%for-sequence (clause ...) () body ...))))



(define-syntax %for*-inner-part
  (syntax-rules ()
    ((_ () body ...)
      (begin^ body ...))
    ((_ ((vars ... flag iter)) body ...)
      (let-values^ (((vars ... flag) (iter)))
        (when^ flag
          body ...)))
    ((_ ((vars ... flag iter) (vars2 ... flag2 iter2) ...) body ...)
      (let-values^ (((vars ... flag) (iter)))
        (when^ flag
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
        (with-syntax (((flag ...) (generate-pretty-temporaries #'(iterator ...))))
          (with-syntax (((iter ...) (generate-pretty-temporaries #'(iterator ...))))
            #`(let ((iter iterator) ...)
                (let for*-loop ()
                  (%for*-inner-part ((vars ... flag iter) ...)
                    (with-while-until
                      body ...
                      (for*-loop)))))))))))

) ; close library
