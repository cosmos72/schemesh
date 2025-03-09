;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers macros (0 8 1))
  (export
    begin^ for for* if^ unless^ when^)
  (import
    (rnrs)
    (only (chezscheme) void)
    (only (schemesh bootstrap) generate-pretty-temporaries))


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


(define-syntax %for-inner-part
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((vars ... flag iter) ...) body ...)
        #`(let-values (((vars ... flag) (iter)) ...)
            (when^ (and flag ...)
              body ...))))))


(define-syntax %for*-inner-part
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #`(begin^ body ...))
      ((_ ((vars ... flag iter)) body ...)
        #`(let-values (((vars ... flag) (iter)))
            (when^ flag
              body ...)))
      ((_ ((vars ... flag iter) (vars2 ... flag2 iter2) ...) body ...)
        #`(let-values (((vars ... flag) (iter)))
            (when^ flag
              (%for*-inner-part ((vars2 ... flag2 iter2) ...)
                body ...)))))))


;; repeatedly call (begin body ...) in a loop,
;; with vars bound to successive elements produced by corresponding iterators.
;;
;; the loop finishes when some iterator reaches reach its end.
;;
;; typical iterators expressions are (in-list ...) (in-vector ...) (in-hash ...) etc.
(define-syntax for
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((vars ... iterator) ...) body ...)
        (with-syntax (((flag ...) (generate-pretty-temporaries #'(iterator ...))))
          (with-syntax (((iter ...) (generate-pretty-temporaries #'(iterator ...))))
            #`(let ((iter iterator) ...)
                (let for-loop ()
                  (%for-inner-part ((vars ... flag iter) ...)
                    body ...
                    (for-loop))))))))))


;; repeatedly call (begin body ...) in a loop,
;; with vars bound to successive elements produced by corresponding iterators.
;;
;; the loop finishes when some iterator reaches its end.
;;
;; typical iterators expressions are (in-list ...) (in-vector ...) (in-hash ...) etc.
;;
;; the only difference between (for) and (for*) is:
;;   (for) evaluates all (iterator) in parallel, then checks if some of them reached their end.
;;   (for*) evaluates each (iterator) one by one, and immediately checks if it reached its end:
;;          in such case, the remaining iterators are not evaluated.
(define-syntax for*
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((vars ... iterator) ...) body ...)
        (with-syntax (((flag ...) (generate-pretty-temporaries #'(iterator ...))))
          (with-syntax (((iter ...) (generate-pretty-temporaries #'(iterator ...))))
            #`(let ((iter iterator) ...)
                (let for*-loop ()
                  (%for*-inner-part ((vars ... flag iter) ...)
                    body ...
                    (for*-loop))))))))))

) ; close library
