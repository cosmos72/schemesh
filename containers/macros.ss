;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers macros (0 7 5))
  (export
    for for*)
  (import
    (rnrs)
    (only (schemesh bootstrap) generate-pretty-temporaries))


(define-syntax %for-inner-part
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((vars ... flag iter) ...) body1 body2 ...)
        #`(let-values (((vars ... flag) (iter)) ...)
            (when (and flag ...)
              body1 body2 ...))))))


(define-syntax %for*-inner-part
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body1 body2 ...)
        #`(begin body1 body2 ...))
      ((_ ((vars ... flag iter)) body1 body2 ...)
        #`(let-values (((vars ... flag) (iter)))
            (when flag
              body1 body2 ...)))
      ((_ ((vars ... flag iter) (vars2 ... flag2 iter2) ...) body1 body2 ...)
        #`(let-values (((vars ... flag) (iter)))
            (when flag
              (%for*-inner-part ((vars2 ... flag2 iter2) ...)
                body1 body2 ...)))))))


;; repeatedly call (begin body1 body2 ...) in a loop,
;; with vars bound to successive elements produced by corresponding iterators.
;;
;; the loop finishes when some iterator reaches reach its end.
;;
;; typical iterators expressions are (in-list ...) (in-vector ...) (in-hashtable ...) etc.
(define-syntax for
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((vars ... iterator) ...) body1 body2 ...)
        (with-syntax (((flag ...) (generate-pretty-temporaries #'(iterator ...))))
          (with-syntax (((iter ...) (generate-pretty-temporaries #'(iterator ...))))
            #`(let ((iter iterator) ...)
                (let for-loop ()
                  (%for-inner-part ((vars ... flag iter) ...)
                    body1 body2 ...
                    (for-loop))))))))))


;; repeatedly call (begin body1 body2 ...) in a loop,
;; with vars bound to successive elements produced by corresponding iterators.
;;
;; the loop finishes when some iterator reaches its end.
;;
;; typical iterators expressions are (in-list ...) (in-vector ...) (in-hashtable ...) etc.
;;
;; the only difference between (for) and (for*) is:
;;   (for) evaluates all (iterator) in parallel, then checks if some of them reached their end.
;;   (for*) evaluates each (iterator) one by one, and immediately checks if it reached its end:
;;          in such case, the remaining iterators are not evaluated.
(define-syntax for*
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((vars ... iterator) ...) body1 body2 ...)
        (with-syntax (((flag ...) (generate-pretty-temporaries #'(iterator ...))))
          (with-syntax (((iter ...) (generate-pretty-temporaries #'(iterator ...))))
            #`(let ((iter iterator) ...)
                (let for*-loop ()
                  (%for*-inner-part ((vars ... flag iter) ...)
                    body1 body2 ...
                    (for*-loop))))))))))

) ; close library
