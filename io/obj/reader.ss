;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file io/obj/obj.ss

(define-record-type obj-reader
  (fields
    get-proc
    (mutable close-proc) ; #f or procedure
    (mutable eof?))      ; boolean
  (protocol
    (lambda (new)
      (lambda (get-proc close-proc)
        (%make-obj-reader new get-proc close-proc))))
  (nongenerative %obj-reader-7c46d04b-34f4-4046-b5c7-b63753c1be39))


;; called internally by make-obj-reader: create and return a obj-reader
(define (%make-obj-reader new get-proc close-proc)
  (assert* 'make-obj-reader (procedure? get-proc))
  (assert* 'make-obj-reader (logbit? 1 (procedure-arity-mask get-proc)))
  (when close-proc
    (assert* 'make-obj-reader (procedure? close-proc))
    (assert* 'make-obj-reader (logbit? 1 (procedure-arity-mask close-proc))))
  (new get-proc close-proc #f))


;; call (get-proc p) to generate one more value and return it.
;; each call will return two values:
;;  either (values elem truish) i.e. the next generate value,
;;  or (values #<unspecified> #f) when the generator is exhausted or after (obj-reader-close p) is called.
(define (obj-reader-get p)
  (assert* 'obj-reader-get (obj-reader? p))
  (if (obj-reader-eof? p)
    (values #f #f)
    (let-values (((obj ok?) ((obj-reader-get-proc p) p)))
      (unless ok?
        (obj-reader-eof?-set! p #t))
      (values obj ok?))))


;; call (close-proc p) to release any resource held by the obj-reader.
;; return unspecified value.
;;
;; further calls to (obj-reader-close p) on the same p have no effect.
(define (obj-reader-close p)
  (assert* 'obj-reader-close (obj-reader? p))
  (let ((close-proc (obj-reader-close-proc p)))
    (when close-proc
      (close-proc p)
      (obj-reader-close-proc-set! p #f)))
  (obj-reader-eof?-set! p #t))


;; create and return a obj-reader that generates always the same value.
;; each call to (obj-reader-get p) will return two values:
;;  either (values const truish) i.e. the next element, which is always eq? to const
;;  or (values #<unspecified> #f) after (obj-reader-close p) is called.
;;
;; note: this generator is unlimited, and stops generating values only if (obj-reader-close p) is called.
(define (constant-reader const)
  (let ((%constant-reader ;; name shown when displaying the closure
          (lambda (p) const)))
    (make-obj-reader %constant-reader #f)))


;; create and return a obj-reader that generates the elements of specified list.
;; each call to (obj-reader-get p) will return two values:
;;  either (values elem truish) i.e. the next element from the list
;;  or (values #<unspecified> #f) when the list is exhausted or after (obj-reader-close p) is called.
(define (list-reader l)
  (let ((%list-reader ;; name shown when displaying the closure
          (lambda (p)
            (if (null? l)
              (values #f #f)
              (let ((elem (car l)))
                (set! l (cdr l))
                (values elem #t))))))
    (make-obj-reader %list-reader #f)))


;; create and return a obj-reader that generates the elements of specified vector.
;; each call to (obj-reader-get p) will return two values:
;;  either (values elem truish) i.e. the next element from the vector
;;  or (values #<unspecified> #f) when the vector is exhausted or after (obj-reader-close p) is called.
(define vector-reader
  (case-lambda
    ((v start end)
      (assert* 'vector-reader (fx<=?* 0 start end (vector-length v)))
      (let ((%vector-reader ;; name shown when displaying the closure
              (lambda (p)
                (if (fx>=? start end)
                  (values #f #f)
                  (let ((elem (vector-ref v start)))
                    (set! start (fx1+ start))
                    (values elem #t))))))
        (make-obj-reader %vector-reader #t)))
    ((v)
      (vector-reader v 0 (vector-length v)))))


;; create and return a obj-reader that generates the elements of specified sequence, one at time.
;; each call to (obj-reader-get p) will return two values:
;;  either (values elem truish) i.e. the next element from the sequence
;;  or (values #<unspecified> #f) when the sequence is exhausted or after (obj-reader-close p) is called.
;;
;; This function effectively converts a sequence to a obj-reader.
(define (sequence-reader seq)
  (assert* 'sequence-reader (procedure? seq))
  (assert* 'sequence-reader (logbit? 0 (procedure-arity-mask seq)))
  (let ((%sequence-reader ;; name shown when displaying the closure
          (lambda (p) (seq))))
    (make-obj-reader %sequence-reader #f)))


;; create and return a closure that accepts zero arguments and, at each call,
;; will return two values:
;;   either (values elem truish) i.e. the next generated value,
;;   or (values #<unspecified> #f) if obj-reader is exhausted or after (obj-reader-close p) is called.
;;
;; This function effectively converts a obj-reader to a sequence.
(define (in-reader p)
  (assert* 'in-reader (obj-reader? p))
  (lambda ()
    (obj-reader-get p)))
