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
    close-box            ; box containing #f or procedure
    (mutable eof?))      ; boolean
  (protocol
    (lambda (new)
      (lambda (get-proc close-proc)
        (%make-obj-reader new get-proc close-proc))))
  (nongenerative %obj-reader-7c46d04b-34f4-4046-b5c7-b63753c1be39))


;; called internally by make-obj-reader: create and return an obj-reader
(define (%make-obj-reader new get-proc close-proc)
  (assert* 'make-obj-reader (procedure? get-proc))
  (assert* 'make-obj-reader (logbit? 1 (procedure-arity-mask get-proc)))
  (when close-proc
    (assert* 'make-obj-reader (procedure? close-proc))
    (assert* 'make-obj-reader (logbit? 1 (procedure-arity-mask close-proc))))
  (new get-proc (box close-proc) #f))


;; call (get-proc rx) to generate one more value and return it.
;; each call will return two values:
;;  either (values elem #t) i.e. the next generate value,
;;  or (values #<unspecified> #f) when the reader is exhausted or after (obj-reader-close rx) has been called.
(define (obj-reader-get rx)
  (assert* 'obj-reader-get (obj-reader? rx))
  (if (obj-reader-eof? rx)
    (values #f #f)
    (let-values (((obj ok?) ((obj-reader-get-proc rx) rx)))
      (unless ok?
        (obj-reader-eof?-set! rx #t))
      (values obj ok?))))


;; call the close-proc stored in obj-reader at its creation,
;; to release any resource held by the obj-reader.
;; return unspecified value.
;;
;; further calls to (obj-reader-close rx) on the same rx have no effect, and do not call close-proc again.
(define (obj-reader-close rx)
  (assert* 'obj-reader-close (obj-reader? rx))
  (obj-reader-eof?-set! rx #t)
  (let* ((close-box  (obj-reader-close-box rx))
         (close-proc (unbox close-box))
         (consumed?  (and close-proc (box-cas! close-box close-proc #f))))
    (when consumed?
      (close-proc rx))))


;; create and return an obj-reader that generates always the same value.
;; each call to (obj-reader-get rx) will return two values:
;;  either (values const #t) i.e. the next element, which is always eq? to const
;;  or (values #<unspecified> #f) after (obj-reader-close rx) has been called.
;;
;; note: this reader is unlimited, and stops generating values only if (obj-reader-close rx) is called.
(define (constant-reader const)
  (let ((%constant-reader ;; name shown when displaying the closure
          (lambda (rx)
            (values const #t))))
    (make-obj-reader %constant-reader #f)))


;; create and return an exhausted obj-reader.
;; each call to (obj-reader-get rx) will always return the two values:
;;  (values #<unspecified> #f) indicating the reader is exhausted.
(define (empty-reader)
  (let ((%empty-reader ;; name shown when displaying the closure
          (lambda (rx)
            (values #f #f))))
    (make-obj-reader %empty-reader #f)))


;; create and return an obj-reader that generates the elements of specified list.
;; each call to (obj-reader-get rx) will return two values:
;;  either (values elem #t) i.e. the next element from the list
;;  or (values #<unspecified> #f) when the list is exhausted or after (obj-reader-close rx) has been called.
;;
;; This function effectively converts a list to an obj-reader, and could reasonably be named `list->reader`
;; although by convention readers are created by functions `TYPE-reader`
(define (list-reader l)
  (let ((%list-reader ;; name shown when displaying the closure
          (lambda (rx)
            (if (null? l)
              (values #f #f)
              (let ((elem (car l)))
                (set! l (cdr l))
                (values elem #t))))))
    (make-obj-reader %list-reader #f)))


;; create and return an obj-reader that generates the elements of specified vector.
;; each call to (obj-reader-get rx) will return two values:
;;  either (values elem #t) i.e. the next element from the vector
;;  or (values #<unspecified> #f) when the vector is exhausted or after (obj-reader-close rx) has been called.
;;
;; This function effectively converts a vector to an obj-reader, and could reasonably be named `vector->reader`
;; although by convention readers are created by functions `TYPE-reader`
(define vector-reader
  (case-lambda
    ((v start end)
      (assert* 'vector-reader (fx<=?* 0 start end (vector-length v)))
      (let ((%vector-reader ;; name shown when displaying the closure
              (lambda (rx)
                (if (fx>=? start end)
                  (values #f #f)
                  (let ((elem (vector-ref v start)))
                    (set! start (fx1+ start))
                    (values elem #t))))))
        (make-obj-reader %vector-reader #t)))
    ((v)
      (vector-reader v 0 (vector-length v)))))


;; create and return an obj-reader that generates the elements of specified unary sequence, one at time.
;; each call to (obj-reader-get rx) will return two values:
;;  either (values elem #t) i.e. the next element from the sequence
;;  or (values #<unspecified> #f) when the sequence is exhausted or after (obj-reader-close rx) has been called.
;;
;; This function effectively converts a sequence to an obj-reader, and could reasonably be named `sequence->reader`
;; although by convention readers are created by functions `TYPE-reader`
(define (sequence-reader seq)
  (assert* 'sequence-reader (procedure? seq))
  (assert* 'sequence-reader (logbit? 0 (procedure-arity-mask seq)))
  (let ((%sequence-reader ;; name shown when displaying the closure
          (lambda (rx) (seq))))
    (make-obj-reader %sequence-reader #f)))


;; create and return a sequence, i.e. a closure that accepts zero arguments and, at each call,
;; will return the two values returned by calling (obj-reader-get rx):
;;   either (values elem #t) i.e. the next generated value,
;;   or (values #<unspecified> #f) if obj-reader is exhausted or after (obj-reader-close rx) has been called.
;;
;; This function effectively converts an obj-reader to a sequence, and could reasonably be named `reader->sequence`,
;; although by convention sequences are created by functions `in-TYPE`
(define (in-reader rx)
  (assert* 'in-reader (obj-reader? rx))
  (lambda ()
    (obj-reader-get rx)))
