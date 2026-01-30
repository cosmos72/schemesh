;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;;;
;;; define an abstract producer type: generates a finite or unlimited sequence of arbitrary values.
;;; if value #!eof is generated, it means the producer is finite and exhausted.
;;;
;;; existing APIs that were considered, and reasons for not reusing them:
;;;
;;;  racket/stream: assumes that each call to (stream-rest s) returns a different stream,
;;;                 and that after calling (stream-rest s), the original s is not modified
;;;                 and its (stream-first s) returns the same value as before.
;;;
;;;  racket/generator: requires an efficient (yield), which Chez Scheme lacks.
;;;                    implementing it with continuations is very slow.
;;;
;;;  srfi 158 generators: requires generators to be thunks i.e. no-argument procedures.
;;;                       there is no simple mechanism to add more behavior,
;;;                       as for example closing a generator to release OS-level resources (file descriptors ...)
;;;                       or waiting simultaneously on multiple generators
(library (scheme2k producer (0 9 3))
  (export
    make-producer producer producer? producer-next producer-close producer-eof?
    in-producer constant->producer hash->producer list->producer sequence->producer vector->producer)
  (import
    (rnrs)
    (only (chezscheme)                    fx1+ logbit? procedure-arity-mask record-type-descriptor record-writer)
    (only (scheme2k bootstrap)            assert* forever fx<=?* generate-pretty-temporaries with-while-until)
    (only (scheme2k containers hashtable) hash-cursor hash-cursor-next!))


;; called internally by make-producer: create and return a producer
(define (%make-producer new next-proc close-proc)
  (assert* 'make-producer (procedure? next-proc))
  (assert* 'make-producer (logbit? 1 (procedure-arity-mask next-proc)))
  (when close-proc
    (assert* 'make-producer (procedure? close-proc))
    (assert* 'make-producer (logbit? 1 (procedure-arity-mask close-proc))))
  (new next-proc close-proc #f))


(define-record-type producer
  (fields
    next-proc
    (mutable close-proc) ; #f or procedure
    (mutable eof?))      ; boolean
  (protocol
    (lambda (new)
      (lambda (next-proc close-proc)
        (%make-producer new next-proc close-proc))))
  (nongenerative %producer-7c46d04b-34f4-4046-b5c7-b63753c1be39))


;; call (next-proc p) to generate one more value and return it.
;; return #!eof when values are exhausted
(define (producer-next p)
  (assert* 'producer-next (producer? p))
  (if (producer-eof? p)
    (eof-object)
    (let ((obj ((producer-next-proc p) p)))
      (when (eof-object? obj)
        (producer-eof?-set! p #t))
      obj)))


;; call (close-proc p) to release any resource held by the producer.
;; return unspecified value.
;;
;; further calls to (producer-close p) on the same p have no effect.
(define (producer-close p)
  (assert* 'producer-close (producer? p))
  (let ((close-proc (producer-close-proc p)))
    (when close-proc
      (close-proc p)
      (producer-close-proc-set! p #f)))
  (producer-eof?-set! p #t))


;; create and return a producer that generates always the same value:
;; each call to (producer-next p) will return const.
;;
;; note: this generator is unlimited, and will return #!eof only if (producer-close p) is called.
(define (constant->producer const)
  (let ((producer/constant ;; name shown when displaying the closure
          (lambda (p) const)))
    (make-producer producer/constant #f)))


;; create and return a producer that generates the cells of specified hashtable.
;; each call to (producer-next p) will return
;;  either a pair (key . value) from the hashtable
;;  or #!eof after all hashtable cells have been produced, or after (producer-close p) is called.
;;
;; note: assigning the cdr of a returned pair propagates to the hashtable.
;; do NOT modify the car of any returned pair!
(define (hash->producer ht)
  (let* ((iter (hash-cursor ht))
         (producer/hashtable ;; name shown when displaying the closure
           (lambda (p)
             (let ((cell (hash-cursor-next! iter)))
               (if cell cell (eof-object))))))
    (make-producer producer/hashtable #f)))


;; create and return a producer that generates the elements of specified list.
;; each call to (producer-next p) will return
;;  either next element from the list
;;  or #!eof after all list elements have been produced, or after (producer-close p) is called.
;;
;; note: there is no way to distinguish between an #!eof element and the end of elements.
;; If you need such distinction, use `(in-list)` or some other mechanism - not a producer.
(define (list->producer l)
  (let ((producer/list ;; name shown when displaying the closure
          (lambda (p)
            (if (null? l)
              (eof-object)
              (let ((elem (car l)))
                (set! l (cdr l))
                elem)))))
    (make-producer producer/list #f)))


;; create and return a producer that generates the elements of specified vector.
;; each call to (producer-next p) will return
;;  either next element from the vector
;;  or #!eof when all vector elements have been produced
;;
;; note: there is no way to distinguish between an #!eof element and the end of elements.
;; If you need such distinction, use `(in-vector)` or some other mechanism - not a producer.
(define vector->producer
  (case-lambda
    ((v start end)
      (assert* 'vector->producer (fx<=?* 0 start end (vector-length v)))
      (let ((producer/vector ;; name shown when displaying the closure
              (lambda (p)
                (if (fx>=? start end)
                  (eof-object)
                  (let ((elem (vector-ref v start)))
                    (set! start (fx1+ start))
                    elem)))))
        (make-producer producer/vector #f)))
    ((v)
      (vector->producer v 0 (vector-length v)))))




;; create and return a producer that generates the elements of specified sequence, one at time.
;; each call to (producer-next p) will return
;;  either next element from the sequence
;;  or #!eof when the sequence is exhausted.
;;
;; This function effectively converts a sequence to a producer.
;;
;; Note: there is no way to distinguish between an #!eof element and the sequence being exhausted.
;; If you need such distinction, use the sequence itself - do not convert it to a producer.
(define (sequence->producer seq)
  (assert* 'sequence->producer (procedure? seq))
  (assert* 'sequence->producer (logbit? 0 (procedure-arity-mask seq)))
  (let ((producer/sequence ;; name shown when displaying the closure
          (lambda (p)
            (let-values (((elem ok?) (seq)))
              (if ok? elem (eof-object))))))
    (make-producer producer/sequence #f)))


;; create and return a closure that accepts zero arguments and, at each call,
;; will return two values:
;;   either (values elem #t) i.e. the next generated value and #t,
;;   or (values #<unspecified> #f) if producer is exhausted.
;;
;; This function effectively converts a producer to a sequence.
(define (in-producer p)
  (assert* 'in-producer (producer? p))
  (lambda ()
    (let ((obj (producer-next p)))
      (values obj (not (eof-object? obj))))))


;; customize how "producer" objects are printed
(record-writer (record-type-descriptor producer)
  (lambda (p port writer)
    (put-string port "(make-producer ")
    (writer (producer-next-proc p) port)
    (put-char port #\space)
    (writer (producer-close-proc p) port)
    (put-char port #\space)
    (writer (if (producer-eof? p) 'eof #f) port)
    (put-string port ")")))


) ; close library