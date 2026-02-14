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
    ;; procedure for reading next element.
    get-proc
    ;; procedure for skipping next element.
    skip-proc
    ;; box containing #f or procedure for closing this reader.
    ;; if box contains #f, it means reader is closed:
    ;;   (obj-reader-get) will return (values #<unspecified> #f)
    ;;   and (obj-reader-eof?) will return true
    close-box)
  ;; (make-obj-reader get-proc skip-proc close-proc)
  (protocol
    (lambda (new)
      (lambda (get-proc skip-proc close-proc)
        (%make-obj-reader new get-proc skip-proc close-proc))))
  (nongenerative %obj-reader-7c46d04b-34f4-4046-b5c7-b63753c1be40))


;; guardian to close obj-readers before garbage collecting them
(define obj-reader-guardian (make-guardian))


;; called internally by make-obj-reader: create and return an obj-reader
(define (%make-obj-reader new get-proc skip-proc close-proc)
  (assert* 'make-obj-reader (procedure? get-proc))
  (assert* 'make-obj-reader (logbit? 1 (procedure-arity-mask get-proc)))
  (when skip-proc
    (assert* 'make-obj-reader (procedure? skip-proc))
    (assert* 'make-obj-reader (logbit? 1 (procedure-arity-mask skip-proc))))
  (when close-proc
    (assert* 'make-obj-reader (procedure? close-proc))
    (assert* 'make-obj-reader (logbit? 1 (procedure-arity-mask close-proc))))

  (let ((reader (new get-proc (%make-skip-proc get-proc skip-proc) (box (or close-proc void1)))))
    ;; if close-proc is specified, register the newly created reader into guardian
    ;; so that close-proc will be called before the reader is garbage collected,
    ;; in case the user forgets to manually close it
    (when close-proc
      (obj-reader-guardian reader))
    reader))


(define (%make-skip-proc get-proc skip-proc)
  (if skip-proc
    skip-proc
    (lambda (rx)
      (let-values (((obj ok?) (get-proc rx)))
        ok?))))


;; Close an obj-reader or subtype.
;; Return unspecified value.
;;
;; Multiple calls to (obj-reader-close rx) on the same obj-reader are allowed,
;; and are equivalent to a single call.
;;
;; After an obj-reader has been closed,
;;   (obj-reader-get rx) will always return (values #<unspecified> #f) without calling its get-proc procedure,
;;   and (obj-reader-eof? rx) will always return #t
;;
;; Note: if obj-reader or subtype constructor accepts as argument some EXISTING resource(s),
;;   for example an input port, then the obj-reader does not OWN the resource(s) - it merely borrows them -
;;   and closing the obj-reader must NOT close those resource(s).
;;
;; If instead obj-reader or subtype creates some resource(s) internally,
;; or is specifically instructed to take ownership of some existing resource(s)
;; (for example by passing a truish value for an optional constructor argument close?),
;;
;; Implementation note: calls the close-proc stored in obj-reader at its creation,
;;   to release any resource OWNED by the obj-reader.
;;   close-proc is guaranteed to be called at most once per obj-reader,
;;   even if (obj-reader-close rx) is called concurrently on the same object from multiple threads.
(define (obj-reader-close rx)
  (assert* 'obj-reader-close (obj-reader? rx))
  (let* ((close-box  (obj-reader-close-box rx))
         (close-proc (unbox close-box)))
    (when (and close-proc (box-cas! close-box close-proc #f))
      (close-proc rx))))


;; return #t if obj-reader has been closed or reached eof, otherwise return #f.
(define (obj-reader-eof? rx)
  (assert* 'obj-reader-eof? (obj-reader? rx))
  (not (unbox (obj-reader-close-box rx))))


;; Read one element and return it.
;; each call will return two values:
;;  either (values elem #t) i.e. the next read element,
;;  or (values #<unspecified> #f) when the reader is exhausted or has been closed.
;;
;; Implementation note: if reader is closed, always returns (values #<unspecified> #f) without calling (get-proc rx).
;; Otherwise calls (get-proc rx) to read the next element.
;; If (get-proc rx) returns (values #<unspecified> #f) i.e. is exhausted,
;; this function will close the reader before returning such values.
(define (obj-reader-get rx)
  (assert* 'obj-reader-get (obj-reader? rx))
  (if (obj-reader-eof? rx)
    (values #f #f)
    (let-values (((obj ok?) ((obj-reader-get-proc rx) rx)))
      (unless ok?
        (obj-reader-close rx))
      (values obj ok?))))


;; Skip one element and return it.
;; each call will return one value:
;;  #t if an element was successfully skipped,
;;  or #f when the reader is exhausted or has been closed.
;;
;; Implementation note: if reader is closed, always returns #f without calling (skip-proc rx).
;; Otherwise calls (skip-proc rx) to skip the next element.
;; If (skip-proc rx) returns #f i.e. is exhausted,
;; this function will close the reader before returning such values.
(define (obj-reader-skip rx)
  (assert* 'obj-reader-skip (obj-reader? rx))
  (if (obj-reader-eof? rx)
    (values #f #f)
    (let ((ok? ((obj-reader-skip-proc rx) rx)))
      (unless ok?
        (obj-reader-close rx))
      (and ok? #t)))) ;; replace truish -> #t


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
    (make-obj-reader %constant-reader #f #f)))


;; create and return an exhausted obj-reader.
;; each call to (obj-reader-get rx) will always return the two values:
;;  (values #<unspecified> #f) indicating the reader is exhausted.
(define (empty-reader)
  (let ((%empty-reader ;; name shown when displaying the closure
          (lambda (rx)
            (values #f #f))))
    (make-obj-reader %empty-reader #f #f)))


(define-syntax for-reader-inner-part
  (syntax-rules ()
    ((_ () () () body ...)
      (begin0 body ...))
    ((_ (var vars ...) (ok? oks ...) (rx rxs ...) body ...)
      ;; (let-values (((var ok?) (obj-reader-get rx))) expands to more verbose code
      (call-with-values
        (lambda ()
          (obj-reader-get rx))
        (lambda (var ok?)
          (when ok?
            (for-reader-inner-part (vars ...) (oks ...) (rxs ...)
              body ...)))))))


;;; Iterate in parallel on elements of given obj-readers rx ..., and evaluate body ... on each element.
;;; Stop iterating when any reader is exhausted, and return unspecified value.
;;;
;;; If no readers are specified, behave as (forever body ...)
;;;
;;; Note: also supports optional early termination if "while expr" or "until expr"
;;; appear at top level without parentheses (and without quotes)
(define-syntax for-reader
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #'(forever body ...))
      ((_ ((var rx) ...) body ...)
        (with-syntax (((ok? ...) (generate-pretty-temporaries #'(var ...)))
                      ((trx ...) (generate-pretty-temporaries #'(rx ...))))
          #'(let* ((trx rx) ...)
              (let for-reader-loop ()
                (for-reader-inner-part (var ...) (ok? ...) (trx ...)
                  (with-while-until
                    body ...
                    (for-reader-loop))))))))))


;; create and return a iterator, i.e. a closure that accepts zero arguments and, at each call,
;; will return the two values returned by calling (obj-reader-get rx):
;;   either (values elem #t) i.e. the next generated value,
;;   or (values #<unspecified> #f) if obj-reader is exhausted or after (obj-reader-close rx) has been called.
;;
;; This function effectively converts an obj-reader to a iterator, and could reasonably be named `reader->iterator`,
;; although by convention iterators are created by functions `in-TYPE`
(define (in-reader rx)
  (assert* 'in-reader (obj-reader? rx))
  (lambda ()
    (obj-reader-get rx)))


;; create and return an obj-reader that generates the elements of specified unary iterator, one at time.
;; each call to (obj-reader-get rx) will return two values:
;;  either (values elem #t) i.e. the next element from the iterator
;;  or (values #<unspecified> #f) when the iterator is exhausted or after (obj-reader-close rx) has been called.
;;
;; This function effectively converts a iterator to an obj-reader, and could reasonably be named `iterator->reader`
;; although by convention readers are created by functions `TYPE-reader`
(define (iterator-reader iter)
  (assert* 'iterator-reader (procedure? iter))
  (assert* 'iterator-reader (logbit? 0 (procedure-arity-mask iter)))
  (let ((%iterator-reader ;; name shown when displaying the closure
          (lambda (rx) (iter))))
    (make-obj-reader %iterator-reader #f #f)))


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
    (make-obj-reader %list-reader #f #f)))


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
        (make-obj-reader %vector-reader #f #f)))
    ((v)
      (vector-reader v 0 (vector-length v)))))


;; Read all elements from specified obj-reader, collect them into a list, and return such list.
(define (reader->list rx)
  (let %reader->list ((rx rx) (l '()))
    (let-values (((elem ok?) (obj-reader-get rx)))
      (if ok?
        (%reader->list rx (cons elem l))
        (reverse! l)))))


;; Read all elements from specified obj-reader, collect them into a vector, and return such vector.
(define (reader->vector rx)
  (let %reader->vector ((rx rx) (l '()))
    (let-values (((elem ok?) (obj-reader-get rx)))
      (if ok?
        (%reader->vector rx (cons elem l))
        (list-reverse->vector l)))))
