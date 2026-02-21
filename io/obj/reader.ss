;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file io/obj/obj.ss

(define-record-type reader
  (fields
    ;; procedure for reading next element.
    get-proc
    ;; procedure for skipping next element.
    skip-proc
    ;; box containing #f or procedure for closing this reader.
    ;; if box contains #f, it means reader is closed:
    ;;   (reader-get) will return (values #<unspecified> #f)
    ;;   and (reader-eof?) will return true
    close-box)
  ;; (make-reader get-proc skip-proc close-proc)
  (protocol
    (lambda (new)
      (lambda (get-proc skip-proc close-proc)
        (%make-reader new get-proc skip-proc close-proc))))
  (nongenerative %reader-7c46d04b-34f4-4046-b5c7-b63753c1be40))


;; guardian to close readers before garbage collecting them
(define reader-guardian (make-guardian))


;; called internally by make-reader: create and return a reader
(define (%make-reader new get-proc skip-proc close-proc)
  (assert* 'make-reader (procedure? get-proc))
  (assert* 'make-reader (logbit? 1 (procedure-arity-mask get-proc)))
  (when skip-proc
    (assert* 'make-reader (procedure? skip-proc))
    (assert* 'make-reader (logbit? 1 (procedure-arity-mask skip-proc))))
  (when close-proc
    (assert* 'make-reader (procedure? close-proc))
    (assert* 'make-reader (logbit? 1 (procedure-arity-mask close-proc))))

  (let ((reader (new get-proc (%make-skip-proc get-proc skip-proc) (box (or close-proc void1)))))
    ;; if close-proc is specified, register the newly created reader into guardian
    ;; so that close-proc will be called before the reader is garbage collected,
    ;; in case the user forgets to manually close it
    (when close-proc
      (reader-guardian reader))
    reader))


(define (%make-skip-proc get-proc skip-proc)
  (if skip-proc
    skip-proc
    (lambda (rx)
      (let-values (((obj ok?) (get-proc rx)))
        ok?))))


;; Close a reader or subtype.
;; Return unspecified value.
;;
;; Multiple calls to (reader-close rx) on the same reader are allowed,
;; and are equivalent to a single call.
;;
;; After a reader has been closed,
;;   (reader-get rx) will always return (values #<unspecified> #f) without calling its get-proc procedure,
;;   and (reader-eof? rx) will always return #t
;;
;; Note: if reader or subtype constructor accepts as argument some EXISTING resource(s),
;;   for example an input port, then the reader does not OWN the resource(s) - it merely borrows them -
;;   and closing the reader must NOT close those resource(s).
;;
;; If instead reader or subtype creates some resource(s) internally,
;; or is specifically instructed to take ownership of some existing resource(s)
;; (for example by passing a truish value for an optional constructor argument close?),
;;
;; Implementation note: calls the close-proc stored in reader at its creation,
;;   to release any resource OWNED by the reader.
;;   close-proc is guaranteed to be called at most once per reader,
;;   even if (reader-close rx) is called concurrently on the same object from multiple threads.
(define (reader-close rx)
  (assert* 'reader-close (reader? rx))
  (let* ((close-box  (reader-close-box rx))
         (close-proc (unbox close-box)))
    (when (and close-proc (box-cas! close-box close-proc #f))
      (close-proc rx))))


;; return #t if reader has been closed or reached eof, otherwise return #f.
(define (reader-eof? rx)
  (assert* 'reader-eof? (reader? rx))
  (not (unbox (reader-close-box rx))))


;; Read one element and return it.
;; each call will return two values:
;;  either (values elem #t) i.e. the next read element,
;;  or (values #<unspecified> #f) when the reader is exhausted or has been closed.
;;
;; Implementation note: if reader is closed, always returns (values #<unspecified> #f) without calling (get-proc rx).
;; Otherwise calls (get-proc rx) to read the next element.
;; If (get-proc rx) returns (values #<unspecified> #f) i.e. is exhausted,
;; this function will close the reader before returning such values.
(define (reader-get rx)
  (assert* 'reader-get (reader? rx))
  (if (reader-eof? rx)
    (values #f #f)
    (let-values (((obj ok?) ((reader-get-proc rx) rx)))
      (unless ok?
        (reader-close rx))
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
(define (reader-skip rx)
  (assert* 'reader-skip (reader? rx))
  (if (reader-eof? rx)
    (values #f #f)
    (let ((ok? ((reader-skip-proc rx) rx)))
      (unless ok?
        (reader-close rx))
      (and ok? #t)))) ;; replace truish -> #t


;; create and return a reader that generates always the same value.
;; each call to (reader-get rx) will return two values:
;;  either (values const #t) i.e. the next element, which is always eq? to const
;;  or (values #<unspecified> #f) after (reader-close rx) has been called.
;;
;; note: this reader is unlimited, and stops generating values only if (reader-close rx) is called.
(define (constant-reader const)
  (let ((%constant-reader ;; name shown when displaying the closure
          (lambda (rx)
            (values const #t))))
    (make-reader %constant-reader #f #f)))


;; create and return an exhausted reader.
;; each call to (reader-get rx) will always return the two values:
;;  (values #<unspecified> #f) indicating the reader is exhausted.
(define (empty-reader)
  (let ((%empty-reader ;; name shown when displaying the closure
          (lambda (rx)
            (values #f #f))))
    (make-reader %empty-reader #f #f)))


(define-syntax for-reader-inner-part
  (syntax-rules ()
    ((_ () () () body ...)
      (begin0 body ...))
    ((_ (var vars ...) (ok? oks ...) (rx rxs ...) body ...)
      ;; (let-values (((var ok?) (reader-get rx))) expands to more verbose code
      (call-with-values
        (lambda ()
          (reader-get rx))
        (lambda (var ok?)
          (when ok?
            (for-reader-inner-part (vars ...) (oks ...) (rxs ...)
              body ...)))))))


;;; Iterate in parallel on elements of given readers rx ..., and evaluate body ... on each element.
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
;; will return the two values returned by calling (reader-get rx):
;;   either (values elem #t) i.e. the next generated value,
;;   or (values #<unspecified> #f) if reader is exhausted or after (reader-close rx) has been called.
;;
;; This function effectively converts a reader to a iterator, and could reasonably be named `reader->iterator`,
;; although by convention iterators are created by functions `in-TYPE`
(define (in-reader rx)
  (assert* 'in-reader (reader? rx))
  (lambda ()
    (reader-get rx)))


;; create and return a reader that generates the elements of specified unary iterator, one at time.
;; each call to (reader-get rx) will return two values:
;;  either (values elem #t) i.e. the next element from the iterator
;;  or (values #<unspecified> #f) when the iterator is exhausted or after (reader-close rx) has been called.
;;
;; This function effectively converts a iterator to a reader, and could reasonably be named `iterator->reader`
;; although by convention readers are created by functions `TYPE-reader`
(define (iterator-reader iter)
  (assert* 'iterator-reader (procedure? iter))
  (assert* 'iterator-reader (logbit? 0 (procedure-arity-mask iter)))
  (let ((%iterator-reader ;; name shown when displaying the closure
          (lambda (rx) (iter))))
    (make-reader %iterator-reader #f #f)))


;; create and return a reader that generates the elements of specified list.
;; each call to (reader-get rx) will return two values:
;;  either (values elem #t) i.e. the next element from the list
;;  or (values #<unspecified> #f) when the list is exhausted or after (reader-close rx) has been called.
;;
;; This function effectively converts a list to a reader, and could reasonably be named `list->reader`
;; although by convention readers are created by functions `TYPE-reader`
(define (list-reader l)
  (let ((%list-reader ;; name shown when displaying the closure
          (lambda (rx)
            (if (null? l)
              (values #f #f)
              (let ((elem (car l)))
                (set! l (cdr l))
                (values elem #t))))))
    (make-reader %list-reader #f #f)))


;; create and return a reader that generates the specified elements.
;; each call to (reader-get rx) will return two values:
;;  either (values elem #t) i.e. the next element from the list
;;  or (values #<unspecified> #f) when the list is exhausted or after (reader-close rx) has been called.
;;
;; This function effectively converts arbitrary elements a reader
(define (datum-reader . l)
  (list-reader l))


;; create and return a reader that generates the elements of specified vector.
;; each call to (reader-get rx) will return two values:
;;  either (values elem #t) i.e. the next element from the vector
;;  or (values #<unspecified> #f) when the vector is exhausted or after (reader-close rx) has been called.
;;
;; This function effectively converts a vector to a reader, and could reasonably be named `vector->reader`
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
        (make-reader %vector-reader #f #f)))
    ((v)
      (vector-reader v 0 (vector-length v)))))


;; cannot import (scheme2k containers list) => define a private, reduced for-list
(define-syntax for-list1
  (syntax-rules ()
    ((_ ((var l)) body ...)
      (do ((tail l (cdr tail)))
          ((null? tail))
        (let ((var (car tail)))
          body ...)))))


;; Read all elements from specified reader, collect them into a list, and return such list.
(define (reader->list rx)
  (let %reader->list ((rx rx) (l '()))
    (let-values (((elem ok?) (reader-get rx)))
      (if ok?
        (%reader->list rx (cons elem l))
        (reverse! l)))))


;; Read all elements from specified reader, collect them into a vector, and return such vector.
(define (reader->vector rx)
  (let %reader->vector ((rx rx) (l '()))
    (let-values (((elem ok?) (reader-get rx)))
      (if ok?
        (%reader->vector rx (cons elem l))
        (list-reverse->vector l)))))


;; create and return a reader that generates the elements of specified readers.
;; The elements of first reader will be generated first, followed by the elements of second reader, and so on.
;;
;; This function effectively aggregates multiple readers.
(define readers
  (case-lambda
    (()
      (empty-reader))
    ((r)
      (assert* 'readers (reader? r))
      r)
    (rs
      (for-list1 ((r rs))
        (assert* 'readers (reader? r)))
      (letrec ((%readers-get ;; name shown when displaying the closure
                (lambda (rx)
                  (if (null? rs)
                    (values #f #f)
                    (let-values (((elem ok?) (reader-get (car rs))))
                      (if ok?
                        (values elem ok?)
                        (begin
                          (reader-close (car rs))
                          (set! rs (cdr rs))
                          (%readers-get rx)))))))
               (%readers-skip ;; name shown when displaying the closure
                (lambda (rx)
                  (cond
                    ((null? rs)             #f)
                    ((reader-skip (car rs)) #t)
                    (else
                      (reader-close (car rs))
                      (set! rs (cdr rs))
                      (%readers-skip rx)))))
               (%readers-close
                 (lambda (rx)
                   (for-list1 ((r rs))
                     (reader-close rs))
                   (set! rs '()))))
        (make-reader %readers-get %readers-skip %readers-close)))))
