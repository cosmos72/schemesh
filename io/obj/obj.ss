;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;;;
;;; define an abstract reader type: generates or reads from somewhere a finite or unlimited sequence of arbitrary values.
;;;
;;; each call to (reader-get r) will return two values:
;;;  either (values elem #t) i.e. the next element
;;;  or (values #<unspecified> #f) when reader is exhausted or after (reader-close r) is called
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
;;;  srfi 158 generators: requires generators to be thunks i.e. no-argument procedures,
;;;                       and cannot distinguish between generated value eq? to (eof-object) and generator being exhausted.
;;;                       Also, there is no simple mechanism to add more behavior,
;;;                       as for example closing a generator early,
;;                        or releasing OS-level resources (file descriptors ...)
;;;                       or waiting simultaneously on multiple generators
(library (scheme2k io obj (0 9 3))
  (export
    ;; obj/reader.ss
    make-reader reader reader? reader-get reader-eof? reader-close reader-skip
    for-reader in-reader constant-reader datum-reader empty-reader list-reader iterator-reader vector-reader
    reader->list reader->vector readers

    ;; obj/filter-reader.ss
    make-filter-reader filter-reader filter-reader?  filter-reader-inner

    ;; obj/nested-reader.ss
    nested-reader nested-reader? nested-reader-inner nested-reader-inner-get nested-reader-inner-eof? nested-reader-inner-close nested-reader-inner-skip

    ;; obj/range-reader.ss
    make-range-reader range-reader range-reader?

    ;; obj/writer.ss
    make-writer writer writer? writer-put writer-eof? writer-close
    discard-writer full-writer list-writer vector-writer)
  (import
    (rnrs)
    (only (chezscheme)          box box-cas! collect-request-handler fx1+ fx1- include logbit? meta-cond procedure-arity-mask
                                record-type-descriptor record-writer reverse! scheme-version-number unbox void)
    (prefix (only (chezscheme)  make-guardian) chez:)
    (only (scheme2k bootstrap)  assert* begin0 forever fx<=?* generate-pretty-temporaries raise-errorf void1 with-while-until))


;; private reimplementation of (list-reverse->vector)
;; avoids circular dependency with (scheme2k containser list)
;;
;; create and return a vector that contains
;; list elements in reverse order.
;; does not modify list.
(define (list-reverse->vector l)
  (let* ((n (length l))
         (v (make-vector n)))
    (do ((i (fx1- n) (fx1- i))
         (l l (cdr l)))
        ((null? l) v)
      (vector-set! v i (car l)))))


;; using guardians for automatically closing readers and writers before they are garbage collected
;; causes errors "Exception in mutex-acquire: mutex is defunct" in Chez Scheme < 10
;; => enable guardians only on Chez Scheme >= 10
(define make-guardian
  (meta-cond
    ((call-with-values scheme-version-number (lambda (major minor patch) (fx>=? major 10)))
      ;; on Chez Scheme >= 10, create an actual guardian
      chez:make-guardian)
    (else
      ;; on Chez Scheme < 10, create a do-nothing guardian
      (lambda ()
        (case-lambda
          ((obj) (void))
          (() #f))))))


(include "io/obj/reader.ss")
(include "io/obj/writer.ss")

(include "io/obj/nested-reader.ss")
(include "io/obj/filter-reader.ss")
(include "io/obj/range-reader.ss")


(define (reader-display r port writer label)
  (put-string port "#<")
  (put-string port label)
  (put-string port (if (reader-eof? r) " eof " " ok "))
  (writer (reader-get-proc r) port)
  (put-char port #\space)
  (writer (unbox (reader-close-box r)) port)
  (put-char port #\>))


;; customize how "reader" objects are printed
(record-writer (record-type-descriptor reader)
  (lambda (r port writer)
    (reader-display r port writer "reader")))


;; customize how "filter-reader" objects are printed
(record-writer (record-type-descriptor filter-reader)
  (lambda (r port writer)
    (reader-display r port writer "filter-reader")))


;; customize how "range-reader" objects are printed
(record-writer (record-type-descriptor range-reader)
  (lambda (r port writer)
    (put-string port "#<range-reader")
    (put-string port (if (reader-eof? r) " eof " " ok "))
    (writer (range-reader-skip-n r) port)
    (put-char port #\space)
    (writer (range-reader-get-n r) port)
    (put-char port #\>)))


;; customize how "writer" objects are printed
(record-writer (record-type-descriptor writer)
  (lambda (w port writer)
    (put-string port "#<writer")
    (put-string port (if (writer-eof? w) " eof " " ok "))
    (writer (writer-put-proc w) port)
    (put-char port #\space)
    (writer (unbox (writer-close-box w)) port)
    (put-char port #\>)))


;; On Chez Scheme >= 10, install into collect-request-handler a procedure that retrieves
;; unreachable readers and writers immediately before they are garbage collected,
;; and closes them
(meta-cond
  ((call-with-values scheme-version-number (lambda (major minor patch) (fx>=? major 10)))
    (let ((gc (collect-request-handler)))
      (collect-request-handler
        (lambda ()
          ;; first, call the original collect-request-handler
          (gc)

          ;; then, retrieve all readers ready to be garbage collected, and close them
          (do ((rx (reader-guardian) (reader-guardian)))
              ((not rx))
            ;; (debugf "closing ~s before garbage collecting it" rx)
            (reader-close rx)))))))

) ; close library
