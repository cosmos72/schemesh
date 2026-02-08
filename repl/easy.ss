;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file repl/repl.ss


;; easy wrapper for (fd-read-all) (get-bytevector-all) (get-string-all) (reader->list)
(define (all from)
  (cond
    ((fixnum? from)      (fd-read-all from))
    ((port? from)        (if (binary-port? from)
                          (get-bytevector-all from)
                          (get-string-all from)))
    ((obj-reader? from)  (reader->list from))
    (else
      (raise-errorf 'all "unsupported reader: ~s" from))))


;; easy wrapper for (fd-read-all) (get-bytevector-all) (get-string-all) (reader->vector)
(define (all/vector from)
  (cond
    ((fixnum? from)      (fd-read-all from))
    ((port? from)        (if (binary-port? from)
                          (get-bytevector-all from)
                          (get-string-all from)))
    ((obj-reader? from)  (reader->vector from))
    (else
      (raise-errorf 'all/vector "unsupported reader: ~s" from))))


;; easy wrapper for (fd-close) (close-port) (obj-reader-close) (obj-writer-close)
(define (close obj)
  (cond
    ((fixnum? obj)      (fd-close obj))
    ((port? obj)        (close-port obj))
    ((obj-reader? obj)  (obj-reader-close obj))
    ((obj-writer? obj)  (obj-writer-close obj))))


;; easy wrapper for (make-dir-reader)
(define dir
  (case-lambda
    ;; current directory charspan must NOT be modified => copy it
    (()     (make-dir-reader (charspan->string (sh-cwd))))
    ((path) (make-dir-reader path))))


;; easy wrapper for (port-eof?) (obj-reader-eof?) (obj-writer-eof?)
(define (eof? obj)
  (cond
    ((port? obj)        (port-eof? obj))
    ((obj-reader? obj)  (obj-reader-eof? obj))
    ((obj-writer? obj)  (obj-writer-eof? obj))))


;; easy wrapper for (fd-read) (get-bytevector-some) (get-line) (obj-reader-get)
;; always returns two values:
;;   either (values elem #t)
;;   or (values #<unspecified> #f) on eof
(define (get from)
  (cond
    ((fixnum? from)
      (let* ((bv (make-bytevector 4096 0))
             (n  (fd-read from bv)))
        (cond
          ((fxzero? n)
            (values #f #f))
          (else
            (bytevector-truncate! bv n)
            (values bv #t)))))
    ((port? from)
      (let ((got (if (binary-port? from)
                   (get-bytevector-some from)
                   (get-line from))))
        (values got (not (eof-object? got)))))
    ((obj-reader? from)
      (obj-reader-get from))
    (else
      (raise-errorf 'get "unsupported reader: ~s" from))))


;; easy wrapper for (fd-write-all) (put-bytevector) (put-string) (obj-writer-put)
;; returns unspecified value
(define (put to datum)
  (cond
    ((fixnum? to)
      (assert* 'put (bytevector? datum))
      (fd-write-all to datum))
    ((port? to)
      (cond
        ((binary-port? to)
          (assert* 'put (bytevector? datum))
          (put-bytevector to datum))
        (else
          (assert* 'put (string? datum))
          (put-string to datum))))
    ((obj-writer? to)
      (obj-writer-put to datum))
    (else
      (raise-errorf 'put "unsupported writer: ~s" to))))


;; easy wrapper for (make-process-reader)
(define (proc)
  (make-process-reader))


;; easy wrapper for (get-line) (obj-reader-skip)
;; always returns one value:
;;   #t if one element was skipped,
;;   or #f if reader is exhausted
(define (skip from)
  (cond
    ((and (port? from) (textual-port? from))
      (not (eof-object? (get-line from))))
    ((obj-reader? from)
      (obj-reader-skip from))
    (else
      (raise-errorf 'skip "unsupported reader: ~s" from))))


;; iterate (get from) then (put to) until from is exhausted
(define (copy from to)
  (let-values (((datum ok?) (get from)))
    (when ok?
      (put to datum)
      (copy from to))))


;; iterate (get from) then (put to) until from is exhausted.
;;
;; then always call (close from) and (close to),
;; even if (get from) or (put to) raise a condition or call a continuation
;;
;; return value of (close to)
(define (copy/close from to)
  (let ((ret #f))
    (dynamic-wind
      void  ; before
      (lambda () (copy from to))
      (lambda () (close from) (set! ret (close to))))
    ret))


;; evaluate body ... with var ... bound to expr ... then always call (close expr-value) ...
;; even if body raises a condition or calls a continuation
;;
;; If used from a sh-expr, (close expr-value) ... will be called when job finishes.
(define-syntax with-sh-closable
  (syntax-rules ()
    ((_ () body ...)
      (begin^ body ...))
    ((_ ((var expr) ...) body ...)
      (with-sh-resource ((var expr close) ...) body ...))))


;; easy wrapper for (make-json-reader)
(define from-json
  (case-lambda
    ((in)
      (make-json-reader in))
    (()
      (make-json-reader (sh-port #f 0 'binary)))))


;; easy wrapper for (list-reader)
;; l must be a list
(define (from-list l)
  (list-reader l))


;; easy wrapper for (make-queue-reader)
;; q must be a queue-writer
(define (from-queue q)
  (make-queue-reader q))


;; easy wrapper for (vector-reader)
;; v must be a vector
(define (from-vector l)
  (vector-reader l))


;; easy wrapper for (make-wire-reader)
(define from-wire
  (case-lambda
    ((in)
      (make-json-reader in))
    (()
      (make-json-reader (sh-port #f 0 'binary)))))


;; easy wrapper for (make-wire-writer)
(define to-json
  (case-lambda
    ((out)
      (make-json-writer out))
    (()
      (make-json-writer (sh-port #f 1 'textual)))))


;; easy wrappwe for (list-writer)
(define (to-list)
  (list-writer))


;; easy wrapper for (make-queue-writer)
(define (to-queue)
  (make-queue-writer))


;; easy wrapper for (vector-writer)
(define (to-vector)
  (vector-writer))


;; easy wrapper for (make-wire-writer)
(define to-wire
  (case-lambda
    ((out)
      (make-wire-writer out))
    (()
      (make-wire-writer (sh-port #f 1 'binary)))))


;; create a reader that autodetects protocol upon the first call to (obj-reader-get)
;; FIXME: currently always creates a json-reader
(define (from-stdin)
  (from-json))


;; TODO: choose writer protocol depending on optional arguments or stdout fd type:
;;   tty    => make-tabular-writer
;;   socket => make-wire-writer
;;   else   => make-json-writer
(define (to-stdout)
  (to-json))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; where


;; create and return a filter reader wrapping a user-provided reader.
;; usage: (where reader expression)
;;   expression should contain one or more forms ,name i.e. (unquote name) that will be expanded
;;   to the value of (field elem 'name) for each processed element.
;;
;; Note: works, but changes the meaning of unquote, and forces user-provided code to insert unquote in unexpected places,
;; thus breaks quasiquoting, both inside (where) own's definition and inside user-provided expressions.
;; Also breaks (expand `(where rx user-provided-form-containing-unquote))
(define-macro (where rx expr)
  (list 'make-filter-reader rx
     (list 'lambda '(elem cache)
       (list 'let-macro '((unquote name) (list 'field 'elem name 'cache))
          expr))))


;; create a filter reader wrapping a user-provided reader.
;; usage: (where reader expression)
;;   expression should contain one or more forms (^ name) that will be expanded
;;   to the value of (field elem 'name) for each processed element.
;;
;; safer than (where), as it only changes the meaning of seldom-used ^
(define-macro (where^ rx expr)
  `(make-filter-reader ,rx
     (lambda (elem cache)
       (let-macro ((^ name) `(field elem ,name cache))
          ,expr))))
