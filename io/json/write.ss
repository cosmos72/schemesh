;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/json/json.ss


(define-record-type (json-writer %make-json-writer json-writer?)
  (parent obj-writer)
  (fields
    out                   ; textual input port
    (mutable rtd-cache)   ; #f or eq-hashtable
    (mutable prologue?)   ; #t before first call to any (json-writer-put...) function
    (mutable epilogue?))  ; #t if we should write #\] before closing the port
  (protocol
    (lambda (args->new)
      (lambda (out)
        ((args->new %json-writer-put %json-writer-close)
          out #f #t #f))))
  (nongenerative %json-writer-7c46d04b-34f4-4046-b5c7-b63753c1be41))


(define make-json-writer
  (case-lambda
    ((out)
      (assert* 'make-json-writer (port? out))
      (assert* 'make-json-writer (textual-port? out))
      (assert* 'make-json-writer (output-port? out))
      (%make-json-writer out))
    (()
      (make-json-writer (current-output-port)))))


(define (json-writer-eof? tx)
  (assert* 'json-writer-eof? (json-writer? tx))
  (obj-writer-eof? tx))


(define (json-writer-close tx)
  (assert* 'json-writer-close (json-writer? tx))
  (obj-writer-close tx))


;; called by (json-writer-close) -> (obj-writer-close)
(define (%json-writer-close tx)
  (let ((out (json-writer-out tx)))
    (when (json-writer-epilogue? tx)
      (put-string out "\n]\n")
      (json-writer-epilogue?-set! tx #f)
      (json-writer-prologue?-set! tx #t))
    (unless (port-closed? out)
      (close-port out))))


(define (write/string out str)
  ;; FIXME must escape characters #\x0 ... #\x1f with JSON syntax, not scheme syntax
  (write str out))


(define (write/symbol out sym)
  (write/string out (symbol->string sym)))


;; if obj is a supported atomic value, write it to out and return #t.
;; otherwise return #f.
(define (write/atomic? out obj)
  (cond
    ((boolean? obj)
      (put-string out (if obj "true" "false"))
      #t)
    ((eq? obj (void))
      (put-string out "null")
      #t)
    ((symbol? obj)
      (write/symbol out obj)
      #t)
    ((string? obj)
      (write/string out obj)
      #t)
    ((or (flonum? obj) (integer? obj))
      ;; json only supports flonums or exact integers.
      ;; it does not support exact fractions nor complex numbers
      (write obj out)
      #t)
    (else
      #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; low-level json writer functions


;; write a single json token. token must be one of:
;;   * a string     => written as quoted string
;;   * a symbol     => written as quoted string
;;   * a flonum or exact integer
;;   * a boolean    => written as true or false
;;   * (void)       => written as null
;;   * (eof-object) => ignored
;;   * a character among #\: #\, #\[ #\] #\{ #\}
(define (json-writer-put-token tx tok)
  (let ((out (json-writer-out tx)))
    (cond
      ((char? tok)
        (assert* 'json-writer-put-token (memv tok '(#\: #\, #\[ #\] #\{ #\})))
        (put-char out tok))
      ((eof-object? tok)
        (void))
      ((write/atomic? out tok)
        (void))
      (else
        (raise-errorf 'json-writer-put-token "unsupported token: ~s" tok))))
  (json-writer-prologue?-set! tx #f))


;; write a json value.
;; obj must be either a json atomic value:
;;   * a string     => written as quoted string
;;   * a symbol     => written as quoted string
;;   * a flonum or exact integer
;;   * a boolean    => written as true or false
;;   * (void)       => written as null
;;   * (eof-object) => ignored
;;
;; or a composite json value:
;;   * a plist key -> value,
;;       where every key is a symbol and every value is a json value
;;   * a span where every element is a json value
(define (json-writer-put-value tx obj)
  (let ((out (json-writer-out tx)))
    (cond
      ((null? obj)
        (put-string out "{}"))

      ((pair? obj)
        (unless (plist? obj)
          (raise-errorf 'json-writer-put-value "list is not a plist: ~s" obj))
        (put-char out #\{)
        (do ((l obj (cddr l)))
            ((null? l))
          (unless (eq? l obj)
            (put-char out #\,))
          (let ((key (car l)))
            (unless (symbol? key)
              (raise-errorf 'json-writer-put-value "unsupported json object key: ~s" key))
            (write/symbol out key)
            (put-char out #\:)
            (json-writer-put-value tx (cadr l))))
        (put-char out #\}))

      ((record? obj)
        (unless (span? obj)
          (raise-errorf 'json-writer-put-value "unsupported json array: ~s" obj))
        (put-char out #\[)
        (do ((i 0 (fx1+ i))
             (n (span-length obj)))
            ((fx>=? i n))
          (unless (fxzero? i)
            (put-char out #\,))
          (json-writer-put-value tx (span-ref obj i)))
        (put-char out #\]))

      ((write/atomic? out obj)
        (void))

      (else
        (raise-errorf 'json-writer-put-value "unsupported datum: ~s" obj)))

    (json-writer-prologue?-set! tx #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serialize scheme datum to json

(define (json-writer-put tx obj)
  (assert* 'json-writer-put (json-writer? tx))
  (obj-writer-put tx obj))


(define (write/key out key)
  (cond
    ((symbol? key) (write/symbol out key))
    ((string? key) (write/string out key))
    (else          (raise-errorf 'json-writer-put "unsupported object key: ~s" key))))


(define (write/key+value out first? key val)
  (unless first?
    (put-char out #\,))
  (write/key out key)
  (put-char out #\:)
  (write/datum out val))


(define (write/array out obj)
  (put-char out #\[)
  (do ((i 0 (fx1+ i))
       (n   (array-length obj))
       (ref (array-accessor obj)))
      ((fx>=? i n))
    (unless (fxzero? i)
      (put-char out #\,))
    (write/datum out (ref obj i)))
  (put-char out #\]))


(define (write/htable out obj)
  (put-char out #\{)
  (let-values (((cursor next!) (htable-cursor obj)))
    (do ((first? #t #f)
         (cell   (next! cursor) (next! cursor)))
        ((not cell))
      (write/key+value out first? (car cell) (cdr cell))))
  (put-char out #\}))


(define (write/list out obj)
  (unless (plist? obj)
    (raise-errorf 'json-writer-put "unsupported datum, list is not a plist: ~s" obj))
  (put-char out #\{)
  (do ((l obj (cddr l)))
      ((null? l))
    (write/key+value out (eq? l obj) (car l) (cadr l)))
  (put-char out #\}))


(define (write/record out obj)
  ;; TODO implement
  (void))


(define (write/datum out obj)
  (cond
    ((write/atomic? out obj)
      (void))
    ((or (null? obj) (pair? obj))
      (write/list out obj))
    ((array? obj)
      (write/array out obj))
    ((htable? obj)
      (write/htable out obj))
    ((record? obj)
      (write/record out obj))
    (else
      (raise-errorf 'json-writer-put "unsupported datum: ~s" obj))))


;; called by (json-writer-put) -> (obj-writer-put)
(define (%json-writer-put tx obj)
  (let ((out (json-writer-out tx)))
    (if (json-writer-prologue? tx)
      (begin
        (put-string out "[\n")
        (json-writer-prologue?-set! tx #f)
        (json-writer-epilogue?-set! tx #t))
      (put-string out ",\n"))
    (write/datum out obj)))
