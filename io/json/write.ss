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
    out                   ; textual output port
    (mutable cache)       ; #f or eq-hashtable rtd -> record-info, set in construction or created lazily
    (mutable prologue?)   ; #t before first call to any (json-writer-put...) function
    (mutable epilogue?)   ; #t if we should write #\] before closing the port
    close-out?)           ; boolean, #t if closing the json-writer must close the underlying textual output port
  (protocol
    (lambda (args->new)
      (lambda (out close-out? cache)
        ((args->new %json-writer-put %json-writer-close)
          out #f #t #f (and close-out? #t)))))
  (nongenerative %json-writer-7c46d04b-34f4-4046-b5c7-b63753c1be43))


;; Create a json-writer that, at each call to one of
;;   (obj-writer-put) (json-writer-put) (json-writer-put-value) or (json-writer-put-token),
;; serializes the received data in streaming mode,
;; and writes it to the underlying textual output port.
;;
;; Note: as per obj-writer contract, by default closing a json-writer does NOT close the underlying textual output port,
;; because it is a pre-existing, borrowed resource passed to the constructor.
;;
;; If a json-writer should take ownership of the textual output port passed to the constructor,
;; then the optional argument close-out? must be truish.
;;
;; Optional argument cache must be #f or a a possibly empty eq-hashtable containing rtd -> record-info
(define make-json-writer
  (case-lambda
    ((out close-out? cache)
      (assert* 'make-json-writer (port? out))
      (assert* 'make-json-writer (textual-port? out))
      (assert* 'make-json-writer (output-port? out))
      (%make-json-writer out close-out? cache))
    ((out close-out?)
      (make-json-writer out close-out? #f))
    ((out)
      (make-json-writer out #f #f))
    (()
      (make-json-writer (current-output-port) #f #f))))


(define (json-writer-eof? tx)
  (assert* 'json-writer-eof? (json-writer? tx))
  (obj-writer-eof? tx))


(define (json-writer-close tx)
  (assert* 'json-writer-close (json-writer? tx))
  (obj-writer-close tx))


;; called by (json-writer-close) and (obj-writer-close)
(define (%json-writer-close tx)
  (let ((out (json-writer-out tx)))
    (when (json-writer-epilogue? tx)
      (put-string out "]\n")
      (json-writer-epilogue?-set! tx #f)
      (json-writer-prologue?-set! tx #t))
    ;; close out only if json-writer constructor was called with truish close-out?
    (if (json-writer-close-out? tx)
      (close-port out)
      (flush-output-port out))))


(define (write/string out str)
  ;; FIXME must escape characters #\x0 ... #\x1f with JSON syntax, not scheme syntax
  (write str out))


(define (write/symbol out sym)
  (write/string out (symbol->string sym)))


(define (write/key out key)
  (cond
    ((symbol? key) (write/symbol out key))
    ((string? key) (write/string out key))
    (else          (raise-errorf 'json-writer-put "unsupported object key: ~s" key))))


(define (write/ratio out ratio)
  (let* ((neg?  (< ratio 0))
         (ratio (if neg? (- ratio) ratio)))
    (when neg?
      (put-char out #\-))
    (let-values (((integer fraction) (div-and-mod ratio 1)))
      (write integer out)
      (put-char out #\.)
      (let* ((fraction*1e16 (div (* fraction 10000000000000000) 1))
             (fraction-string (number->string fraction*1e16))
             (fraction-string-length (string-length fraction-string)))
        (do ((i 16 (fx1- i)))
            ((fx<=? i fraction-string-length))
          (put-char out #\0))
        (let ((reduced-length
                (do ((i fraction-string-length (fx1- i)))
                    ((or (fx<=? i 0) (not (char=? #\0 (string-ref fraction-string (fx1- i)))))
                      i))))
          (string-truncate! fraction-string reduced-length)
          (put-string out fraction-string))))))


(define (write/flonum out obj)
  (let ((str (number->string obj)))
    (put-string out str)
    (unless (string-index-right str #\e)
      ;; convention: exponent means it's an inexact number,
      ;;          no exponent means it's an exact number
      (put-string out "e0"))))


(define (write/number out obj)
  (cond
    ;; do NOT use (integer? obj) because it returns #t on flonums ending with .0
    ((flonum? obj)
      (write/flonum out obj))
    ((ratnum? obj)
      (write/ratio out obj))
    (else
      (write obj out))))


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
    ((and (number? obj) (real? obj))
      ;; json does not support complex numbers
      (write/number out obj)
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


(define (put/key+value tx first? key val)
  (let ((out (json-writer-out tx)))
    (unless first?
      (put-char out #\,))
    (write/key out key)
    (put-char out #\:)
    (put/datum tx val)))


(define (put/array tx obj)
  (let ((out (json-writer-out tx)))
    (put-char out #\[)
    (do ((i 0 (fx1+ i))
         (n   (array-length obj))
         (ref (array-accessor obj)))
        ((fx>=? i n))
      (unless (fxzero? i)
        (put-char out #\,))
      (put/datum tx (ref obj i)))
    (put-char out #\])))


(define (put/bytespan tx obj)
  (write/string (json-writer-out tx) (utf8b-bytespan->string obj)))


(define (put/bytevector tx obj)
  (write/string (json-writer-out tx) (utf8b->string obj)))


(define (put/htable tx obj)
  (let ((out (json-writer-out tx)))
    (put-char out #\{)
    (let-values (((cursor next!) (htable-cursor obj)))
      (do ((first? #t #f)
           (cell   (next! cursor) (next! cursor)))
          ((not cell))
        (put/key+value tx first? (car cell) (cdr cell))))
    (put-char out #\})))


(define (put/list tx obj)
  (unless (plist? obj)
    (raise-errorf 'json-writer-put "list is not a plist: ~s" obj))
  (let ((out (json-writer-out tx)))
    (put-char out #\{)
    (do ((l obj (cddr l)))
        ((null? l))
      (put/key+value tx (eq? l obj) (car l) (cadr l)))
    (put-char out #\})))


(define (ensure-cache tx)
  (or (json-writer-cache tx)
      (let ((cache (make-eq-hashtable)))
        (json-writer-cache-set! tx cache)
        cache)))


(define (put/record tx obj)
  (let* ((cache  (ensure-cache tx))
         (iter   (json-field-cursor obj cache))
         (out    (json-writer-out tx)))
    (put-char out #\{)
    (do ((first? #t #f)
         (cell   (field-cursor-next! iter) (field-cursor-next! iter)))
        ((not cell))
      (put/key+value tx first? (car cell) ((cdr cell) obj)))
    (put-char out #\})))


(define (put/datum tx obj)
  (cond
    ((write/atomic? (json-writer-out tx) obj)
      (void))
    ((or (null? obj) (pair? obj))
      (put/list tx obj))
    ((array? obj)
      (put/array tx obj))
    ((bytespan? obj)
      (put/bytespan tx obj))
    ((bytevector? obj)
      (put/bytevector tx obj))
    ((htable? obj)
      (put/htable tx obj))
    ((record? obj)
      (put/record tx obj))
    (else
      (raise-errorf 'json-writer-put "unsupported datum: ~s" obj))))


;; called by (json-writer-put) and (obj-writer-put)
(define (%json-writer-put tx obj)
  (let ((out (json-writer-out tx)))
    (if (json-writer-prologue? tx)
      (begin
        (put-char out #\[)
        (json-writer-prologue?-set! tx #f)
        (json-writer-epilogue?-set! tx #t))
      (put-string out ",\n"))
    (put/datum tx obj)))
