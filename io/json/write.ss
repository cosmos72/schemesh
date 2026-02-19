;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/json/json.ss


(define-record-type (json-writer %make-json-writer json-writer?)
  (parent writer)
  (fields
    out                   ; binary output port
    wbuf                  ; bytespan, write buffer
    (mutable cache)       ; #f or eq-hashtable rtd -> reflect-info, set in construction or created lazily
    (mutable prologue?)   ; #t before first call to any (json-writer-put...) function
    (mutable epilogue?)   ; #t if we should write #\] before closing the port
    close-out?)           ; boolean, #t if closing the json-writer must close the underlying binary output port
  (protocol
    (lambda (args->new)
      (lambda (out close-out? cache)
        ((args->new %json-writer-put %json-writer-close)
          out (bytespan) #f #t #f (and close-out? #t)))))
  (nongenerative %json-writer-7c46d04b-34f4-4046-b5c7-b63753c1be43))


;; Create a json-writer that, at each call to one of
;;   (writer-put) (json-writer-put-value) or (json-writer-put-token),
;; serializes the received data in streaming mode,
;; and writes it to the underlying binary output port.
;;
;; Note: as per writer contract, by default closing a json-writer does NOT close the underlying binary output port,
;; because it is a pre-existing, borrowed resource passed to the constructor.
;;
;; If a json-writer should take ownership of the binary output port passed to the constructor,
;; then the optional argument close-out? must be truish.
;;
;; Optional argument cache must be #f or a possibly empty eq-hashtable containing rtd -> reflect-info
(define make-json-writer
  (case-lambda
    ((out close-out? cache)
      (assert* 'make-json-writer (port? out))
      (assert* 'make-json-writer (binary-port? out))
      (assert* 'make-json-writer (output-port? out))
      (%make-json-writer out close-out? cache))
    ((out close-out?)
      (make-json-writer out close-out? #f))
    ((out)
      (make-json-writer out #f #f))
    (()
      (make-json-writer (sh-stdout) #f #f))))


;; called by (json-writer-close) and (writer-close)
(define (%json-writer-close tx)
  (let ((out (json-writer-out tx)))
    (when (json-writer-epilogue? tx)
      (put-bytevector out #vu8(93 10)) ; #\] #\newline
      (json-writer-epilogue?-set! tx #f)
      (json-writer-prologue?-set! tx #t))
    ;; close out only if json-writer constructor was called with truish close-out?
    (if (json-writer-close-out? tx)
      (close-port out)
      (flush-output-port out))))


(define (put-bytespan out wbuf)
  (put-bytevector out (bytespan-peek-data wbuf) (bytespan-peek-beg wbuf) (bytespan-length wbuf)))


(define (u4-to-hex-digit-byte u4)
  (if (fx<? u4 10)
    (fx+ u4 48)   ; #\0 ... #\9
    (fx+ u4 87))) ; #\a ... #\f


(define (write/string out wbuf str)
  ;; escape characters #\x0 ... #\x1f with JSON syntax, not scheme syntax
  ;; also escape #\" and #\\
  (bytespan-clear! wbuf)
  (bytespan-insert-right/u8! wbuf 34) ; #\"
  (do ((i 0 (fx1+ i))
       (n (string-length str)))
      ((fx>=? i n))
    (let ((ch (string-ref str i)))
      (cond
        ((char>=? ch #\space)
          (when (or (eqv? ch #\") (eqv? ch #\\))
            (bytespan-insert-right/u8! wbuf 92)) ; #\\
          (bytespan-insert-right/char! wbuf ch))
        ((eqv? ch #\backspace)
          (bytespan-insert-right/u8! wbuf 92)   ; #\\
          (bytespan-insert-right/u8! wbuf 98))  ; #\b
        ((eqv? ch #\tab)
          (bytespan-insert-right/u8! wbuf 92)   ; #\\
          (bytespan-insert-right/u8! wbuf 116)) ; #\t
        ((eqv? ch #\newline)
          (bytespan-insert-right/u8! wbuf 92)   ; #\\
          (bytespan-insert-right/u8! wbuf 110)) ; #\n
        ((eqv? ch #\page)
          (bytespan-insert-right/u8! wbuf 92)   ; #\\
          (bytespan-insert-right/u8! wbuf 102)) ; #\f
        ((eqv? ch #\return)
          (bytespan-insert-right/u8! wbuf 92)   ; #\\
          (bytespan-insert-right/u8! wbuf 114)) ; #\r
        (else
          (bytespan-insert-right/bytevector! wbuf #vu8(92 117 48 48)) ; #\\ #\u #\0 #\0
          (let ((u8 (char->integer ch)))
            (bytespan-insert-right/u8! wbuf (u4-to-hex-digit-byte (fxarithmetic-shift-right u8 4)))
            (bytespan-insert-right/u8! wbuf (u4-to-hex-digit-byte (fxand u8 #xf))))))))
  (bytespan-insert-right/u8! wbuf 34) ; #\"
  (put-bytespan out wbuf))


(define (write/symbol out wbuf sym)
  (write/string out wbuf (symbol->string sym)))


(define (write/key out wbuf key)
  (cond
    ((symbol? key) (write/symbol out wbuf key))
    ((string? key) (write/string out wbuf key))
    (else          (raise-errorf 'json-writer-put "unsupported object key: ~s" key))))

(define (write/ratio out wbuf ratio)
  (bytespan-clear! wbuf)
  (let* ((neg?  (< ratio 0))
         (ratio (if neg? (- ratio) ratio)))
    (when neg?
      (bytespan-insert-right/u8! wbuf 45)) ; #\-
    (let-values (((integer fraction) (div-and-mod ratio 1)))
      (bytespan-display-right/integer! wbuf integer)
      (unless (zero? fraction)
        (let ((fraction*1e16 (div (* fraction 10000000000000000) 1)))
          (unless (zero? fraction*1e16)
            (bytespan-insert-right/u8! wbuf 46) ; #\.
            (bytespan-display-right/unsigned-k-digits! wbuf fraction*1e16 16)
            ;; remove least significant zeroes
            (do ()
                ((not (fx=? 48 (bytespan-ref-right/u8 wbuf))))
              (bytespan-delete-right! wbuf 1)))))))
  (put-bytespan out wbuf))


(define (write/flonum out wbuf obj)
  (bytespan-clear! wbuf)
  (let ((str (number->string obj)))
    (bytespan-insert-right/string! wbuf str)
    (unless (string-index-right str #\e)
      ;; convention: exponent means it's an inexact number,
      ;;          no exponent means it's an exact number
      (bytespan-insert-right/bytevector! wbuf #vu8(101 48)))) ; #\e #\0
  (put-bytespan out wbuf))


(define (write/number out wbuf obj)
  (cond
    ;; do NOT use (integer? obj) because it returns #t on flonums ending with .0
    ((flonum? obj)
      (write/flonum out wbuf obj))
    ((ratnum? obj)
      (write/ratio out wbuf obj))
    (else
      (bytespan-clear! wbuf)
      (bytespan-display-right/integer! wbuf obj)
      (put-bytespan out wbuf))))


;; if obj is a supported atomic value, write it to out and return #t.
;; otherwise return #f.
(define (write/atomic? out wbuf obj)
  (cond
    ((boolean? obj)
      (put-bytevector out (if obj #vu8(116 114 117 101)       ; "true"
                                  #vu8(102 97 108 115 101))) ; "false"
      #t)
    ((eq? obj (void))
      (put-bytevector out #vu8(110 117 108 108)) ; "null"
      #t)
    ((symbol? obj)
      (write/symbol out wbuf obj)
      #t)
    ((string? obj)
      (write/string out wbuf obj)
      #t)
    ((and (number? obj) (real? obj))
      ;; json does not support complex numbers
      (write/number out wbuf obj)
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
        (put-u8 out (char->integer tok)))
      ((eof-object? tok)
        (void))
      ((write/atomic? out (json-writer-wbuf tx) tok)
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
  (let ((out  (json-writer-out tx))
        (wbuf (json-writer-wbuf tx)))
    (cond
      ((null? obj)
        (put-bytevector out #vu8(123 125))) ; "{}"

      ((pair? obj)
        (unless (plist? obj)
          (raise-errorf 'json-writer-put-value "list is not a plist: ~s" obj))
        (put-u8 out 123) ; #\{
        (do ((l obj (cddr l)))
            ((null? l))
          (unless (eq? l obj)
            (put-u8 out 44)) ; #\,
          (let ((key (car l)))
            (unless (symbol? key)
              (raise-errorf 'json-writer-put-value "unsupported json object key: ~s" key))
            (write/symbol out wbuf key)
            (put-u8 out 58) ; #\:
            (json-writer-put-value tx (cadr l))))
        (put-u8 out 125)) ; #\}

      ((record? obj)
        (unless (span? obj)
          (raise-errorf 'json-writer-put-value "unsupported json array: ~s" obj))
        (put-u8 out 91) ; #\[
        (do ((i 0 (fx1+ i))
             (n (span-length obj)))
            ((fx>=? i n))
          (unless (fxzero? i)
            (put-u8 out 44)) ; #\,
          (json-writer-put-value tx (span-ref obj i)))
        (put-u8 out 93)) ; #\]

      ((write/atomic? out wbuf obj)
        (void))

      (else
        (raise-errorf 'json-writer-put-value "unsupported datum: ~s" obj)))

    (json-writer-prologue?-set! tx #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; serialize scheme datum to json


(define (put/key+value tx first? key val)
  (let ((out (json-writer-out tx)))
    (unless first?
      (put-u8 out 44)) ; #\,
    (write/key out (json-writer-wbuf tx) key)
    (put-u8 out 58) ; #\:
    (put/datum tx val)))


(define (put/array tx obj)
  (let ((out (json-writer-out tx)))
    (put-u8 out 91) ; #\[
    (do ((i 0 (fx1+ i))
         (n   (array-length obj))
         (ref (array-accessor obj)))
        ((fx>=? i n))
      (unless (fxzero? i)
        (put-u8 out 44)) ; #\,
      (put/datum tx (ref obj i)))
    (put-u8 out 93))) ; #\]


(define (put/bytespan tx obj)
  ;; FIXME remove roundtrip bytespan -> string -> bytespan
  (write/string (json-writer-out tx) (json-writer-wbuf tx) (utf8b-bytespan->string obj)))


(define (put/bytevector tx obj)
  ;; FIXME remove roundtrip bytespan -> string -> bytespan
  (write/string (json-writer-out tx) (json-writer-wbuf tx) (utf8b->string obj)))


(define (put/htable tx obj)
  (let ((out (json-writer-out tx))
        (first? #t))
    (put-u8 out 123) ; #\{
    (for ((k v (in-htable obj)))
      (put/key+value tx first? k v)
      (set! first? #f))
    (put-u8 out 125))) ; #\}


(define (put/list tx obj)
  (unless (plist? obj)
    (raise-errorf 'json-writer-put "list is not a plist: ~s" obj))
  (let ((out (json-writer-out tx)))
    (put-u8 out 123) ; #\{
    (do ((l obj (cddr l)))
        ((null? l))
      (put/key+value tx (eq? l obj) (car l) (cadr l)))
    (put-u8 out 125))) ; #\}


(define (ensure-cache tx)
  (or (json-writer-cache tx)
      (let ((cache (make-eq-hashtable)))
        (json-writer-cache-set! tx cache)
        cache)))


(define (put/record tx obj)
  (let ((seq  (json-in-fields obj (ensure-cache tx)))
        (out  (json-writer-out tx)))
    (put-u8 out 123) ; #\{
    (let %put/record ((seq seq) (first? #t))
      (let-values (((key value ok?) (seq)))
        (when ok?
          (put/key+value tx first? key value)
          (%put/record seq #f))))
    (put-u8 out 125))) ; #\}


(define (put/datum tx obj)
  (cond
    ((write/atomic? (json-writer-out tx) (json-writer-wbuf tx) obj)
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


;; called by (json-writer-put) and (writer-put)
(define (%json-writer-put tx obj)
  (let ((out (json-writer-out tx)))
    (if (json-writer-prologue? tx)
      (begin
        (put-u8 out 91) ; #\[
        (json-writer-prologue?-set! tx #f)
        (json-writer-epilogue?-set! tx #t))
      (put-bytevector out #vu8(44 10))) ; ",\n"
    (put/datum tx obj)))
