;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k conversions (0 9 3))
  (export
    display-condition* display-any display-bytevector0 write-bytevector0
    any->bytevector text->bytevector text->string
    any->bytevector0 bytevector->bytevector0 text? text->bytevector0
    any->string argv->list list->argv string-hashtable->argv transcoder-utf8

    ;; unicode.ss
    char-display-wide?)
  (import
    (rnrs)
    (only (rnrs mutable-pairs)   set-car!)
    (only (chezscheme)           condition-continuation continuation-condition? fx1+ fx1- void)
    (only (scheme2k bootstrap)   assert* fx<=?* raise-assertf)
    (only (scheme2k containers)  bytespan? bytespan->bytevector bytespan->bytevector*! bytespan->bytevector0
                                 bytespan-reserve-right! bytespan-insert-right/string! bytespan-insert-right/u8!
                                 bytevector<? bytevector-index
                                 charspan? charspan-empty? charspan-index/char charspan->string charspan->utf8b charspan->utf8b/0
                                 for-hash for-list make-bytespan string-index
                                 string->utf8b string->utf8b/0 utf8b->string utf8b->string
                                 subvector-sort! utf8b-bytespan->string)
    (scheme2k conversions unicode))


(define display-condition*
  (case-lambda
    ((x)
      (display-condition* x (current-output-port)))
    ((x port)
      (when (condition? x)
        (put-string port "#<condition")
        (do ((clist (simple-conditions x) (cdr clist)))
            ((null? clist) (void))
          (let ((c (car clist)))
            (cond
              ((assertion-violation? c)       (put-string port " &assertion"))
              ((non-continuable-violation? c) (put-string port " &non-continuable"))
              ((implementation-restriction-violation? c)
                                        (put-string port " &implementation-restriction "))
              ((lexical-violation? c)   (put-string port " &lexical"))
              ((syntax-violation? c)    (put-string port " &syntax ")
                                        (put-datum  port (syntax-violation-form c))
                                        (put-string port " ")
                                        (put-datum  port (syntax-violation-subform c)))
              ((undefined-violation? c) (put-string port " &undefined"))
              ((violation? c)           (put-string port " &violation"))
              ((serious-condition? c)   (put-string port " &serious"))
              ((error? c)               (put-string port " &error"))
              ((warning? c)             (put-string port " &warning"))
              ((i/o-read-error? c)      (put-string port " &i/o-read"))
              ((i/o-write-error? c)     (put-string port " &i/o-write"))
              ((i/o-invalid-position-error? c)
                                        (put-string port " &i/o-invalid-position"))
              ((i/o-error? c)           (put-string port " &i/o"))
              ((continuation-condition? c)
                                        (put-string port " &continuation ")
                                        (put-datum  port (condition-continuation c)))
              ((message-condition? c)   (put-string port " &message ")
                                        (put-datum  port (condition-message c)))
              ((irritants-condition? c) (put-string port " &irritants ")
                                        (put-datum  port (condition-irritants c)))
              ((who-condition? c)       (put-string port " &who ")
                                        (put-datum  port (condition-who c))))))
        (put-string port ">")))))

(define (display-any x port)
  (if (condition? x)
    (display-condition* x port)
    (display x port)))

;; convert a bytevector0 to string using UTF-8b
;; and print it quoted, i.e. surrounded by "" and with special characters escaped
(define (write-bytevector0 x port)
  (let ((str (utf8b->string x 0 (fx1- (bytevector-length x)))))
    (write str port)))

;; convert a 0-terminated bytevector to string using UTF-8b
;; and print it unquoted, i.e. not surrounded by ""
(define (display-bytevector0 x port)
  (let ((str (utf8b->string x 0 (fx1- (bytevector-length x)))))
    (display str port)))

;; convert any value to a string
(define (any->string x)
  (cond ((string? x) x)
        ((bytevector? x) (utf8b->string x))
        ((eq? (void) x) "")
        (else
          (let-values (((port get-string)
                       (open-string-output-port)))
            (display-any x port)
            (get-string)))))

(define transcoder-utf8 (make-transcoder (utf-8-codec) (eol-style lf)
                          (error-handling-mode raise)))

;; convert any value to a bytevector
(define (any->bytevector x)
  (cond
    ((bytevector? x) x)
    ((string? x)     (string->utf8b x))
    ((char? x)       (string->utf8b (string x)))
    ((eq? (void) x)  #vu8())
    (else
      (let-values (((port get-bytevector)
                   (open-bytevector-output-port transcoder-utf8)))
        (display-any x port)
        (get-bytevector)))))

;; convert any sequence of values to 0-terminated bytevector
(define (any->bytevector0 . args)
  (let-values (((port get-bytevector)
                (open-bytevector-output-port transcoder-utf8)))
    (for-list ((e args))
      (cond
        ; suboptimal: this performs a lossless roundtrip
        ; bytevector -> string -> bytevector using UTF-8b decoding/enconding
        ((bytevector? e) (put-string port (utf8b->string e)))
        ((string? e)     (put-string port e))
        ((eq? (void) e)  #f)
        (else            (display-any e port))))
    (put-char port #\nul)
    (get-bytevector)))

;; convert bytevector to 0-terminated bytevector
;;
;; returned bytevector can be modified,
;; but note that it can share data with original bytevector.
(define bytevector->bytevector0
  (case-lambda
    ((bv start end)
      (assert* 'bytevector->bytevector0 (fx<=?* 0 start end (bytevector-length bv)))
      (cond
        ((fx=? start end)
          (make-bytevector 1 0))
        ((fxzero? (bytevector-u8-ref bv (fx1- end)))
          bv)
        (else
          (let* ((len (fx- end start))
                 (ret (make-bytevector (fx1+ len))))
            (bytevector-copy! bv start ret 0 len)
            (bytevector-u8-set! ret len 0)
            ret))))
    ((bv)
      (bytevector->bytevector0 bv 0 (bytevector-length bv)))))


;; return #t if x is a bytevector, bytespan, string or charspan,
;; otherwise return #f
(define (text? x)
  (or (bytevector? x) (string? x) (bytespan? x) (charspan? x)))


;; convert a bytevector, bytespan, string or charspan to bytevector, then appends a byte 0.
;; uses UTF-8b to convert characters to bytes.
;;
;; returned bytevector can be modified,
;; but note that it can share data with original bytevector.
(define (text->bytevector0 x)
  (cond
    ((bytevector? x)
       (bytevector->bytevector0 x))
    ((string? x)
       (if (fxzero? (string-length x))
         #vu8(0)
         (string->utf8b/0 x)))
    ((bytespan? x)
       (bytespan->bytevector0 x))
    ((charspan? x)
       (if (charspan-empty? x)
         #vu8(0)
         (bytespan->bytevector*! (charspan->utf8b/0 x))))
    (else
      (raise-assertf 'text->bytevector0 "~s is not bytevector, bytespan, string or charspan" x))))


;; convert a bytevector, string or charspan to bytevector.
;; uses UTF-8b to convert characters to bytes.
(define (text->bytevector x)
  (cond
    ((bytevector? x)
      x)
    ((string? x)
      (if (fxzero? (string-length x))
        #vu8()
        (string->utf8b x)))
    ((charspan? x)
      (if (charspan-empty? x)
        #vu8()
        (bytespan->bytevector (charspan->utf8b x))))
    (else
      (raise-assertf 'text->bytevector "~s is not bytevector, string or charspan" x))))



;; convert a bytevector, string or charspan to string.
;; uses UTF-8b to convert bytes to characters.
(define (text->string x)
  (cond
    ((bytevector? x)
      (utf8b->string x))
    ((string? x)
      x)
    ((bytespan? x)
      (utf8b-bytespan->string x))
    ((charspan? x)
      (charspan->string x))
    (else
      (raise-assertf 'text->bytevector "~s is not bytevector, string or charspan" x))))


;; convert a 0-terminated bytevector containing UTF-8b to string
(define (bytevector0->string x)
  (utf8b->string x 0 (fx1- (bytevector-length x))))


;; convert a list of strings, bytevectors, bytespans or charspans to vector-of-bytevector0
;; i.e. to a vector of 0-terminated UTF-8b bytevectors
;;
;; Note: throws if a string, bytevector or charspan to be converted contains #\nul
(define (list->argv l)
  (let ((argv (list->vector l)))
    (do ((i 0 (fx1+ i)))
        ((>= i (vector-length argv)) argv)
      (vector-set! argv i (text->bytevector0 (validate-c-arg (vector-ref argv i)))))))


;; throws if string, bytevector or charspan x contains #\nul
(define (validate-c-arg x)
  (let ((msg1 "string arguments for C functions must not contain embedded #\\nul\n\tFound: ")
        (msg2 "\n\tConsider using the builtin \"split-at-0\""))
    (cond
      ((bytevector? x)
        (when (bytevector-index x 0)
          (raise-assertf 'list->argv "~a(string->utf8b ~s)~a" msg1 (utf8b->string x) msg2)))
      ((string? x)
        (when (string-index x #\nul)
          (raise-assertf 'list->argv "~a~s~a" msg1 x msg2)))
      ((charspan? x)
        (when (charspan-index/char x #\nul)
          (raise-assertf 'list->argv "~a~s~a" msg1 x msg2)))
      (else
        (raise-assertf 'list->argv "~s is not bytevector, string or charspan" x)))
    x))


;; convert two strings key and val to a bytevector0 containing key=value\x0;
(define (key-value->bytevector0 key val)
  (let ((bsp (make-bytespan 0)))
    (bytespan-reserve-right! bsp (fx+ 2 (fx+ (string-length key) (string-length val))))
    (bytespan-insert-right/string! bsp key)
    (bytespan-insert-right/u8!     bsp 61) ; #\=
    (bytespan-insert-right/string! bsp val)
    (bytespan-insert-right/u8!     bsp 0)  ; #\x0
    (bytespan->bytevector*! bsp)))


;; convert a hashtable containing string keys and string values
;; to a sorted vector of bytevector0, where each element is key=value\x0;
(define (string-hashtable->argv htable)
  (let* ((i 0)
         (n (hashtable-size htable))
         (vec (make-vector n)))
    (for-hash ((key val htable))
      (vector-set! vec i (key-value->bytevector0 key val))
      (set! i (fx1+ i)))
    (subvector-sort! bytevector<? vec)
    vec))


;; convert a vector of 0-terminated UTF-8b bytevectors
;; to a list of strings,
(define (argv->list argv)
  (let ((l (vector->list argv)))
    (do ((tail l (cdr tail)))
        ((null? tail) l)
      (set-car! tail (bytevector0->string (car tail))))))


)
