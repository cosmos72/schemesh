;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh conversions (0 7 1))
  (export
    display-condition* display-any display-bytevector0 write-bytevector0
    any->bytevector text->bytevector
    any->bytevector0 bytevector->bytevector0 text->bytevector0
    any->string argv->list list->argv string-hashtable->argv transcoder-utf8
    sh-eval->bytevector)
  (import
    (rnrs)
    (only (rnrs mutable-pairs)   set-car!)
    (only (chezscheme)           condition-continuation continuation-condition? fx1+ fx1- void)
    (only (schemesh bootstrap)   raise-assertv sh-eval-string)
    (only (schemesh containers)  bytespan->bytevector charspan? charspan-empty? charspan->utf8b charspan->utf8b/0
                                 hashtable-iterate list-iterate
                                 string->utf8b string->utf8b/0 utf8b->string utf8b-range->string))


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
  (let ((str (utf8b-range->string x 0 (fx1- (bytevector-length x)))))
    (write str port)))

;; convert a 0-terminated bytevector to string using UTF-8b
;; and print it unquoted, i.e. not surrounded by ""
(define (display-bytevector0 x port)
  (let ((str (utf8b-range->string x 0 (fx1- (bytevector-length x)))))
    (display str port)))

;; convert any value to a string
(define (any->string x)
  (cond ((string? x) x)
        ((bytevector? x) (utf8b->string x))
        ((eq? (void) x) "")
        (#t (let-values (((port get-string)
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
    (#t (let-values (((port get-bytevector)
                      (open-bytevector-output-port transcoder-utf8)))
          (display-any x port)
          (get-bytevector)))))

;; convert any sequence of values to 0-terminated bytevector
(define (any->bytevector0 . args)
  (let-values (((port get-bytevector)
                (open-bytevector-output-port transcoder-utf8)))
    (list-iterate args
      (lambda (e)
        (cond
          ; suboptimal: this performs a lossless roundtrip
          ; bytevector -> string -> bytevector using UTF-8b decoding/enconding
          ((bytevector? e) (display (utf8b->string e) port))
          ((string? e)     (display e port))
          ((eq? (void) e)  #f)
          (#t              (display-any e port)))))
    (display #\nul port)
    (get-bytevector)))

(define bv0 #vu8(0))

;; convert bytevector to 0-terminated bytevector
(define (bytevector->bytevector0 x)
  (let ((len (bytevector-length x)))
    (cond
      ((fxzero? len)
        bv0)
      ((fxzero? (bytevector-u8-ref x (fx1- len)))
        x)
      (#t
        (let ((ret (make-bytevector (fx1+ len))))
          (bytevector-copy! x 0 ret 0 len)
          (bytevector-u8-set! ret len 0)
          ret)))))

;; convert a bytevector, string or charspan to terminated bytevector, then appends a byte 0.
;; uses UTF-8b to convert characters to bytes.
(define (text->bytevector0 x)
  (cond
    ((bytevector? x)
       (bytevector->bytevector0 x))
    ((string? x)
       (if (fxzero? (string-length x))
         bv0
         (string->utf8b/0 x)))
    ((charspan? x)
       (if (charspan-empty? x)
         bv0
         (bytespan->bytevector (charspan->utf8b/0 x))))
    (#t
      (raise-assertv 'text->bytevector0 '(or (bytevector? x) (string? x) (charspan? x)) x))))


;; convert a bytevector, string or charspan to bytevector
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
    (#t
      (raise-assertv 'text->bytevector '(or (bytevector? x) (string? x) (charspan? x)) x))))


;; convert a 0-terminated bytevector containing UTF-8b to string
(define (bytevector0->string x)
  (utf8b-range->string x 0 (fx1- (bytevector-length x))))


;; convert a list of strings or UTF-8b bytevectors to vector-of-bytevector0
;; i.e. to a vector of 0-terminated UTF-8b bytevectors
(define (list->argv l)
  (let ((argv (list->vector l)))
    (do ((i 0 (fx1+ i)))
        ((>= i (vector-length argv)) argv)
      (vector-set! argv i (text->bytevector0 (vector-ref argv i))))))


;; convert a vector of 0-terminated UTF-8b bytevectors
;; to a list of strings,
(define (argv->list argv)
  (let ((l (vector->list argv)))
    (do ((tail l (cdr tail)))
        ((null? tail) l)
      (set-car! tail (bytevector0->string (car tail))))))


;; convert a hashtable containing string keys and string values
;; to a vector of bytevector0, where each element is key=value\x0;
(define (string-hashtable->argv htable)
  (let* ((i 0)
         (n (hashtable-size htable))
         (out (make-vector n)))
    (hashtable-iterate htable
      (lambda (cell)
        (let ((key (car cell))
              (val (cdr cell)))
          (vector-set! out i (any->bytevector0 key "=" val))
          (set! i (fx1+ i)))))
    out))

(define (sh-eval->bytevector str)
  (any->bytevector (sh-eval-string str)))

)
