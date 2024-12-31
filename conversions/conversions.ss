;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh conversions (0 1))
  (export
    display-condition* display-any display-bytevector0 write-bytevector0
    any->bytevector any->bytevector0 bytevector->bytevector0 text->bytevector0
    any->string argv->list list->argv string-hashtable->argv transcoder-utf8
    sh-eval->bytevector)
  (import
    (rnrs)
    (only (rnrs mutable-pairs)   set-car!)
    (only (chezscheme)           fx1+ fx1- void)
    (only (schemesh bootstrap)   assert* sh-eval-string)
    (only (schemesh containers)  hashtable-iterate list-iterate string->utf8b string->utf8b/0
                                 utf8b->string utf8b-range->string))


(define (display-condition* x port)
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
          ((i/o-read-error? c)      (put-string port " &i/o-read"))
          ((i/o-write-error? c)     (put-string port " &i/o-write"))
          ((i/o-invalid-position-error? c)
                                    (put-string port " &i/o-invalid-position"))
          ;         more i/o errors ...
          ((i/o-error? c)           (put-string port " &i/o"))
          ((error? c)               (put-string port " &error"))
          ((warning? c)             (put-string port " &warning"))
          ((message-condition? c)   (put-string port " &message ")
                                    (put-datum  port (condition-message c)))
          ((irritants-condition? c) (put-string port " &irritants ")
                                    (put-datum  port (condition-irritants c)))
          ((who-condition? c)       (put-string port " &who ")
                                    (put-datum  port (condition-who c)))
          ((serious-condition? c)   (put-string port " &serious")))))
    (put-string port ">")))

(define (display-any x port)
  (if (condition? x)
    (display-condition* x port)
    (display x port)))

; convert to string a bytevector0 containing UTF-8b
; and print it quoted, i.e. surrounded by "" and with special characters escaped
(define (write-bytevector0 x port)
  (let ((str (utf8b-range->string x 0 (fx1- (bytevector-length x)))))
    (write str port)))

; convert to string a bytevector0 containing UTF-8b
; and print it unquoted, i.e. surrounded by ""
(define (display-bytevector0 x port)
  (let ((str (utf8b-range->string x 0 (fx1- (bytevector-length x)))))
    (display str port)))

; convert any value to a string
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

; convert any value to a bytevector
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

; convert any sequence of values to #\nul terminated bytevector
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

; convert bytevector to #\nul terminated bytevector
(define (bytevector->bytevector0 x)
  (let ((len (bytevector-length x)))
    (if (or (fxzero? len) (fxzero? (bytevector-u8-ref x (fx1- len))))
      bv0
      (let ((ret (make-bytevector (fx1+ len))))
        (bytevector-copy! x 0 ret 0 len)
        (bytevector-u8-set! ret len 0)
        ret))))

; convert string or bytevector to #\nul terminated bytevector containing UTF-8b
(define (text->bytevector0 x)
  (cond
    ((bytevector? x)
       (bytevector->bytevector0 x))
    ((string? x)
       (if (fxzero? (string-length x))
         bv0
         (string->utf8b/0 x)))
    (#t (assert* 'text->bytevector0 (string? x)))))


; convert a #\nul terminated bytevector containing UTF-8b to string
(define (bytevector0->string x)
  (utf8b-range->string x 0 (fx1- (bytevector-length x))))


; convert a list of strings or bytevectors to vector-of-bytevector0
; i.e. to a vector of #\nul terminated UTF-8b bytevectors
(define (list->argv l)
  (let ((argv (list->vector l)))
    (do ((i 0 (fx1+ i)))
        ((>= i (vector-length argv)) argv)
      (vector-set! argv i (text->bytevector0 (vector-ref argv i))))))


; convert a vector of #\nul terminated UTF-8b bytevectors
; to a list of strings
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
