;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  define Scheme type "chargbuffer", a gap buffer containing chars. ;;;;;;;
;;;;; Implementation: contains two charspans, a "left" and a "right" ones ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (schemesh containers chargbuffer (0 1))
  (export
    list->chargbuffer string->chargbuffer string->chargbuffer* charspan->chargbuffer charspan->chargbuffer*
    make-chargbuffer chargbuffer chargbuffer? chargbuffer->charspan chargbuffer->string
    chargbuffer-length chargbuffer-empty?
    chargbuffer-ref chargbuffer-set! chargbuffer-clear! chargbuffer-split-at!
    chargbuffer-insert-at! chargbuffer-insert-at/cspan! chargbuffer-insert-at/cbuf!
    chargbuffer-erase-at! chargbuffer-iterate)
  (import
    (rnrs)
    (only (chezscheme) fx1+ record-writer string-copy! void)
    (only (schemesh bootstrap) assert*)
    (schemesh containers charspan))

(define-record-type
  (%chargbuffer %make-chargbuffer chargbuffer?)
  (fields
     (mutable left  chargbuffer-left  chargbuffer-left-set!)
     (mutable right chargbuffer-right chargbuffer-right-set!))
  (nongenerative #{%chargbuffer itah4n3k0nl66ucaakkpqk55m-16}))

(define (list->chargbuffer l)
  (%make-chargbuffer (charspan) (list->charspan l)))

(define (string->chargbuffer str)
  (%make-chargbuffer (charspan) (string->charspan str)))

; view a string as chargbuffer
(define (string->chargbuffer* str)
  (%make-chargbuffer (charspan) (string->charspan* str)))

(define (charspan->chargbuffer csp)
  (%make-chargbuffer (charspan) (charspan-copy csp)))

; view a charspan as chargbuffer
(define (charspan->chargbuffer* csp)
  (%make-chargbuffer (charspan) csp))

(define make-chargbuffer
  (case-lambda
    ((n)      (%make-chargbuffer (charspan) (make-charspan n)))
    ((n fill) (%make-chargbuffer (charspan) (make-charspan n fill)))))

(define (chargbuffer->string gb)
  (let* ((left    (chargbuffer-left  gb))
         (right   (chargbuffer-right gb))
         (left-n  (charspan-length left))
         (right-n (charspan-length right))
         (dst (make-string (fx+ left-n right-n))))
    (string-copy! (charspan-peek-data left)  (charspan-peek-beg left)
                  dst 0 left-n)
    (string-copy! (charspan-peek-data right) (charspan-peek-beg right)
                  dst left-n right-n)
    dst))

(define (chargbuffer->charspan gb)
  (string->charspan* (chargbuffer->string gb)))

(define (chargbuffer . vals)
  (list->chargbuffer vals))

(define (chargbuffer-length gb)
  (fx+ (charspan-length (chargbuffer-left gb)) (charspan-length (chargbuffer-right gb))))

(define (chargbuffer-empty? gb)
  (and (charspan-empty? (chargbuffer-left gb)) (charspan-empty? (chargbuffer-right gb))))

(define (chargbuffer-ref gb idx)
  (assert* 'chargbuffer-ref (fx<? -1 idx (chargbuffer-length gb)))
  (let ((left-n (charspan-length (chargbuffer-left gb))))
    (if (fx<? idx left-n)
      (charspan-ref (chargbuffer-left  gb) idx)
      (charspan-ref (chargbuffer-right gb) (fx- idx left-n)))))

(define (chargbuffer-set! gb idx ch)
  (assert* 'chargbuffer-set! (fx<? -1 idx (chargbuffer-length gb)))
  (let ((left-n (charspan-length (chargbuffer-left gb))))
    (if (fx<? idx left-n)
      (charspan-set! (chargbuffer-left  gb) idx ch)
      (charspan-set! (chargbuffer-right gb) (fx- idx left-n) ch))))

(define (chargbuffer-clear! gb)
  (charspan-clear! (chargbuffer-left  gb))
  (charspan-clear! (chargbuffer-right gb)))

(define (chargbuffer-split-at! gb idx)
  (assert* 'chargbuffer-split-at! (fx<=? 0 idx (chargbuffer-length gb)))
  (let* ((left  (chargbuffer-left  gb))
         (right (chargbuffer-right gb))
         (delta (fx- idx (charspan-length left))))
    (cond
      ((fx>? delta 0)
        (charspan-insert-back/cspan! left right 0 delta)
        (charspan-erase-front! right delta))
      ((fx<? delta 0)
        (charspan-insert-front/cspan! right left idx (fx- delta))
        (charspan-erase-back! left (fx- delta))))))

; insert one char into chargbuffer at position idx
(define (chargbuffer-insert-at! gb idx ch)
  (assert* 'chargbuffer-insert-at! (fx<=? 0 idx (chargbuffer-length gb)))
  (let* ((left   (chargbuffer-left  gb))
         (right  (chargbuffer-right gb))
         (left-n (charspan-length left))
         (delta  (fx- idx left-n)))
    (cond
      ((fxzero? idx)
        (charspan-insert-front! left ch))
      ((fx=? idx (chargbuffer-length gb))
        (charspan-insert-back! right ch))
      (#t
        (chargbuffer-split-at! gb idx)
        (charspan-insert-back! left ch)))))

; read src-n elements from charspan csp-src starting from src-start
; and insert them into chargbuffer at position idx
(define (chargbuffer-insert-at/cspan! gb idx csp-src src-start src-n)
  (assert* 'chargbuffer-insert-at/cspan! (fx<=? 0 idx       (chargbuffer-length gb)))
  (assert* 'chargbuffer-insert-at/cspan! (fx<=? 0 src-start (charspan-length csp-src)))
  (assert* 'chargbuffer-insert-at/cspan! (fx<=? 0 (fx+ src-start src-n) (charspan-length csp-src)))
  (let* ((left   (chargbuffer-left  gb))
         (right  (chargbuffer-right gb))
         (left-n (charspan-length left))
         (delta  (fx- idx left-n)))
    (cond
      ((fxzero? src-n) ; nothing to do
        (void))
      ((fxzero? idx)
        (charspan-insert-front/cspan! left csp-src src-start src-n))
      ((fx=? idx (chargbuffer-length gb))
        (charspan-insert-back/cspan! right csp-src src-start src-n))
      (#t
        (chargbuffer-split-at! gb idx)
        (charspan-insert-back/cspan! left csp-src src-start src-n)))))

; read src-n elements from chargbuffer gb-src starting from src-start
; and insert them into chargbuffer at position idx
(define (chargbuffer-insert-at/cbuf! gb idx gb-src src-start src-n)
  (assert* 'chargbuffer-insert-at/cbuf! (fx<=? 0 idx       (chargbuffer-length gb)))
  (assert* 'chargbuffer-insert-at/cbuf! (fx<=? 0 src-start (chargbuffer-length gb-src)))
  (assert* 'chargbuffer-insert-at/cbuf! (fx<=? 0 (fx+ src-start src-n) (chargbuffer-length gb-src)))
  (when (fx>? src-n 0)
    (let* ((left   (chargbuffer-left  gb-src))
           (right  (chargbuffer-right gb-src))
           (left-n (charspan-length left)))
      (when (fx<? src-start left-n)
        (let ((delta (fxmin src-n (fx- left-n src-start))))
          (chargbuffer-insert-at/cspan! gb idx left src-start delta)
          (set! idx (fx+ idx delta))
          (set! src-start (fx+ src-start delta))
          (set! src-n (fx- src-n delta))))
      (when (fx>? src-n 0)
        (chargbuffer-insert-at/cspan! gb idx right (fx- src-start left-n) src-n)))))

; remove n elements from chargbuffer starting at start
(define (chargbuffer-erase-at! gb start n)
  (let* ((left    (chargbuffer-left  gb))
         (right   (chargbuffer-right gb))
         (left-n  (charspan-length left))
         (right-n (charspan-length right))
         (len     (fx+ left-n right-n))
         (end     (fx+ start n)))
    (assert* 'chargbuffer-erase-at! (fx<=? 0 start len))
    (assert* 'chargbuffer-erase-at! (fx<=? 0 n (fx- len start)))
    (cond
      ((fxzero? n) (void)) ; nothing to do
      ((fxzero? start)
        (let ((head (fxmin n left-n)))
          (charspan-erase-front! left head)
          (charspan-erase-front! right (fx- n head))))
      ((fx=? end left-n)
        (charspan-erase-back! left n))
      ((fx=? start left-n)
        (charspan-erase-front! right n))
      ((fx=? end len)
        (let ((tail (fxmin n right-n)))
          (charspan-erase-back! right tail)
          (charspan-erase-back! left (fx- n tail))))
      (#t
        (chargbuffer-split-at! gb end)
        (charspan-erase-back! left n)))))

(define (chargbuffer-iterate gb proc)
  (do ((i 0 (fx1+ i))
       (n (chargbuffer-length gb)))
    ((or (fx>=? i n) (not (proc i (chargbuffer-ref gb i)))))))

; customize how "chargbuffer" objects are printed
(record-writer (record-type-descriptor %chargbuffer)
  (lambda (gb port writer)
    (display "(string->chargbuffer* " port)
    (write (chargbuffer->string gb) port)
    (display ")" port)))

) ; close library
