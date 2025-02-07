;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  define Scheme type "chargbuffer", a gap buffer containing chars. ;;;;;;;
;;;;; Implementation: contains two charspans, a "left" and a "right" ones ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (schemesh containers chargbuffer (0 7 3))
  (export
    list->chargbuffer string->chargbuffer string->chargbuffer* charspan->chargbuffer charspan->chargbuffer*
    make-chargbuffer chargbuffer chargbuffer? chargbuffer->charspan chargbuffer->string
    chargbuffer-length chargbuffer-empty?
    chargbuffer-ref chargbuffer-set! chargbuffer-clear! chargbuffer-split-at!
    chargbuffer-insert-at! chargbuffer-insert-at/cspan! chargbuffer-insert-at/cbuf!
    chargbuffer-erase-range! chargbuffer-iterate)
  (import
    (rnrs)
    (only (chezscheme) fx1+ record-writer string-copy! void)
    (only (schemesh bootstrap) assert* -> ^)
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
  (fx+ (-> gb chargbuffer-left charspan-length) (-> gb chargbuffer-right charspan-length)))

(define (chargbuffer-empty? gb)
  (and (-> gb chargbuffer-left charspan-empty?) (-> gb chargbuffer-right charspan-empty?)))

(define (chargbuffer-ref gb idx)
  (assert* 'chargbuffer-ref (fx<? -1 idx (chargbuffer-length gb)))
  (let ((left-n (-> gb chargbuffer-left charspan-length)))
    (if (fx<? idx left-n)
      (-> gb chargbuffer-left  (charspan-ref ^ idx))
      (-> gb chargbuffer-right (charspan-ref ^ (fx- idx left-n))))))

(define (chargbuffer-set! gb idx ch)
  (assert* 'chargbuffer-set! (fx<? -1 idx (chargbuffer-length gb)))
  (let ((left-n (-> gb chargbuffer-left charspan-length)))
    (if (fx<? idx left-n)
      (-> gb chargbuffer-left  (charspan-set! ^ idx ch))
      (-> gb chargbuffer-right (charspan-set! ^ (fx- idx left-n) ch)))))

(define (chargbuffer-clear! gb)
  (-> gb chargbuffer-left  charspan-clear!)
  (-> gb chargbuffer-right charspan-clear!))

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
        (charspan-insert-front/cspan! right left idx (fx- idx delta))
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

; read elements in range [src-start, src-end) from charspan csp-src
; and insert them into chargbuffer at position idx
(define (chargbuffer-insert-at/cspan! gb idx csp-src src-start src-end)
  (assert* 'chargbuffer-insert-at/cspan! (fx<=? 0 idx (chargbuffer-length gb)))
  (assert* 'chargbuffer-insert-at/cspan! (fx<=? 0 src-start src-end (charspan-length csp-src)))
  (let* ((left   (chargbuffer-left  gb))
         (right  (chargbuffer-right gb))
         (left-n (charspan-length left))
         (delta  (fx- idx left-n)))
    (cond
      ((fx>=? src-start src-end) ; nothing to do
        (void))
      ((fxzero? idx)
        (charspan-insert-front/cspan! left csp-src src-start src-end))
      ((fx=? idx (chargbuffer-length gb))
        (charspan-insert-back/cspan! right csp-src src-start src-end))
      (#t
        (chargbuffer-split-at! gb idx)
        (charspan-insert-back/cspan! left csp-src src-start src-end)))))


; read elements in range [src-start, src-end) from charspan csp-src
; and insert them into chargbuffer at position idx
(define (chargbuffer-insert-at/cbuf! gb idx gb-src src-start src-end)
  (assert* 'chargbuffer-insert-at/cbuf! (fx<=? 0 idx       (chargbuffer-length gb)))
  (assert* 'chargbuffer-insert-at/cbuf! (fx<=? 0 src-start src-end (chargbuffer-length gb-src)))
  (when (fx<? src-start src-end)
    (let* ((left   (chargbuffer-left  gb-src))
           (right  (chargbuffer-right gb-src))
           (left-n (charspan-length left)))
      (when (fx<? src-start left-n)
        ; read from (chargbuffer-left gb-src) and insert into gb
        (let ((pos (fxmin left-n src-end)))
          (chargbuffer-insert-at/cspan! gb idx left src-start pos)
          (set! idx (fx+ idx (fx- pos src-start)))
          (set! src-start pos)))
      (when (fx<? src-start src-end)
        ; read from (chargbuffer-right gb-src) and insert into gb
        (let ((pos (fx- src-start left-n))
              (end (fx- src-end   left-n)))
          (chargbuffer-insert-at/cspan! gb idx right pos end))))))


; remove elements in range [start, end) from chargbuffer gb
(define (chargbuffer-erase-range! gb start end)
  (let* ((left    (chargbuffer-left  gb))
         (right   (chargbuffer-right gb))
         (left-n  (charspan-length left))
         (right-n (charspan-length right))
         (len     (fx+ left-n right-n))
         (n       (fx- end start)))
    (assert* 'chargbuffer-erase-range! (fx<=? 0 start end len))
    (cond
      ((fxzero? n)
        (void)) ; nothing to do
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
    ((or (fx>=? i n) (not (proc i (chargbuffer-ref gb i))))
     (fx>=? i n))))

; customize how "chargbuffer" objects are printed
(record-writer (record-type-descriptor %chargbuffer)
  (lambda (gb port writer)
    (display "(string->chargbuffer* " port)
    (write (chargbuffer->string gb) port)
    (display ")" port)))

) ; close library
