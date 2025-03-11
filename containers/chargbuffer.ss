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

(library (schemesh containers chargbuffer (0 8 1))
  (export
    in-chargbuffer list->chargbuffer string->chargbuffer string->chargbuffer*
    charspan->chargbuffer charspan->chargbuffer*
    make-chargbuffer chargbuffer chargbuffer? chargbuffer->charspan chargbuffer->string
    chargbuffer-length chargbuffer-empty?
    chargbuffer-ref chargbuffer-set! chargbuffer-clear! chargbuffer-split-at!
    chargbuffer-insert-at! chargbuffer-insert-at/cspan! chargbuffer-insert-at/cbuf!
    chargbuffer-erase-range! chargbuffer-iterate)
  (import
    (rnrs)
    (only (chezscheme) fx1+ record-writer string-copy! void)
    (only (schemesh bootstrap) assert* assert-not* fx<=?*)
    (schemesh containers charspan))

(define-record-type
  (%chargbuffer %make-chargbuffer chargbuffer?)
  (fields
     (mutable left  <- chargbuffer-left-set!)
     (mutable right -> chargbuffer-right-set!))
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
  (let* ((left    (<- gb))
         (right   (-> gb))
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
  (fx+ (charspan-length (<- gb))
       (charspan-length (-> gb))))

(define (chargbuffer-empty? gb)
  (and (charspan-empty? (<- gb))
       (charspan-empty? (-> gb))))

(define (chargbuffer-ref gb idx)
  (assert* 'chargbuffer-ref (fx<? -1 idx (chargbuffer-length gb)))
  (let ((left-n (charspan-length (<- gb))))
    (if (fx<? idx left-n)
      (charspan-ref (<- gb) idx)
      (charspan-ref (-> gb) (fx- idx left-n)))))

(define (chargbuffer-set! gb idx ch)
  (assert* 'chargbuffer-set! (fx<? -1 idx (chargbuffer-length gb)))
  (let ((left-n (charspan-length (<- gb))))
    (if (fx<? idx left-n)
      (charspan-set! (<- gb) idx ch)
      (charspan-set! (-> gb) (fx- idx left-n) ch))))

(define (chargbuffer-clear! gb)
  (charspan-clear! (<- gb))
  (charspan-clear! (-> gb)))

(define (chargbuffer-split-at! gb idx)
  (assert* 'chargbuffer-split-at! (fx<=? 0 idx (chargbuffer-length gb)))
  (let* ((left  (<- gb))
         (right (-> gb))
         (delta (fx- idx (charspan-length left))))
    (cond
      ((fx>? delta 0)
        (charspan-insert-right/cspan! left right 0 delta)
        (charspan-erase-left! right delta))
      ((fx<? delta 0)
        (charspan-insert-left/cspan! right left idx (fx- idx delta))
        (charspan-erase-right! left (fx- delta))))))

; insert one char into chargbuffer at position idx
(define (chargbuffer-insert-at! gb idx ch)
  (assert* 'chargbuffer-insert-at! (fx<=? 0 idx (chargbuffer-length gb)))
  (let* ((left   (<- gb))
         (right  (-> gb))
         (left-n (charspan-length left))
         (delta  (fx- idx left-n)))
    (cond
      ((fxzero? idx)
        (charspan-insert-left! left ch))
      ((fx=? idx (chargbuffer-length gb))
        (charspan-insert-right! right ch))
      (else
        (chargbuffer-split-at! gb idx)
        (charspan-insert-right! left ch)))))


; read elements in range [src-start, src-end) from charspan csp-src
; and insert them into chargbuffer at position idx
(define chargbuffer-insert-at/cspan!
  (case-lambda
    ((gb idx csp-src src-start src-end)
      (assert* 'chargbuffer-insert-at/cspan! (fx<=? 0 idx (chargbuffer-length gb)))
      (assert* 'chargbuffer-insert-at/cspan! (fx<=?* 0 src-start src-end (charspan-length csp-src)))
      (when (fx<? src-start src-end)
        (let* ((left   (<- gb))
               (right  (-> gb))
               (left-n (charspan-length left))
               (delta  (fx- idx left-n)))
          (assert-not* 'chargbuffer-insert-at/cbuf! (eq? left  csp-src))
          (assert-not* 'chargbuffer-insert-at/cbuf! (eq? right csp-src))
          (cond
            ((fxzero? idx)
              (charspan-insert-left/cspan! left csp-src src-start src-end))
            ((fx=? idx (chargbuffer-length gb))
              (charspan-insert-right/cspan! right csp-src src-start src-end))
            (else
              (chargbuffer-split-at! gb idx)
              (charspan-insert-right/cspan! left csp-src src-start src-end))))))
    ((gb idx csp-src)
      (chargbuffer-insert-at/cspan! gb idx csp-src 0 (charspan-length csp-src)))))


; read elements in range [src-start, src-end) from charspan csp-src
; and insert them into chargbuffer at position idx
(define chargbuffer-insert-at/cbuf!
  (case-lambda
    ((gb idx gb-src src-start src-end)
      (assert* 'chargbuffer-insert-at/cbuf! (fx<=? 0 idx (chargbuffer-length gb)))
      (assert* 'chargbuffer-insert-at/cbuf! (fx<=?* 0 src-start src-end (chargbuffer-length gb-src)))
      (when (fx<? src-start src-end)
        (assert-not* 'chargbuffer-insert-at/cbuf! (eq? gb gb-src))
        (let* ((left   (<- gb-src))
               (right  (-> gb-src))
               (left-n (charspan-length left)))
          (when (fx<? src-start left-n)
            ; read from (<- gb-src) and insert into gb
            (let ((pos (fxmin left-n src-end)))
              (chargbuffer-insert-at/cspan! gb idx left src-start pos)
              (set! idx (fx+ idx (fx- pos src-start)))
              (set! src-start pos)))
          (when (fx<? src-start src-end)
            ; read from (-> gb-src) and insert into gb
            (let ((pos (fx- src-start left-n))
                  (end (fx- src-end   left-n)))
              (chargbuffer-insert-at/cspan! gb idx right pos end))))))
    ((gb idx gb-src)
      (chargbuffer-insert-at/cbuf! gb idx gb-src 0 (chargbuffer-length gb-src)))))


; remove elements in range [start, end) from chargbuffer gb
(define (chargbuffer-erase-range! gb start end)
  (let* ((left    (<- gb))
         (right   (-> gb))
         (left-n  (charspan-length left))
         (right-n (charspan-length right))
         (len     (fx+ left-n right-n))
         (n       (fx- end start)))
    (assert* 'chargbuffer-erase-range! (fx<=?* 0 start end len))
    (cond
      ((fxzero? n)
        (void)) ; nothing to do
      ((fxzero? start)
        (let ((head (fxmin n left-n)))
          (charspan-erase-left! left head)
          (charspan-erase-left! right (fx- n head))))
      ((fx=? end left-n)
        (charspan-erase-right! left n))
      ((fx=? start left-n)
        (charspan-erase-left! right n))
      ((fx=? end len)
        (let ((tail (fxmin n right-n)))
          (charspan-erase-right! right tail)
          (charspan-erase-right! left (fx- n tail))))
      (else
        (chargbuffer-split-at! gb end)
        (charspan-erase-right! left n)))))


;; create and return a closure that iterates on elements of chargbuffer gb.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in chargbuffer gb and #t,
;; or (values #<unspecified> #f) if end of chargbuffer is reached.
(define in-chargbuffer
  (case-lambda
    ((gb start end step)
      (assert* 'in-chargbuffer (fx<=?* 0 start end (chargbuffer-length gb)))
      (assert* 'in-chargbuffer (fx>=? step 0))
      (let ((%in-chargbuffer ; name shown when displaying the closure
              (lambda ()
                (if (fx<? start end)
                  (let ((elem (chargbuffer-ref gb start)))
                    (set! start (fx+ start step))
                    (values elem #t))
                  (values #\nul #f)))))
        %in-chargbuffer))
    ((gb start end)
      (in-chargbuffer gb start end 1))
    ((gb)
      (in-chargbuffer gb 0 (chargbuffer-length gb) 1))))


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
