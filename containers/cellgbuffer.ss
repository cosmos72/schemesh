;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  define Scheme type "cellgbuffer", a gap buffer containing cells. ;;;;;;;
;;;;; Implementation: contains two cellspans, a "left" and a "right" ones ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (schemesh containers cellgbuffer (0 8 3))
  (export
    in-cellgbuffer list->cellgbuffer string->cellgbuffer
    cellspan->cellgbuffer cellspan->cellgbuffer*
    make-cellgbuffer cellgbuffer cellgbuffer?
    cellgbuffer->cellspans*
    cellgbuffer-length cellgbuffer-empty?
    cellgbuffer-ref cellgbuffer-set! cellgbuffer-clear! cellgbuffer-split-at!
    cellgbuffer-insert-at! cellgbuffer-insert-at/cellspan! cellgbuffer-insert-at/cellgbuffer!
    cellgbuffer-delete! cellgbuffer-iterate)
  (import
    (rnrs)
    (only (chezscheme) fx1+ fx/ record-writer void)
    (only (schemesh bootstrap) assert* assert-not* fx<=?*)
    (schemesh containers cellspan))

(define-record-type (%cellgbuffer %make-cellgbuffer cellgbuffer?)
  (fields
     (mutable left  c< cellgbuffer-left-set!)
     (mutable right c> cellgbuffer-right-set!))
  (nongenerative %cellgbuffer-7c46d04b-34f4-4046-b5c7-b63753c1be39))

;; convert a list of cellacters to cellgbuffer
(define (list->cellgbuffer l)
  (%make-cellgbuffer (cellspan) (list->cellspan l)))

;; convert a string to cellgbuffer
(define (string->cellgbuffer str)
  (%make-cellgbuffer (cellspan) (string->cellspan str)))

;; convert a cellspan to cellgbuffer
(define (cellspan->cellgbuffer csp)
  (%make-cellgbuffer (cellspan) (cellspan-copy csp)))

;; view a cellspan as cellgbuffer
;;
;; modifications to the cellspan will propagate to the returned cellgbuffer
;; until the cellgbuffer reallocates its internal storage.
(define (cellspan->cellgbuffer* csp)
  (%make-cellgbuffer (cellspan) csp))

(define make-cellgbuffer
  (case-lambda
    ((n)      (%make-cellgbuffer (make-cellspan (fx/ n 2))      (make-cellspan (fx/ (fx1+ n) 2))))
    ((n fill) (%make-cellgbuffer (make-cellspan (fx/ n 2) fill) (make-cellspan (fx/ (fx1+ n) 2) fill)))))


;; view a cellgbuffer as two cellspans.
;;
;; modifications to the returned cellspans will propagate to the cellgbuffer
;; (and vice-versa) until the cellgbuffer reallocates its internal storage.
;;
;; returns two values: the two cellspans
(define (cellgbuffer->cellspans* cgb)
  (values (c< cgb) (c> cgb)))


;; create a cellgbuffer from zero or more cellacters
(define (cellgbuffer . cells)
  (list->cellgbuffer cells))

(define (cellgbuffer-length cgb)
  (fx+ (cellspan-length (c< cgb))
       (cellspan-length (c> cgb))))

(define (cellgbuffer-empty? cgb)
  (and (cellspan-empty? (c< cgb))
       (cellspan-empty? (c> cgb))))

(define (cellgbuffer-ref cgb idx)
  (assert* 'cellgbuffer-ref (fx<? -1 idx (cellgbuffer-length cgb)))
  (let ((left-n (cellspan-length (c< cgb))))
    (if (fx<? idx left-n)
      (cellspan-ref (c< cgb) idx)
      (cellspan-ref (c> cgb) (fx- idx left-n)))))

(define (cellgbuffer-set! cgb idx ch)
  (assert* 'cellgbuffer-set! (fx<? -1 idx (cellgbuffer-length cgb)))
  (let ((left-n (cellspan-length (c< cgb))))
    (if (fx<? idx left-n)
      (cellspan-set! (c< cgb) idx ch)
      (cellspan-set! (c> cgb) (fx- idx left-n) ch))))

(define (cellgbuffer-clear! cgb)
  (cellspan-clear! (c< cgb))
  (cellspan-clear! (c> cgb)))

(define (cellgbuffer-split-at! cgb idx)
  (assert* 'cellgbuffer-split-at! (fx<=? 0 idx (cellgbuffer-length cgb)))
  (let* ((left  (c< cgb))
         (right (c> cgb))
         (delta (fx- idx (cellspan-length left))))
    (cond
      ((fx>? delta 0)
        (cellspan-insert-right/cellspan! left right 0 delta)
        (cellspan-delete-left! right delta))
      ((fx<? delta 0)
        (cellspan-insert-left/cellspan! right left idx (fx- idx delta))
        (cellspan-delete-right! left (fx- delta))))))

;; insert one cell into cellgbuffer at position idx
(define (cellgbuffer-insert-at! cgb idx ch)
  (assert* 'cellgbuffer-insert-at! (fx<=? 0 idx (cellgbuffer-length cgb)))
  (let* ((left   (c< cgb))
         (right  (c> cgb))
         (left-n (cellspan-length left))
         (delta  (fx- idx left-n)))
    (cond
      ((fxzero? idx)
        (cellspan-insert-left! left ch))
      ((fx=? idx (cellgbuffer-length cgb))
        (cellspan-insert-right! right ch))
      (else
        (cellgbuffer-split-at! cgb idx)
        (cellspan-insert-right! left ch)))))


;; read elements in range [src-start, src-end) from cellspan csp-src
;; and insert them into cellgbuffer at position idx
(define cellgbuffer-insert-at/cellspan!
  (case-lambda
    ((cgb idx csp-src src-start src-end)
      (assert* 'cellgbuffer-insert-at/cellspan! (fx<=? 0 idx (cellgbuffer-length cgb)))
      (assert* 'cellgbuffer-insert-at/cellspan! (fx<=?* 0 src-start src-end (cellspan-length csp-src)))
      (when (fx<? src-start src-end)
        (let* ((left   (c< cgb))
               (right  (c> cgb))
               (left-n (cellspan-length left))
               (delta  (fx- idx left-n)))
          (assert-not* 'cellgbuffer-insert-at/cellspan! (eq? left  csp-src))
          (assert-not* 'cellgbuffer-insert-at/cellspan! (eq? right csp-src))
          (cond
            ((fxzero? idx)
              (cellspan-insert-left/cellspan! left csp-src src-start src-end))
            ((fx=? idx (cellgbuffer-length cgb))
              (cellspan-insert-right/cellspan! right csp-src src-start src-end))
            (else
              (cellgbuffer-split-at! cgb idx)
              (cellspan-insert-right/cellspan! left csp-src src-start src-end))))))
    ((cgb idx csp-src)
      (cellgbuffer-insert-at/cellspan! cgb idx csp-src 0 (cellspan-length csp-src)))))


;; read elements in range [src-start, src-end) from cellgbuffer csp-src
;; and insert them into cellgbuffer cgb at position idx
(define cellgbuffer-insert-at/cellgbuffer!
  (case-lambda
    ((cgb idx cgb-src src-start src-end)
      (assert* 'cellgbuffer-insert-at/cellgbuffer! (fx<=? 0 idx (cellgbuffer-length cgb)))
      (assert* 'cellgbuffer-insert-at/cellgbuffer! (fx<=?* 0 src-start src-end (cellgbuffer-length cgb-src)))
      (when (fx<? src-start src-end)
        (assert-not* 'cellgbuffer-insert-at/cellgbuffer! (eq? cgb cgb-src))
        (let* ((left   (c< cgb-src))
               (right  (c> cgb-src))
               (left-n (cellspan-length left)))
          (when (fx<? src-start left-n)
            ; read from (c< cgb-src) and insert into cgb
            (let ((pos (fxmin left-n src-end)))
              (cellgbuffer-insert-at/cellspan! cgb idx left src-start pos)
              (set! idx (fx+ idx (fx- pos src-start)))
              (set! src-start pos)))
          (when (fx<? src-start src-end)
            ; read from (c> cgb-src) and insert into cgb
            (let ((pos (fx- src-start left-n))
                  (end (fx- src-end   left-n)))
              (cellgbuffer-insert-at/cellspan! cgb idx right pos end))))))
    ((cgb idx cgb-src)
      (cellgbuffer-insert-at/cellgbuffer! cgb idx cgb-src 0 (cellgbuffer-length cgb-src)))))


;; remove elements in range [start, end) from cellgbuffer cgb
(define (cellgbuffer-delete! cgb start end)
  (let* ((left    (c< cgb))
         (right   (c> cgb))
         (left-n  (cellspan-length left))
         (right-n (cellspan-length right))
         (len     (fx+ left-n right-n))
         (n       (fx- end start)))
    (assert* 'cellgbuffer-delete! (fx<=?* 0 start end len))
    (cond
      ((fxzero? n)
        (void)) ; nothing to do
      ((fxzero? start)
        (let ((head (fxmin n left-n)))
          (cellspan-delete-left! left head)
          (cellspan-delete-left! right (fx- n head))))
      ((fx=? end left-n)
        (cellspan-delete-right! left n))
      ((fx=? start left-n)
        (cellspan-delete-left! right n))
      ((fx=? end len)
        (let ((tail (fxmin n right-n)))
          (cellspan-delete-right! right tail)
          (cellspan-delete-right! left (fx- n tail))))
      (else
        (cellgbuffer-split-at! cgb end)
        (cellspan-delete-right! left n)))))


;; create and return a closure that iterates on elements of cellgbuffer cgb.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in cellgbuffer cgb and #t,
;; or (values #<unspecified> #f) if end of cellgbuffer is reached.
(define in-cellgbuffer
  (case-lambda
    ((cgb start end step)
      (assert* 'in-cellgbuffer (fx<=?* 0 start end (cellgbuffer-length cgb)))
      (assert* 'in-cellgbuffer (fx>=? step 0))
      (let ((%in-cellgbuffer ; name shown when displaying the closure
              (lambda ()
                (if (fx<? start end)
                  (let ((elem (cellgbuffer-ref cgb start)))
                    (set! start (fx+ start step))
                    (values elem #t))
                  (values #\nul #f)))))
        %in-cellgbuffer))
    ((cgb start end)
      (in-cellgbuffer cgb start end 1))
    ((cgb)
      (in-cellgbuffer cgb 0 (cellgbuffer-length cgb) 1))))


(define (cellgbuffer-iterate cgb proc)
  (do ((i 0 (fx1+ i))
       (n (cellgbuffer-length cgb)))
    ((or (fx>=? i n) (not (proc i (cellgbuffer-ref cgb i))))
     (fx>=? i n))))

;; customize how "cellgbuffer" objects are printed
(record-writer (record-type-descriptor %cellgbuffer)
  (lambda (cgb port writer)
    (display "(string->cellgbuffer* " port)
    (display "..." port) ;; (write (cellgbuffer->string cgb) port)
    (display ")" port)))

) ; close library
