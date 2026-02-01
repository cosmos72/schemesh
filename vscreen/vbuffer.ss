;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file vscreen/all.ss


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;   define Scheme type "vbuffer", a gap buffer containing ccells.   ;;;;;;;
;;;;; Implementation: contains two vcellspans, a "left" and a "right" one  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a gap-buffer containing vcells
(define-record-type (%vbuffer %make-vbuffer vbuffer?)
  (fields
     (mutable left  v< vbuffer-left-set!)
     (mutable right v> vbuffer-right-set!))
  (nongenerative %vbuffer-7c46d04b-34f4-4046-b5c7-b63753c1be39))

;; convert a list of cellacters to vbuffer
(define (list->vbuffer l)
  (%make-vbuffer (vcellspan) (list->vcellspan l)))

;; convert a string to vbuffer
(define (string->vbuffer str)
  (%make-vbuffer (vcellspan) (string->vcellspan str)))

;; convert a vcellspan to vbuffer
(define (vcellspan->vbuffer csp)
  (%make-vbuffer (vcellspan) (vcellspan-copy csp)))

;; view a vcellspan as vbuffer
;;
;; modifications to the vcellspan will propagate to the returned vbuffer
;; until the vbuffer reallocates its internal storage.
(define (vcellspan->vbuffer* csp)
  (%make-vbuffer (vcellspan) csp))

(define make-vbuffer
  (case-lambda
    ((n)      (%make-vbuffer (make-vcellspan (fx/ n 2))      (make-vcellspan (fx/ (fx1+ n) 2))))
    ((n fill) (%make-vbuffer (make-vcellspan (fx/ n 2) fill) (make-vcellspan (fx/ (fx1+ n) 2) fill)))))


;; create a vbuffer from zero or more cellacters
(define (vbuffer . cells)
  (list->vbuffer cells))

(define (vbuffer-length vb)
  (fx+ (vcellspan-length (v< vb))
       (vcellspan-length (v> vb))))

(define (vbuffer-empty? vb)
  (and (vcellspan-empty? (v< vb))
       (vcellspan-empty? (v> vb))))

(define (vbuffer-ref vb idx)
  (assert* 'vbuffer-ref (fx<? -1 idx (vbuffer-length vb)))
  (let ((left-n (vcellspan-length (v< vb))))
    (if (fx<? idx left-n)
      (vcellspan-ref (v< vb) idx)
      (vcellspan-ref (v> vb) (fx- idx left-n)))))

;; c must be a character or cell
(define (vbuffer-set! vb idx c)
  (assert* 'vbuffer-set! (fx<? -1 idx (vbuffer-length vb)))
  (let ((left-n (vcellspan-length (v< vb))))
    (if (fx<? idx left-n)
      (vcellspan-set! (v< vb) idx c)
      (vcellspan-set! (v> vb) (fx- idx left-n) c))))

(define (vbuffer-clear! vb)
  (vcellspan-clear! (v< vb))
  (vcellspan-clear! (v> vb)))

(define (vbuffer-split-at! vb idx)
  (assert* 'vbuffer-split-at! (fx<=? 0 idx (vbuffer-length vb)))
  (let* ((left  (v< vb))
         (right (v> vb))
         (delta (fx- idx (vcellspan-length left))))
    (cond
      ((fx>? delta 0)
        (vcellspan-insert-right/vcellspan! left right 0 delta)
        (vcellspan-delete-left! right delta))
      ((fx<? delta 0)
        (vcellspan-insert-left/vcellspan! right left idx (fx- idx delta))
        (vcellspan-delete-right! left (fx- delta))))))

;; insert one character or cell into vbuffer at position idx
(define (vbuffer-insert-at! vb idx c)
  (assert* 'vbuffer-insert-at! (fx<=? 0 idx (vbuffer-length vb)))
  (let* ((left   (v< vb))
         (right  (v> vb))
         (left-n (vcellspan-length left))
         (delta  (fx- idx left-n)))
    (cond
      ((fxzero? idx)
        (vcellspan-insert-left! left c))
      ((fx=? idx (vbuffer-length vb))
        (vcellspan-insert-right! right c))
      (else
        (vbuffer-split-at! vb idx)
        (vcellspan-insert-right! left c)))))


;; read elements in range [src-start, src-end) from vcellspan csp-src
;; and insert them into vbuffer at position idx
(define vbuffer-insert-at/vcellspan!
  (case-lambda
    ((vb idx csp-src src-start src-end)
      (assert* 'vbuffer-insert-at/vcellspan! (fx<=? 0 idx (vbuffer-length vb)))
      (assert* 'vbuffer-insert-at/vcellspan! (fx<=?* 0 src-start src-end (vcellspan-length csp-src)))
      (when (fx<? src-start src-end)
        (let* ((left   (v< vb))
               (right  (v> vb))
               (left-n (vcellspan-length left))
               (delta  (fx- idx left-n)))
          (assert-not* 'vbuffer-insert-at/vcellspan! (eq? left  csp-src))
          (assert-not* 'vbuffer-insert-at/vcellspan! (eq? right csp-src))
          (cond
            ((fxzero? idx)
              (vcellspan-insert-left/vcellspan! left csp-src src-start src-end))
            ((fx=? idx (vbuffer-length vb))
              (vcellspan-insert-right/vcellspan! right csp-src src-start src-end))
            (else
              (vbuffer-split-at! vb idx)
              (vcellspan-insert-right/vcellspan! left csp-src src-start src-end))))))
    ((vb idx csp-src)
      (vbuffer-insert-at/vcellspan! vb idx csp-src 0 (vcellspan-length csp-src)))))


;; read elements in range [src-start, src-end) from vbuffer csp-src
;; and insert them into vbuffer vb at position idx
(define vbuffer-insert-at/vbuffer!
  (case-lambda
    ((vb idx vb-src src-start src-end)
      (assert* 'vbuffer-insert-at/vbuffer! (fx<=? 0 idx (vbuffer-length vb)))
      (assert* 'vbuffer-insert-at/vbuffer! (fx<=?* 0 src-start src-end (vbuffer-length vb-src)))
      (when (fx<? src-start src-end)
        (assert-not* 'vbuffer-insert-at/vbuffer! (eq? vb vb-src))
        (let* ((left   (v< vb-src))
               (right  (v> vb-src))
               (left-n (vcellspan-length left)))
          (when (fx<? src-start left-n)
            ; read from (v< vb-src) and insert into vb
            (let ((pos (fxmin left-n src-end)))
              (vbuffer-insert-at/vcellspan! vb idx left src-start pos)
              (set! idx (fx+ idx (fx- pos src-start)))
              (set! src-start pos)))
          (when (fx<? src-start src-end)
            ; read from (v> vb-src) and insert into vb
            (let ((pos (fx- src-start left-n))
                  (end (fx- src-end   left-n)))
              (vbuffer-insert-at/vcellspan! vb idx right pos end))))))
    ((vb idx vb-src)
      (vbuffer-insert-at/vbuffer! vb idx vb-src 0 (vbuffer-length vb-src)))))


;; remove elements in range [start, end) from vbuffer vb
(define (vbuffer-delete! vb start end)
  (let* ((left    (v< vb))
         (right   (v> vb))
         (left-n  (vcellspan-length left))
         (right-n (vcellspan-length right))
         (len     (fx+ left-n right-n))
         (n       (fx- end start)))
    (assert* 'vbuffer-delete! (fx<=?* 0 start end len))
    (cond
      ((fxzero? n)
        (void)) ; nothing to do
      ((fxzero? start)
        (let ((head (fxmin n left-n)))
          (vcellspan-delete-left! left head)
          (vcellspan-delete-left! right (fx- n head))))
      ((fx=? end left-n)
        (vcellspan-delete-right! left n))
      ((fx=? start left-n)
        (vcellspan-delete-left! right n))
      ((fx=? end len)
        (let ((tail (fxmin n right-n)))
          (vcellspan-delete-right! right tail)
          (vcellspan-delete-right! left (fx- n tail))))
      (else
        (vbuffer-split-at! vb end)
        (vcellspan-delete-right! left n)))))


;; create and return a closure that iterates on elements of vbuffer vb.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in vbuffer vb and #t,
;; or (values #<unspecified> #f) if end of vbuffer is reached.
(define in-vbuffer
  (case-lambda
    ((vb start end step)
      (assert* 'in-vbuffer (fx<=?* 0 start end (vbuffer-length vb)))
      (assert* 'in-vbuffer (fx>=? step 0))
      (let ((%in-vbuffer ; name shown when displaying the closure
              (lambda ()
                (if (fx<? start end)
                  (let ((elem (vbuffer-ref vb start)))
                    (set! start (fx+ start step))
                    (values elem #t))
                  (values #\x0 #f)))))
        %in-vbuffer))
    ((vb start end)
      (in-vbuffer vb start end 1))
    ((vb)
      (in-vbuffer vb 0 (vbuffer-length vb) 1))))


(define (vbuffer-iterate vb proc)
  (do ((i 0 (fx1+ i))
       (n (vbuffer-length vb)))
    ((or (fx>=? i n) (not (proc i (vbuffer-ref vb i))))
     (fx>=? i n))))


(define vbuffer-display/bytespan
  (case-lambda
    ((line start end wbuf)
      (let ((old-palette 0))
        (do ((pos start (fx1+ pos)))
            ((fx>=? pos end))
          (let* ((cl      (vbuffer-ref line pos))
                 (palette (vcell->vpalette cl)))
            (vcell-display/bytespan cl old-palette wbuf)
            (unless (fx=? old-palette palette)
              (set! old-palette palette))))
        (unless (fxzero? old-palette)
          (vpalette-display/bytespan 0 wbuf))))
    ((line wbuf)
     (vbuffer-display/bytespan line 0 (vbuffer-length line) wbuf))))


(define vbuffer-write
  (case-lambda
    ((vb start end port)
      (assert* 'vbuffer-write (fx<=?* 0 start end (vbuffer-length vb)))
      (put-char port #\")
      (let ((old-palette 0))
        (do ((i start (fx1+ i)))
            ((fx>=? i end))
          (let* ((cl      (vbuffer-ref vb i))
                 (palette (vcell->vpalette cl)))
            (vcell-write cl old-palette port)
            (unless (fx=? palette old-palette)
              (set! old-palette palette)))))
      (put-char port #\"))
    ((vb port)
      (vbuffer-write vb 0 (vbuffer-length vb) port))))
