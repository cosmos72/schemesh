;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;   define Scheme type "vbuffer", a gap buffer containing ccells.   ;;;;;;;
;;;;; Implementation: contains two vcellspans, a "left" and a "right" one  ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (schemesh screen vbuffer (0 9 2))
  (export
    in-vbuffer list->vbuffer string->vbuffer
    vcellspan->vbuffer vcellspan->vbuffer*
    make-vbuffer vbuffer vbuffer?
    vbuffer-length vbuffer-empty?
    vbuffer-ref vbuffer-set! vbuffer-clear! vbuffer-split-at!
    vbuffer-insert-at! vbuffer-insert-at/vcellspan! vbuffer-insert-at/vbuffer!
    vbuffer-delete! vbuffer-iterate
    vbuffer-display/bytespan vbuffer-write)
  (import
    (rnrs)
    (only (chezscheme)             fx1+ fx/ record-writer void)
    (only (schemesh bootstrap)     assert* assert-not* fx<=?*)
    (only (schemesh screen vcell)  vcell->char vcell->vpalette vcell-write vcell-display/bytespan vpalette-display vpalette-display/bytespan)
    (schemesh screen vcellspan))

;; a gap-buffer containing vcells
(define-record-type (%vbuffer %make-vbuffer vbuffer?)
  (fields
     (mutable left  cl< vbuffer-left-set!)
     (mutable right cl> vbuffer-right-set!))
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

(define (vbuffer-length cgb)
  (fx+ (vcellspan-length (cl< cgb))
       (vcellspan-length (cl> cgb))))

(define (vbuffer-empty? cgb)
  (and (vcellspan-empty? (cl< cgb))
       (vcellspan-empty? (cl> cgb))))

(define (vbuffer-ref cgb idx)
  (assert* 'vbuffer-ref (fx<? -1 idx (vbuffer-length cgb)))
  (let ((left-n (vcellspan-length (cl< cgb))))
    (if (fx<? idx left-n)
      (vcellspan-ref (cl< cgb) idx)
      (vcellspan-ref (cl> cgb) (fx- idx left-n)))))

;; c must be a character or cell
(define (vbuffer-set! cgb idx c)
  (assert* 'vbuffer-set! (fx<? -1 idx (vbuffer-length cgb)))
  (let ((left-n (vcellspan-length (cl< cgb))))
    (if (fx<? idx left-n)
      (vcellspan-set! (cl< cgb) idx c)
      (vcellspan-set! (cl> cgb) (fx- idx left-n) c))))

(define (vbuffer-clear! cgb)
  (vcellspan-clear! (cl< cgb))
  (vcellspan-clear! (cl> cgb)))

(define (vbuffer-split-at! cgb idx)
  (assert* 'vbuffer-split-at! (fx<=? 0 idx (vbuffer-length cgb)))
  (let* ((left  (cl< cgb))
         (right (cl> cgb))
         (delta (fx- idx (vcellspan-length left))))
    (cond
      ((fx>? delta 0)
        (vcellspan-insert-right/vcellspan! left right 0 delta)
        (vcellspan-delete-left! right delta))
      ((fx<? delta 0)
        (vcellspan-insert-left/vcellspan! right left idx (fx- idx delta))
        (vcellspan-delete-right! left (fx- delta))))))

;; insert one character or cell into vbuffer at position idx
(define (vbuffer-insert-at! cgb idx c)
  (assert* 'vbuffer-insert-at! (fx<=? 0 idx (vbuffer-length cgb)))
  (let* ((left   (cl< cgb))
         (right  (cl> cgb))
         (left-n (vcellspan-length left))
         (delta  (fx- idx left-n)))
    (cond
      ((fxzero? idx)
        (vcellspan-insert-left! left c))
      ((fx=? idx (vbuffer-length cgb))
        (vcellspan-insert-right! right c))
      (else
        (vbuffer-split-at! cgb idx)
        (vcellspan-insert-right! left c)))))


;; read elements in range [src-start, src-end) from vcellspan csp-src
;; and insert them into vbuffer at position idx
(define vbuffer-insert-at/vcellspan!
  (case-lambda
    ((cgb idx csp-src src-start src-end)
      (assert* 'vbuffer-insert-at/vcellspan! (fx<=? 0 idx (vbuffer-length cgb)))
      (assert* 'vbuffer-insert-at/vcellspan! (fx<=?* 0 src-start src-end (vcellspan-length csp-src)))
      (when (fx<? src-start src-end)
        (let* ((left   (cl< cgb))
               (right  (cl> cgb))
               (left-n (vcellspan-length left))
               (delta  (fx- idx left-n)))
          (assert-not* 'vbuffer-insert-at/vcellspan! (eq? left  csp-src))
          (assert-not* 'vbuffer-insert-at/vcellspan! (eq? right csp-src))
          (cond
            ((fxzero? idx)
              (vcellspan-insert-left/vcellspan! left csp-src src-start src-end))
            ((fx=? idx (vbuffer-length cgb))
              (vcellspan-insert-right/vcellspan! right csp-src src-start src-end))
            (else
              (vbuffer-split-at! cgb idx)
              (vcellspan-insert-right/vcellspan! left csp-src src-start src-end))))))
    ((cgb idx csp-src)
      (vbuffer-insert-at/vcellspan! cgb idx csp-src 0 (vcellspan-length csp-src)))))


;; read elements in range [src-start, src-end) from vbuffer csp-src
;; and insert them into vbuffer cgb at position idx
(define vbuffer-insert-at/vbuffer!
  (case-lambda
    ((cgb idx cgb-src src-start src-end)
      (assert* 'vbuffer-insert-at/vbuffer! (fx<=? 0 idx (vbuffer-length cgb)))
      (assert* 'vbuffer-insert-at/vbuffer! (fx<=?* 0 src-start src-end (vbuffer-length cgb-src)))
      (when (fx<? src-start src-end)
        (assert-not* 'vbuffer-insert-at/vbuffer! (eq? cgb cgb-src))
        (let* ((left   (cl< cgb-src))
               (right  (cl> cgb-src))
               (left-n (vcellspan-length left)))
          (when (fx<? src-start left-n)
            ; read from (cl< cgb-src) and insert into cgb
            (let ((pos (fxmin left-n src-end)))
              (vbuffer-insert-at/vcellspan! cgb idx left src-start pos)
              (set! idx (fx+ idx (fx- pos src-start)))
              (set! src-start pos)))
          (when (fx<? src-start src-end)
            ; read from (cl> cgb-src) and insert into cgb
            (let ((pos (fx- src-start left-n))
                  (end (fx- src-end   left-n)))
              (vbuffer-insert-at/vcellspan! cgb idx right pos end))))))
    ((cgb idx cgb-src)
      (vbuffer-insert-at/vbuffer! cgb idx cgb-src 0 (vbuffer-length cgb-src)))))


;; remove elements in range [start, end) from vbuffer cgb
(define (vbuffer-delete! cgb start end)
  (let* ((left    (cl< cgb))
         (right   (cl> cgb))
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
        (vbuffer-split-at! cgb end)
        (vcellspan-delete-right! left n)))))


;; create and return a closure that iterates on elements of vbuffer cgb.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in vbuffer cgb and #t,
;; or (values #<unspecified> #f) if end of vbuffer is reached.
(define in-vbuffer
  (case-lambda
    ((cgb start end step)
      (assert* 'in-vbuffer (fx<=?* 0 start end (vbuffer-length cgb)))
      (assert* 'in-vbuffer (fx>=? step 0))
      (let ((%in-vbuffer ; name shown when displaying the closure
              (lambda ()
                (if (fx<? start end)
                  (let ((elem (vbuffer-ref cgb start)))
                    (set! start (fx+ start step))
                    (values elem #t))
                  (values #\nul #f)))))
        %in-vbuffer))
    ((cgb start end)
      (in-vbuffer cgb start end 1))
    ((cgb)
      (in-vbuffer cgb 0 (vbuffer-length cgb) 1))))


(define (vbuffer-iterate cgb proc)
  (do ((i 0 (fx1+ i))
       (n (vbuffer-length cgb)))
    ((or (fx>=? i n) (not (proc i (vbuffer-ref cgb i))))
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
    ((cgb start end port)
      (assert* 'vbuffer-write (fx<=?* 0 start end (vbuffer-length cgb)))
      (put-char port #\")
      (let ((old-palette 0))
        (do ((i start (fx1+ i)))
            ((fx>=? i end))
          (let* ((cl      (vbuffer-ref cgb i))
                 (palette (vcell->vpalette cl)))
            (vcell-write cl old-palette port)
            (unless (fx=? palette old-palette)
              (set! old-palette palette)))))
      (put-char port #\"))
    ((cgb port)
      (vbuffer-write cgb 0 (vbuffer-length cgb) port))))


;; customize how "vbuffer" objects are printed
(record-writer (record-type-descriptor %vbuffer)
  (lambda (cgb port writer)
    (display "(string->vbuffer " port)
    (vbuffer-write cgb port)
    (display ")" port)))

) ; close library
