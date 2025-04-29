;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  define Scheme type "chargbuffer", a gap buffer containing chars. ;;;;;;;
;;;;; Implementation: contains two charspans, a "left" and a "right" ones ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (schemesh containers chargbuffer (0 8 3))
  (export
    in-chargbuffer list->chargbuffer string->chargbuffer string->chargbuffer*
    charspan->chargbuffer charspan->chargbuffer*
    make-chargbuffer chargbuffer chargbuffer?
    chargbuffer->charspan chargbuffer->charspans* chargbuffer->string
    chargbuffer-length chargbuffer-empty?
    chargbuffer-ref chargbuffer-set! chargbuffer-clear! chargbuffer-split-at!
    chargbuffer-insert-at! chargbuffer-insert-at/charspan! chargbuffer-insert-at/chargbuffer!
    chargbuffer-delete! chargbuffer-iterate)
  (import
    (rnrs)
    (only (chezscheme) fx1+ fx/ record-writer string-copy! void)
    (only (schemesh bootstrap) assert* assert-not* fx<=?*)
    (schemesh containers charspan))

(define-record-type (%chargbuffer %make-chargbuffer chargbuffer?)
  (fields
     (mutable left  ch< chargbuffer-left-set!)
     (mutable right ch> chargbuffer-right-set!))
  (nongenerative %chargbuffer-7c46d04b-34f4-4046-b5c7-b63753c1be39))

;; convert a list of characters to chargbuffer
(define (list->chargbuffer l)
  (%make-chargbuffer (charspan) (list->charspan l)))

;; convert a string to chargbuffer
(define (string->chargbuffer str)
  (%make-chargbuffer (charspan) (string->charspan str)))

;; view a string as chargbuffer
(define (string->chargbuffer* str)
  (%make-chargbuffer (charspan) (string->charspan* str)))

;; convert a charspan to chargbuffer
(define (charspan->chargbuffer csp)
  (%make-chargbuffer (charspan) (charspan-copy csp)))

;; view a charspan as chargbuffer
;;
;; modifications to the charspan will propagate to the returned chargbuffer
;; until the chargbuffer reallocates its internal storage.
(define (charspan->chargbuffer* csp)
  (%make-chargbuffer (charspan) csp))

(define make-chargbuffer
  (case-lambda
    ((n)      (%make-chargbuffer (make-charspan (fx/ n 2))      (make-charspan (fx/ (fx1+ n) 2))))
    ((n fill) (%make-chargbuffer (make-charspan (fx/ n 2) fill) (make-charspan (fx/ (fx1+ n) 2) fill)))))

;; convert a chargbuffer to string
(define (chargbuffer->string cgb)
  (let* ((left    (ch< cgb))
         (right   (ch> cgb))
         (left-n  (charspan-length left))
         (right-n (charspan-length right))
         (dst (make-string (fx+ left-n right-n))))
    (string-copy! (charspan-peek-data left)  (charspan-peek-beg left)
                  dst 0 left-n)
    (string-copy! (charspan-peek-data right) (charspan-peek-beg right)
                  dst left-n right-n)
    dst))

;; convert a chargbuffer to charspan
(define (chargbuffer->charspan cgb)
  (string->charspan* (chargbuffer->string cgb)))

;; view a chargbuffer as two charspans.
;;
;; modifications to the returned charspans will propagate to the chargbuffer
;; (and vice-versa) until the chargbuffer reallocates its internal storage.
;;
;; returns two values: the two charspans
(define (chargbuffer->charspans* cgb)
  (values (ch< cgb) (ch> cgb)))


;; create a chargbuffer from zero or more characters
(define (chargbuffer . chars)
  (list->chargbuffer chars))

(define (chargbuffer-length cgb)
  (fx+ (charspan-length (ch< cgb))
       (charspan-length (ch> cgb))))

(define (chargbuffer-empty? cgb)
  (and (charspan-empty? (ch< cgb))
       (charspan-empty? (ch> cgb))))

(define (chargbuffer-ref cgb idx)
  (assert* 'chargbuffer-ref (fx<? -1 idx (chargbuffer-length cgb)))
  (let ((left-n (charspan-length (ch< cgb))))
    (if (fx<? idx left-n)
      (charspan-ref (ch< cgb) idx)
      (charspan-ref (ch> cgb) (fx- idx left-n)))))

(define (chargbuffer-set! cgb idx ch)
  (assert* 'chargbuffer-set! (fx<? -1 idx (chargbuffer-length cgb)))
  (let ((left-n (charspan-length (ch< cgb))))
    (if (fx<? idx left-n)
      (charspan-set! (ch< cgb) idx ch)
      (charspan-set! (ch> cgb) (fx- idx left-n) ch))))

(define (chargbuffer-clear! cgb)
  (charspan-clear! (ch< cgb))
  (charspan-clear! (ch> cgb)))

(define (chargbuffer-split-at! cgb idx)
  (assert* 'chargbuffer-split-at! (fx<=? 0 idx (chargbuffer-length cgb)))
  (let* ((left  (ch< cgb))
         (right (ch> cgb))
         (delta (fx- idx (charspan-length left))))
    (cond
      ((fx>? delta 0)
        (charspan-insert-right/charspan! left right 0 delta)
        (charspan-delete-left! right delta))
      ((fx<? delta 0)
        (charspan-insert-left/charspan! right left idx (fx- idx delta))
        (charspan-delete-right! left (fx- delta))))))

;; insert one char into chargbuffer at position idx
(define (chargbuffer-insert-at! cgb idx ch)
  (assert* 'chargbuffer-insert-at! (fx<=? 0 idx (chargbuffer-length cgb)))
  (let* ((left   (ch< cgb))
         (right  (ch> cgb))
         (left-n (charspan-length left))
         (delta  (fx- idx left-n)))
    (cond
      ((fxzero? idx)
        (charspan-insert-left! left ch))
      ((fx=? idx (chargbuffer-length cgb))
        (charspan-insert-right! right ch))
      (else
        (chargbuffer-split-at! cgb idx)
        (charspan-insert-right! left ch)))))


;; read elements in range [src-start, src-end) from charspan csp-src
;; and insert them into chargbuffer at position idx
(define chargbuffer-insert-at/charspan!
  (case-lambda
    ((cgb idx csp-src src-start src-end)
      (assert* 'chargbuffer-insert-at/charspan! (fx<=? 0 idx (chargbuffer-length cgb)))
      (assert* 'chargbuffer-insert-at/charspan! (fx<=?* 0 src-start src-end (charspan-length csp-src)))
      (when (fx<? src-start src-end)
        (let* ((left   (ch< cgb))
               (right  (ch> cgb))
               (left-n (charspan-length left))
               (delta  (fx- idx left-n)))
          (assert-not* 'chargbuffer-insert-at/chargbuffer! (eq? left  csp-src))
          (assert-not* 'chargbuffer-insert-at/chargbuffer! (eq? right csp-src))
          (cond
            ((fxzero? idx)
              (charspan-insert-left/charspan! left csp-src src-start src-end))
            ((fx=? idx (chargbuffer-length cgb))
              (charspan-insert-right/charspan! right csp-src src-start src-end))
            (else
              (chargbuffer-split-at! cgb idx)
              (charspan-insert-right/charspan! left csp-src src-start src-end))))))
    ((cgb idx csp-src)
      (chargbuffer-insert-at/charspan! cgb idx csp-src 0 (charspan-length csp-src)))))


;; read elements in range [src-start, src-end) from chargbuffer cgb-src
;; and insert them into chargbuffer at position idx
(define chargbuffer-insert-at/chargbuffer!
  (case-lambda
    ((cgb idx cgb-src src-start src-end)
      (assert* 'chargbuffer-insert-at/chargbuffer! (fx<=? 0 idx (chargbuffer-length cgb)))
      (assert* 'chargbuffer-insert-at/chargbuffer! (fx<=?* 0 src-start src-end (chargbuffer-length cgb-src)))
      (when (fx<? src-start src-end)
        (assert-not* 'chargbuffer-insert-at/chargbuffer! (eq? cgb cgb-src))
        (let* ((left   (ch< cgb-src))
               (right  (ch> cgb-src))
               (left-n (charspan-length left)))
          (when (fx<? src-start left-n)
            ; read from (ch< cgb-src) and insert into cgb
            (let ((pos (fxmin left-n src-end)))
              (chargbuffer-insert-at/charspan! cgb idx left src-start pos)
              (set! idx (fx+ idx (fx- pos src-start)))
              (set! src-start pos)))
          (when (fx<? src-start src-end)
            ; read from (ch> cgb-src) and insert into cgb
            (let ((pos (fx- src-start left-n))
                  (end (fx- src-end   left-n)))
              (chargbuffer-insert-at/charspan! cgb idx right pos end))))))
    ((cgb idx cgb-src)
      (chargbuffer-insert-at/chargbuffer! cgb idx cgb-src 0 (chargbuffer-length cgb-src)))))


;; remove elements in range [start, end) from chargbuffer cgb
(define (chargbuffer-delete! cgb start end)
  (let* ((left    (ch< cgb))
         (right   (ch> cgb))
         (left-n  (charspan-length left))
         (right-n (charspan-length right))
         (len     (fx+ left-n right-n))
         (n       (fx- end start)))
    (assert* 'chargbuffer-delete! (fx<=?* 0 start end len))
    (cond
      ((fxzero? n)
        (void)) ; nothing to do
      ((fxzero? start)
        (let ((head (fxmin n left-n)))
          (charspan-delete-left! left head)
          (charspan-delete-left! right (fx- n head))))
      ((fx=? end left-n)
        (charspan-delete-right! left n))
      ((fx=? start left-n)
        (charspan-delete-left! right n))
      ((fx=? end len)
        (let ((tail (fxmin n right-n)))
          (charspan-delete-right! right tail)
          (charspan-delete-right! left (fx- n tail))))
      (else
        (chargbuffer-split-at! cgb end)
        (charspan-delete-right! left n)))))


;; create and return a closure that iterates on elements of chargbuffer cgb.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in chargbuffer cgb and #t,
;; or (values #<unspecified> #f) if end of chargbuffer is reached.
(define in-chargbuffer
  (case-lambda
    ((cgb start end step)
      (assert* 'in-chargbuffer (fx<=?* 0 start end (chargbuffer-length cgb)))
      (assert* 'in-chargbuffer (fx>=? step 0))
      (let ((%in-chargbuffer ; name shown when displaying the closure
              (lambda ()
                (if (fx<? start end)
                  (let ((elem (chargbuffer-ref cgb start)))
                    (set! start (fx+ start step))
                    (values elem #t))
                  (values #\nul #f)))))
        %in-chargbuffer))
    ((cgb start end)
      (in-chargbuffer cgb start end 1))
    ((cgb)
      (in-chargbuffer cgb 0 (chargbuffer-length cgb) 1))))


(define (chargbuffer-iterate cgb proc)
  (do ((i 0 (fx1+ i))
       (n (chargbuffer-length cgb)))
    ((or (fx>=? i n) (not (proc i (chargbuffer-ref cgb i))))
     (fx>=? i n))))

;; customize how "chargbuffer" objects are printed
(record-writer (record-type-descriptor %chargbuffer)
  (lambda (cgb port writer)
    (display "(string->chargbuffer* " port)
    (write (chargbuffer->string cgb) port)
    (display ")" port)))

) ; close library
