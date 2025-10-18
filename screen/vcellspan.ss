;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;  define Scheme type "vcellspan", a resizeable vector of cells  ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (schemesh screen vcellspan (0 9 2))
  (export
    list->vcellspan string->vcellspan make-vcellspan
    vcellspan vcellspan? assert-vcellspan? vcellspan-length vcellspan-empty? vcellspan-clear!
    vcellspan-capacity vcellspan-capacity-left vcellspan-capacity-right
    vcellspan-ref vcellspan-ref-right
    vcellspan-set! vcellspan-fill!  vcellspan-copy vcellspan-copy!
    vcellspan-reserve-left! vcellspan-reserve-right! vcellspan-resize-left! vcellspan-resize-right!
    vcellspan-insert-left!         vcellspan-insert-right!
    vcellspan-insert-left/vcellspan!  vcellspan-insert-right/vcellspan!
    vcellspan-delete-left!         vcellspan-delete-right! vcellspan-index vcellspan-index/char
    vcellspan-iterate in-vcellspan  vcellspan-write)

  (import
    (rnrs)
    (only (chezscheme)                fx1+ fx1- record-writer bytevector-truncate! void)
    (only (schemesh bootstrap)        assert* assert-not* fx<=?*)
    (only (schemesh containers list)  for-list)
    (schemesh containers charspan)
    (only (schemesh screen vcell)     vcell->char)
    (schemesh screen vcellvector))



(define-record-type (%vcellspan %make-vcellspan vcellspan?)
  (fields
     (mutable beg vcellspan-beg vcellspan-beg-set!)
     (mutable end vcellspan-end vcellspan-end-set!)
     (mutable vec vcellspan-vec vcellspan-vec-set!)) ;; vcellvector
  (nongenerative %vcellspan-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define (assert-vcellspan? who line)
  (unless (vcellspan? line)
    (assertion-violation who "not a vcellspan" line)))


(define vcellspan-peek-beg vcellspan-beg)
(define vcellspan-peek-end vcellspan-end)
(define vcellspan-peek-data vcellspan-vec)


;; create a vcellspan containing n cells.
;; If fill is specified, then vcellspan is filled with it.
;; Otherwise it is filled with (vcell #\nul).
(define make-vcellspan
  (case-lambda
    ((n)
      (%make-vcellspan 0 n (make-vcellvector n)))
    ((n fill)
      (%make-vcellspan 0 n (make-vcellvector n fill)))))


;; c-list must be a list of cells or characters
(define (list->vcellspan c-list)
  (let ((clv (list->vcellvector c-list)))
    (%make-vcellspan 0 (vcellvector-length clv) clv)))


;; create vcellspan copying contents of specified string
(define string->vcellspan
  (case-lambda
    ((str start end)
      (%make-vcellspan 0 (fx- end start) (string->vcellvector str start end)))
    ((str)
      (string->vcellspan str 0 (string-length str)))))

;; c-list must be a list of cells or characters
(define (vcellspan . c-list)
  (list->vcellspan c-list))

(define (vcellspan-length csp)
  (fx- (vcellspan-end csp) (vcellspan-beg csp)))

;; return length of internal vcellvector, i.e. maximum number of elements
;; that can be stored without reallocating
(define (vcellspan-capacity csp)
  (vcellvector-length (vcellspan-vec csp)))

(define (vcellspan-empty? csp)
  (fx>=? (vcellspan-beg csp) (vcellspan-end csp)))

(define (vcellspan-clear! csp)
  (vcellspan-beg-set! csp 0)
  (vcellspan-end-set! csp 0))

(define vcellspan-ref
  (case-lambda
    ((csp idx)
      (assert* 'vcellspan-ref (fx<? -1 idx (vcellspan-length csp)))
      (vcellvector-ref (vcellspan-vec csp) (fx+ idx (vcellspan-beg csp))))
    ((csp)
      (vcellspan-ref csp 0))))

(define vcellspan-ref-right
  (case-lambda
    ((csp idx)
      (assert* 'vcellspan-ref-right (fx<? -1 idx (vcellspan-length csp)))
      (let ((pos (fx- (vcellspan-length csp) (fx1+ idx))))
        (vcellvector-ref (vcellspan-vec csp) (fx+ pos (vcellspan-beg csp)))))
    ((csp)
      (vcellspan-ref-right csp 0))))

;; c must be a character or cell
(define (vcellspan-set! csp idx c)
  (assert* 'vcellspan-set! (fx<? -1 idx (vcellspan-length csp)))
  (vcellvector-set! (vcellspan-vec csp) (fx+ idx (vcellspan-beg csp)) c))

;; c must be a character or cell
(define vcellspan-fill!
  (case-lambda
    ((csp start end c)
      (assert* 'vcellspan-fill! (fx<=?* 0 start end (vcellspan-length csp)))
      (let ((offset (vcellspan-beg csp)))
        (vcellvector-fill! (vcellspan-vec csp) (fx+ offset start) (fx+ offset end) c)))
    ((csp cell)
      (vcellspan-fill! csp 0 (vcellspan-length csp) cell))))


;; make a copy of vcellspan and return it
(define (vcellspan-copy src)
  (let* ((n (vcellspan-length src))
         (dst (make-vcellspan n)))
    (vcellvector-copy! (vcellspan-vec src) (vcellspan-beg src)
                       (vcellspan-vec dst) (vcellspan-beg dst) n)
    dst))

;; copy n cells from a vcellspan to another one
(define (vcellspan-copy! src src-start dst dst-start n)
  (assert* 'vcellspan-copy! (fx<=?* 0 src-start (fx+ src-start n) (vcellspan-length src)))
  (assert* 'vcellspan-copy! (fx<=?* 0 dst-start (fx+ dst-start n) (vcellspan-length dst)))
  (vcellvector-copy! (vcellspan-vec src) (fx+ src-start (vcellspan-beg src))
                     (vcellspan-vec dst) (fx+ dst-start (vcellspan-beg dst)) n))



(define (vcellspan-reallocate-left! csp len cap)
  (assert* 'vcellspan-reallocate-left! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (vcellspan-length csp)))
        (old-vec  (vcellspan-vec csp))
        (new-vec  (make-vcellvector cap))
        (new-beg  (fx- cap len)))
    (vcellvector-copy! old-vec (vcellspan-beg csp) new-vec new-beg copy-len)
    (vcellspan-beg-set! csp new-beg)
    (vcellspan-end-set! csp cap)
    (vcellspan-vec-set! csp new-vec)))

(define (vcellspan-reallocate-right! csp len cap)
  (assert* 'vcellspan-reallocate-right! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (vcellspan-length csp)))
        (old-vec (vcellspan-vec csp))
        (new-vec (make-vcellvector cap)))
    (vcellvector-copy! old-vec (vcellspan-beg csp) new-vec 0 copy-len)
    (vcellspan-beg-set! csp 0)
    (vcellspan-end-set! csp len)
    (vcellspan-vec-set! csp new-vec)))

;; return distance between begin of internal vcellvector and last element
(define (vcellspan-capacity-left csp)
  (vcellspan-end csp))

;; return distance between first element and end of internal vcellvector
(define (vcellspan-capacity-right csp)
  (fx- (vcellvector-length (vcellspan-vec csp)) (vcellspan-beg csp)))

;; ensure distance between begin of internal vcellvector and last element is >= n.
;; does NOT change the length
(define (vcellspan-reserve-left! csp len)
  (assert* 'vcellspan-reserve-left! (fx>=? len 0))
  (let ((vec      (vcellspan-vec csp))
        (cap-left (vcellspan-capacity-left csp)))
    (cond
      ((fx<=? len cap-left)
        ;; nothing to do
        (void))
      ((fx<=? len (vcellvector-length vec))
        ;; vcellvector is large enough, move elements to the back
        (let* ((cap     (vcellspan-capacity csp))
               (old-len (vcellspan-length csp))
               (new-beg (fx- cap old-len)))
          (vcellvector-copy! vec (vcellspan-beg csp) vec new-beg old-len)
          (vcellspan-beg-set! csp new-beg)
          (vcellspan-end-set! csp cap)))
      (else
        ;; vcellvector is too small, reallocate it
        (let ((new-cap (fxmax 8 len (fx* 2 cap-left))))
          (vcellspan-reallocate-left! csp (vcellspan-length csp) new-cap))))))

;; ensure distance between first element and end of internal vcellvector is >= n.
;; does NOT change the length
(define (vcellspan-reserve-right! csp len)
  (assert* 'vcellspan-reserve-right! (fx>=? len 0))
  (let ((vec       (vcellspan-vec csp))
        (cap-right (vcellspan-capacity-right csp)))
    (cond
      ((fx<=? len cap-right)
        ;; nothing to do
        (void))
      ((fx<=? len (vcellvector-length vec))
        ;; vcellvector is large enough, move elements to the front
        (let ((len (vcellspan-length csp)))
          (vcellvector-copy! vec (vcellspan-beg csp) vec 0 len)
          (vcellspan-beg-set! csp 0)
          (vcellspan-end-set! csp len)))
      (else
        ;; vcellvector is too small, reallocate it
        (let ((new-cap (fxmax 8 len (fx* 2 cap-right))))
          (vcellspan-reallocate-right! csp (vcellspan-length csp) new-cap))))))

;; grow or shrink vcellspan on the left (front), set length to n
(define (vcellspan-resize-left! csp len)
  (assert* 'vcellspan-resize-left! (fx>=? len 0))
  (vcellspan-reserve-left! csp len)
  (assert* 'vcellspan-resize-left! (fx>=? (vcellspan-capacity-left csp) len))
  (vcellspan-beg-set! csp (fx- (vcellspan-end csp) len)))

;; grow or shrink vcellspan on the right (back), set length to n
(define (vcellspan-resize-right! csp len)
  (assert* 'vcellspan-resize-right! (fx>=? len 0))
  (vcellspan-reserve-right! csp len)
  (assert* 'vcellspan-resize-right! (fx>=? (vcellspan-capacity-right csp) len))
  (vcellspan-end-set! csp (fx+ len (vcellspan-beg csp))))


;; each c must be a character or cell
(define vcellspan-insert-left!
  (case-lambda
    ((csp)
      (void))
    ((csp c1)
      (vcellspan-resize-left! csp (fx1+ (vcellspan-length csp)))
      (vcellspan-set! csp 0 c1))
    ((csp c1 c2)
      (vcellspan-resize-left! csp (fx+ 2 (vcellspan-length csp)))
      (vcellspan-set! csp 0 c1)
      (vcellspan-set! csp 1 c2))
    ((csp . c-list)
      (vcellspan-resize-left! csp (fx+ (vcellspan-length csp) (length c-list)))
      (do ((pos 0 (fx1+ pos))
           (tail c-list (cdr tail)))
          ((null? tail))
        (vcellspan-set! csp pos (car tail))))))


;; each c must be a character or cell
(define vcellspan-insert-right!
  (case-lambda
    ((csp)
      (void))
    ((csp c1)
      (let ((len (vcellspan-length csp)))
        (vcellspan-resize-right! csp (fx1+ len))
        (vcellspan-set! csp len c1)))
    ((csp c1 c2)
      (let ((len (vcellspan-length csp)))
        (vcellspan-resize-right! csp (fx+ 2 len))
        (vcellspan-set! csp len c1)
        (vcellspan-set! csp (fx1+ len) c2)))
    ((csp . c-list)
      (let ((len (vcellspan-length csp)))
        (vcellspan-resize-right! csp (fx+ len (length c-list)))
        (do ((pos len (fx1- pos))
             (tail c-list (cdr tail)))
            ((null? tail))
          (vcellspan-set! csp pos (car tail)))))))


;; insert range [start, end) of vcellspan csp-src at the beginning of vcellspan csp-dst
(define vcellspan-insert-left/vcellspan!
  (case-lambda
    ((csp-dst csp-src src-start src-end)
      (assert* 'vcellspan-insert-left/vcellspan! (fx<=?* 0 src-start src-end (vcellspan-length csp-src)))
      (assert-not* 'vcellspan-insert-left/vcellspan! (eq? csp-dst csp-src))
      (when (fx<? src-start src-end)
        (let ((src-n (fx- src-end src-start)))
          (vcellspan-resize-left! csp-dst (fx+ src-n (vcellspan-length csp-dst)))
          (vcellspan-copy! csp-src src-start csp-dst 0 src-n))))
    ((csp-dst csp-src)
      (vcellspan-insert-left/vcellspan! csp-dst csp-src 0 (vcellspan-length csp-src)))))



;; append range [start, end) of vcellspan csp-src at the end of vcellspan csp-dst
(define vcellspan-insert-right/vcellspan!
  (case-lambda
    ((csp-dst csp-src src-start src-end)
      (assert* 'vcellspan-insert-right/vcellspan! (fx<=?* 0 src-start src-end (vcellspan-length csp-src)))
      (assert-not* 'vcellspan-insert-right/vcellspan! (eq? csp-dst csp-src))
      (when (fx<? src-start src-end)
        (let ((pos (vcellspan-length csp-dst))
              (src-n (fx- src-end src-start)))
          (vcellspan-resize-right! csp-dst (fx+ pos src-n))
          (vcellspan-copy! csp-src src-start csp-dst pos src-n))))
    ((csp-dst csp-src)
      (vcellspan-insert-right/vcellspan! csp-dst csp-src 0 (vcellspan-length csp-src)))))


;; erase n elements at the left (front) of vcellspan
(define (vcellspan-delete-left! csp n)
  (assert* 'vcellspan-delete-left! (fx<=? 0 n (vcellspan-length csp)))
  (unless (fxzero? n)
    (vcellspan-beg-set! csp (fx+ n (vcellspan-beg csp)))))

;; erase n elements at the right (back) of vcellspan
(define (vcellspan-delete-right! csp n)
  (assert* 'vcellspan-delete-right! (fx<=? 0 n (vcellspan-length csp)))
  (unless (fxzero? n)
    (vcellspan-end-set! csp (fx- (vcellspan-end csp) n))))


;; iterate on vcellspan elements in range [start, end) and return the index
;; of first element that causes (predicate elem) to return truish.
;;
;; Return #f if no such element is found.
(define vcellspan-index
  (case-lambda
    ((sp start end predicate)
      (assert* 'vcellspan-index (fx<=?* 0 start end (vcellspan-length sp)))
      (do ((i start (fx1+ i)))
          ((or (fx>=? i end) (predicate (vcellspan-ref sp i)))
            (if (fx>=? i end) #f i))))
    ((sp predicate)
      (vcellspan-index sp 0 (vcellspan-length sp) predicate))))


;; iterate on vcellspan elements in range [start, end) and return
;; the index of first vcellspan element equal to character ch.
;;
;; Return #f if no such element is found.
(define vcellspan-index/char
  (case-lambda
    ((sp start end ch)
      (vcellspan-index sp start end (lambda (e) (char=? ch (vcell->char e)))))
    ((sp ch)
      (vcellspan-index sp (lambda (e) (char=? ch (vcell->char e)))))))


;; create and return a closure that iterates on elements of vcellspan csp.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in vcellspan csp and #t,
;; or (values #<unspecified> #f) if end of vcellspan is reached.
(define in-vcellspan
  (case-lambda
    ((csp start end step)
      (assert* 'in-vcellspan (fx<=?* 0 start end (vcellspan-length csp)))
      (assert* 'in-vcellspan (fx>=? step 0))
      (let ((%in-vcellspan ; name shown when displaying the closure
              (lambda ()
                (if (fx<? start end)
                  (let ((elem (vcellspan-ref csp start)))
                    (set! start (fx+ start step))
                    (values elem #t))
                  (values #\nul #f)))))
        %in-vcellspan))
    ((csp start end)
      (in-vcellspan csp start end 1))
    ((csp)
      (in-vcellspan csp 0 (vcellspan-length csp) 1))))


(define (vcellspan-iterate csp proc)
  (let ((start (vcellspan-beg csp))
        (end   (vcellspan-end csp))
        (vec   (vcellspan-vec csp)))
    (do ((i start (fx1+ i)))
      ((or (fx>=? i end) (not (proc (fx- i start) (vcellspan-ref vec i))))
        (fx>=? i end)))))


(define vcellspan-write
  (case-lambda
    ((csp start end port)
      (assert* 'vcellspan-display! (fx<=?* 0 start end (vcellspan-length csp)))
      (let ((offset (vcellspan-beg csp)))
        (vcellvector-write (vcellspan-vec csp) (fx+ offset start) (fx+ offset end) port)))
    ((csp port)
      (vcellvector-write (vcellspan-vec csp) (vcellspan-beg csp) (vcellspan-end csp) port))))


;; customize how vcellspan objects are printed
(record-writer (record-type-descriptor %vcellspan)
  (lambda (csp port writer)
    (display "(string->vcellspan " port)
    (vcellspan-write csp port)
    (display ")" port)))

) ; close library
