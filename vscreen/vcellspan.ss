;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file vscreen/all.ss


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;  define Scheme type "vcellspan", a resizeable vector of cells  ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;; Otherwise it is filled with (vcell #\x0).
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

(define (vcellspan-length vsp)
  (fx- (vcellspan-end vsp) (vcellspan-beg vsp)))

;; return length of internal vcellvector, i.e. maximum number of elements
;; that can be stored without reallocating
(define (vcellspan-capacity vsp)
  (vcellvector-length (vcellspan-vec vsp)))

(define (vcellspan-empty? vsp)
  (fx>=? (vcellspan-beg vsp) (vcellspan-end vsp)))

(define (vcellspan-clear! vsp)
  (vcellspan-beg-set! vsp 0)
  (vcellspan-end-set! vsp 0))

(define vcellspan-ref
  (case-lambda
    ((vsp idx)
      (assert* 'vcellspan-ref (fx<? -1 idx (vcellspan-length vsp)))
      (vcellvector-ref (vcellspan-vec vsp) (fx+ idx (vcellspan-beg vsp))))
    ((vsp)
      (vcellspan-ref vsp 0))))

(define (vcellspan-ref-right vsp idx)
  (assert* 'vcellspan-ref-right (fx<? -1 idx (vcellspan-length vsp)))
  (let ((pos (fx- (vcellspan-length vsp) (fx1+ idx))))
    (vcellvector-ref (vcellspan-vec vsp) (fx+ pos (vcellspan-beg vsp)))))

;; c must be a character or cell
(define (vcellspan-set! vsp idx c)
  (assert* 'vcellspan-set! (fx<? -1 idx (vcellspan-length vsp)))
  (vcellvector-set! (vcellspan-vec vsp) (fx+ idx (vcellspan-beg vsp)) c))

;; c must be a character or cell
(define vcellspan-fill!
  (case-lambda
    ((vsp start end c)
      (assert* 'vcellspan-fill! (fx<=?* 0 start end (vcellspan-length vsp)))
      (let ((offset (vcellspan-beg vsp)))
        (vcellvector-fill! (vcellspan-vec vsp) (fx+ offset start) (fx+ offset end) c)))
    ((vsp cell)
      (vcellspan-fill! vsp 0 (vcellspan-length vsp) cell))))


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


(define (vcellspan-reallocate-left! vsp len cap)
  (assert* 'vcellspan-reallocate-left! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (vcellspan-length vsp)))
        (old-vec  (vcellspan-vec vsp))
        (new-vec  (make-vcellvector cap))
        (new-beg  (fx- cap len)))
    (vcellvector-copy! old-vec (vcellspan-beg vsp) new-vec new-beg copy-len)
    (vcellspan-beg-set! vsp new-beg)
    (vcellspan-end-set! vsp cap)
    (vcellspan-vec-set! vsp new-vec)))

(define (vcellspan-reallocate-right! vsp len cap)
  (assert* 'vcellspan-reallocate-right! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (vcellspan-length vsp)))
        (old-vec (vcellspan-vec vsp))
        (new-vec (make-vcellvector cap)))
    (vcellvector-copy! old-vec (vcellspan-beg vsp) new-vec 0 copy-len)
    (vcellspan-beg-set! vsp 0)
    (vcellspan-end-set! vsp len)
    (vcellspan-vec-set! vsp new-vec)))

;; return distance between begin of internal vcellvector and last element
(define (vcellspan-capacity-left vsp)
  (vcellspan-end vsp))

;; return distance between first element and end of internal vcellvector
(define (vcellspan-capacity-right vsp)
  (fx- (vcellvector-length (vcellspan-vec vsp)) (vcellspan-beg vsp)))

;; ensure distance between begin of internal vcellvector and last element is >= n.
;; does NOT change the length
(define (vcellspan-reserve-left! vsp len)
  (assert* 'vcellspan-reserve-left! (fx>=? len 0))
  (let ((vec      (vcellspan-vec vsp))
        (cap-left (vcellspan-capacity-left vsp)))
    (cond
      ((fx<=? len cap-left)
        ;; nothing to do
        (void))
      ((fx<=? len (vcellvector-length vec))
        ;; vcellvector is large enough, move elements to the back
        (let* ((cap     (vcellspan-capacity vsp))
               (old-len (vcellspan-length vsp))
               (new-beg (fx- cap old-len)))
          (vcellvector-copy! vec (vcellspan-beg vsp) vec new-beg old-len)
          (vcellspan-beg-set! vsp new-beg)
          (vcellspan-end-set! vsp cap)))
      (else
        ;; vcellvector is too small, reallocate it
        (let ((new-cap (fxmax 8 len (fx* 2 cap-left))))
          (vcellspan-reallocate-left! vsp (vcellspan-length vsp) new-cap))))))

;; ensure distance between first element and end of internal vcellvector is >= n.
;; does NOT change the length
(define (vcellspan-reserve-right! vsp len)
  (assert* 'vcellspan-reserve-right! (fx>=? len 0))
  (let ((vec       (vcellspan-vec vsp))
        (cap-right (vcellspan-capacity-right vsp)))
    (cond
      ((fx<=? len cap-right)
        ;; nothing to do
        (void))
      ((fx<=? len (vcellvector-length vec))
        ;; vcellvector is large enough, move elements to the front
        (let ((len (vcellspan-length vsp)))
          (vcellvector-copy! vec (vcellspan-beg vsp) vec 0 len)
          (vcellspan-beg-set! vsp 0)
          (vcellspan-end-set! vsp len)))
      (else
        ;; vcellvector is too small, reallocate it
        (let ((new-cap (fxmax 8 len (fx* 2 cap-right))))
          (vcellspan-reallocate-right! vsp (vcellspan-length vsp) new-cap))))))

;; grow or shrink vcellspan on the left (front), set length to n
(define (vcellspan-resize-left! vsp len)
  (assert* 'vcellspan-resize-left! (fx>=? len 0))
  (vcellspan-reserve-left! vsp len)
  (assert* 'vcellspan-resize-left! (fx>=? (vcellspan-capacity-left vsp) len))
  (vcellspan-beg-set! vsp (fx- (vcellspan-end vsp) len)))

;; grow or shrink vcellspan on the right (back), set length to n
(define (vcellspan-resize-right! vsp len)
  (assert* 'vcellspan-resize-right! (fx>=? len 0))
  (vcellspan-reserve-right! vsp len)
  (assert* 'vcellspan-resize-right! (fx>=? (vcellspan-capacity-right vsp) len))
  (vcellspan-end-set! vsp (fx+ len (vcellspan-beg vsp))))


;; each c must be a character or cell
(define vcellspan-insert-left!
  (case-lambda
    ((vsp)
      (void))
    ((vsp c1)
      (vcellspan-resize-left! vsp (fx1+ (vcellspan-length vsp)))
      (vcellspan-set! vsp 0 c1))
    ((vsp c1 c2)
      (vcellspan-resize-left! vsp (fx+ 2 (vcellspan-length vsp)))
      (vcellspan-set! vsp 0 c1)
      (vcellspan-set! vsp 1 c2))
    ((vsp . c-list)
      (vcellspan-resize-left! vsp (fx+ (vcellspan-length vsp) (length c-list)))
      (do ((pos 0 (fx1+ pos))
           (tail c-list (cdr tail)))
          ((null? tail))
        (vcellspan-set! vsp pos (car tail))))))


;; each c must be a character or cell
(define vcellspan-insert-right!
  (case-lambda
    ((vsp)
      (void))
    ((vsp c1)
      (let ((len (vcellspan-length vsp)))
        (vcellspan-resize-right! vsp (fx1+ len))
        (vcellspan-set! vsp len c1)))
    ((vsp c1 c2)
      (let ((len (vcellspan-length vsp)))
        (vcellspan-resize-right! vsp (fx+ 2 len))
        (vcellspan-set! vsp len c1)
        (vcellspan-set! vsp (fx1+ len) c2)))
    ((vsp . c-list)
      (let ((len (vcellspan-length vsp)))
        (vcellspan-resize-right! vsp (fx+ len (length c-list)))
        (do ((pos len (fx1- pos))
             (tail c-list (cdr tail)))
            ((null? tail))
          (vcellspan-set! vsp pos (car tail)))))))


;; insert range [start, end) of vcellspan vsp-src at the beginning of vcellspan vsp-dst
(define vcellspan-insert-left/vcellspan!
  (case-lambda
    ((vsp-dst vsp-src src-start src-end)
      (assert* 'vcellspan-insert-left/vcellspan! (fx<=?* 0 src-start src-end (vcellspan-length vsp-src)))
      (assert-not* 'vcellspan-insert-left/vcellspan! (eq? vsp-dst vsp-src))
      (when (fx<? src-start src-end)
        (let ((src-n (fx- src-end src-start)))
          (vcellspan-resize-left! vsp-dst (fx+ src-n (vcellspan-length vsp-dst)))
          (vcellspan-copy! vsp-src src-start vsp-dst 0 src-n))))
    ((vsp-dst vsp-src)
      (vcellspan-insert-left/vcellspan! vsp-dst vsp-src 0 (vcellspan-length vsp-src)))))


;; append range [start, end) of vcellspan vsp-src at the end of vcellspan vsp-dst
(define vcellspan-insert-right/vcellspan!
  (case-lambda
    ((vsp-dst vsp-src src-start src-end)
      (assert* 'vcellspan-insert-right/vcellspan! (fx<=?* 0 src-start src-end (vcellspan-length vsp-src)))
      (assert-not* 'vcellspan-insert-right/vcellspan! (eq? vsp-dst vsp-src))
      (when (fx<? src-start src-end)
        (let ((pos (vcellspan-length vsp-dst))
              (src-n (fx- src-end src-start)))
          (vcellspan-resize-right! vsp-dst (fx+ pos src-n))
          (vcellspan-copy! vsp-src src-start vsp-dst pos src-n))))
    ((vsp-dst vsp-src)
      (vcellspan-insert-right/vcellspan! vsp-dst vsp-src 0 (vcellspan-length vsp-src)))))


;; erase n elements at the left (front) of vcellspan
(define (vcellspan-delete-left! vsp n)
  (assert* 'vcellspan-delete-left! (fx<=? 0 n (vcellspan-length vsp)))
  (unless (fxzero? n)
    (vcellspan-beg-set! vsp (fx+ n (vcellspan-beg vsp)))))

;; erase n elements at the right (back) of vcellspan
(define (vcellspan-delete-right! vsp n)
  (assert* 'vcellspan-delete-right! (fx<=? 0 n (vcellspan-length vsp)))
  (unless (fxzero? n)
    (vcellspan-end-set! vsp (fx- (vcellspan-end vsp) n))))


;; iterate on vcellspan elements in range [start, end) and return the index
;; of first element that causes (predicate elem) to return truish.
;;
;; Return #f if no such element is found.
(define vcellspan-index
  (case-lambda
    ((vsp char-or-pred start end)
      (assert* 'vcellspan-index (fx<=?* 0 start end (vcellspan-length vsp)))
      (let ((pred (if (char? char-or-pred)
                    (lambda (e) (char=? char-or-pred (vcell->char e)))
                    char-or-pred)))
        (assert* 'vcellspan-index (procedure? pred))
        (assert* 'vcellspan-index (logbit? 1 (procedure-arity-mask pred)))
        (do ((i start (fx1+ i)))
            ((or (fx>=? i end) (pred (vcellspan-ref vsp i)))
              (and (fx<? i end) i)))))
    ((vsp char-or-pred)
      (vcellspan-index vsp char-or-pred 0 (vcellspan-length vsp)))))


;; create and return a closure that iterates on elements of vcellspan vsp.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in vcellspan vsp and #t,
;; or (values #<unspecified> #f) if end of vcellspan is reached.
(define in-vcellspan
  (case-lambda
    ((vsp start end step)
      (assert* 'in-vcellspan (fx<=?* 0 start end (vcellspan-length vsp)))
      (assert* 'in-vcellspan (fx>=? step 0))
      (let ((%in-vcellspan ; name shown when displaying the closure
              (lambda ()
                (if (fx<? start end)
                  (let ((elem (vcellspan-ref vsp start)))
                    (set! start (fx+ start step))
                    (values elem #t))
                  (values #\x0 #f)))))
        %in-vcellspan))
    ((vsp start end)
      (in-vcellspan vsp start end 1))
    ((vsp)
      (in-vcellspan vsp 0 (vcellspan-length vsp) 1))))


;; (vcellspan-iterate vsp proc) iterates on all elements of given vcellspan vsp,
;; and calls (proc index elem) on each element. stops iterating if (proc ...) returns #f
;;
;; (proc index elem) can call directly or indirectly functions
;; that inspect the vcellspan(s) elements, and can also call (vcellspan-set! vsp ...).
;;
;; It must NOT call any other function that modifies the vcellspan (insert or erase elements,
;; change the vcellspan size or capacity, etc).
;;
;; If no vcellspan is specified, the loop finishes when body ... evaluates to #f
;;
;; Returns value of last call to (proc index elem), or #t if (proc index elem) was never called.
(define vcellspan-iterate
  (case-lambda
    ((vsp start end proc)
      (assert* 'vcellspan-iterate (fx<=?* 0 start end (vcellspan-length vsp)))
      (assert* 'vcellspan-iterate (procedure? proc))
      (let %vcellspan-iterate ((vsp vsp) (proc proc) (ret #t) (i start) (n end))
        (if (fx<? i n)
          (let ((ret (proc i (vcellspan-ref vsp i))))
            (and ret (%vcellspan-iterate vsp proc ret (fx1+ i) n)))
          ret)))
    ((vsp proc)
      (vcellspan-iterate vsp 0 (vcellspan-length vsp) proc))))


(define vcellspan-write
  (case-lambda
    ((vsp start end port)
      (assert* 'vcellspan-display! (fx<=?* 0 start end (vcellspan-length vsp)))
      (let ((offset (vcellspan-beg vsp)))
        (vcellvector-write (vcellspan-vec vsp) (fx+ offset start) (fx+ offset end) port)))
    ((vsp port)
      (vcellvector-write (vcellspan-vec vsp) (vcellspan-beg vsp) (vcellspan-end vsp) port))))
