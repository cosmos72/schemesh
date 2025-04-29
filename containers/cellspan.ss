;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;  define Scheme type "cellspan", a resizeable vector of cells  ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (schemesh containers cellspan (0 8 3))
  (export
    list->cellspan string->cellspan make-cellspan
    cellspan cellspan? assert-cellspan? cellspan-length cellspan-empty? cellspan-clear!
    cellspan-capacity cellspan-capacity-left cellspan-capacity-right
    cellspan-ref cellspan-ref-right
    cellspan-set! cellspan-fill!  cellspan-copy cellspan-copy!
    cellspan-reserve-left! cellspan-reserve-right! cellspan-resize-left! cellspan-resize-right!
    cellspan-insert-left!         cellspan-insert-right!
    cellspan-insert-left/cellspan!  cellspan-insert-right/cellspan!
    cellspan-delete-left!         cellspan-delete-right! cellspan-index cellspan-index/char
    cellspan-iterate in-cellspan  cellspan-write)

  (import
    (rnrs)
    (only (chezscheme)                  fx1+ fx1- record-writer bytevector-truncate! void)
    (only (schemesh bootstrap)          assert* assert-not* fx<=?*)
    (only (schemesh containers list)    for-list)
    (only (schemesh containers palette) cell->char)
    (schemesh containers cellvector))



(define-record-type (%cellspan %make-cellspan cellspan?)
  (fields
     (mutable beg cellspan-beg cellspan-beg-set!)
     (mutable end cellspan-end cellspan-end-set!)
     (mutable vec cellspan-vec cellspan-vec-set!)) ;; cellvector
  (nongenerative %cellspan-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define (assert-cellspan? who line)
  (unless (cellspan? line)
    (assertion-violation who "not a cellspan" line)))


(define cellspan-peek-beg cellspan-beg)
(define cellspan-peek-end cellspan-end)
(define cellspan-peek-data cellspan-vec)


;; create a cellspan containing n cells.
;; If fill is specified, then cellspan is filled with it.
;; Otherwise it is filled with (cell #\nul).
(define make-cellspan
  (case-lambda
    ((n)
      (%make-cellspan 0 n (make-cellvector n)))
    ((n fill)
      (%make-cellspan 0 n (make-cellvector n fill)))))


;; c-list must be a list of cells or characters
(define (list->cellspan c-list)
  (let ((clv (list->cellvector c-list)))
    (%make-cellspan 0 (cellvector-length clv) clv)))


;; create cellspan copying contents of specified string
(define (string->cellspan str)
  (let ((n (string-length str)))
    (%make-cellspan 0 n (string->cellvector str))))


;; c-list must be a list of cells or characters
(define (cellspan . c-list)
  (list->cellspan c-list))

(define (cellspan-length csp)
  (fx- (cellspan-end csp) (cellspan-beg csp)))

;; return length of internal cellvector, i.e. maximum number of elements
;; that can be stored without reallocating
(define (cellspan-capacity csp)
  (cellspan-length (cellspan-vec csp)))

(define (cellspan-empty? csp)
  (fx>=? (cellspan-beg csp) (cellspan-end csp)))

(define (cellspan-clear! csp)
  (cellspan-beg-set! csp 0)
  (cellspan-end-set! csp 0))

(define cellspan-ref
  (case-lambda
    ((csp idx)
      (assert* 'cellspan-ref (fx<? -1 idx (cellspan-length csp)))
      (cellvector-ref (cellspan-vec csp) (fx+ idx (cellspan-beg csp))))
    ((csp)
      (cellspan-ref csp 0))))

(define cellspan-ref-right
  (case-lambda
    ((csp idx)
      (assert* 'cellspan-ref-right (fx<? -1 idx (cellspan-length csp)))
      (let ((pos (fx- (cellspan-length csp) (fx1+ idx))))
        (cellvector-ref (cellspan-vec csp) (fx+ pos (cellspan-beg csp)))))
    ((csp)
      (cellspan-ref-right csp 0))))

;; c must be a character or cell
(define (cellspan-set! csp idx c)
  (assert* 'cellspan-set! (fx<? -1 idx (cellspan-length csp)))
  (cellvector-set! (cellspan-vec csp) (fx+ idx (cellspan-beg csp)) c))

;; c must be a character or cell
(define cellspan-fill!
  (case-lambda
    ((csp start end c)
      (assert* 'cellspan-fill! (fx<=?* 0 start end (cellspan-length csp)))
      (let ((offset (cellspan-beg csp)))
        (cellvector-fill! (cellspan-vec csp) (fx+ offset start) (fx+ offset end) c)))
    ((csp cell)
      (cellspan-fill! csp 0 (cellspan-length csp) cell))))


;; make a copy of cellspan and return it
(define (cellspan-copy src)
  (let* ((n (cellspan-length src))
         (dst (make-cellspan n)))
    (cellvector-copy! (cellspan-vec src) (cellspan-beg src)
                      (cellspan-vec dst) (cellspan-beg dst) n)
    dst))

;; copy n cells from a cellspan to another one
(define (cellspan-copy! src src-start dst dst-start n)
  (assert* 'cellspan-copy! (fx<=?* 0 src-start (fx+ src-start n) (cellspan-length src)))
  (assert* 'cellspan-copy! (fx<=?* 0 dst-start (fx+ dst-start n) (cellspan-length dst)))
  (cellvector-copy! (cellspan-vec src) (fx+ src-start (cellspan-beg src))
                    (cellspan-vec dst) (fx+ dst-start (cellspan-beg dst)) n))



(define (cellspan-reallocate-left! csp len cap)
  (assert* 'cellspan-reallocate-left! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (cellspan-length csp)))
        (old-vec  (cellspan-vec csp))
        (new-vec  (make-cellvector cap))
        (new-beg  (fx- cap len)))
    (cellvector-copy! old-vec (cellspan-beg csp) new-vec new-beg copy-len)
    (cellspan-beg-set! csp new-beg)
    (cellspan-end-set! csp cap)
    (cellspan-vec-set! csp new-vec)))

(define (cellspan-reallocate-right! csp len cap)
  (assert* 'cellspan-reallocate-right! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (cellspan-length csp)))
        (old-vec (cellspan-vec csp))
        (new-vec (make-cellvector cap)))
    (cellvector-copy! old-vec (cellspan-beg csp) new-vec 0 copy-len)
    (cellspan-beg-set! csp 0)
    (cellspan-end-set! csp len)
    (cellspan-vec-set! csp new-vec)))

;; return distance between begin of internal cellvector and last element
(define (cellspan-capacity-left csp)
  (cellspan-end csp))

;; return distance between first element and end of internal cellvector
(define (cellspan-capacity-right csp)
  (fx- (cellvector-length (cellspan-vec csp)) (cellspan-beg csp)))

;; ensure distance between begin of internal cellvector and last element is >= n.
;; does NOT change the length
(define (cellspan-reserve-left! csp len)
  (assert* 'cellspan-reserve-left! (fx>=? len 0))
  (let ((vec (cellspan-vec csp))
        (cap-left (cellspan-capacity-left csp)))
    (cond
      ((fx<=? len cap-left)
       ; nothing to do
       (void))
      ((fx<=? len (cellvector-length vec))
        ; cellvector is large enough, move elements to the back
        (let* ((cap (cellspan-capacity csp))
               (old-len (cellspan-length csp))
               (new-beg (fx- cap old-len)))
          (cellvector-copy! vec (cellspan-beg csp) vec new-beg old-len)
          (cellspan-beg-set! csp new-beg)
          (cellspan-end-set! csp cap)))
      (else
       ; cellvector is too small, reallocate it
       (let ((new-cap (fxmax 8 len (fx* 2 cap-left))))
         (cellspan-reallocate-left! csp (cellspan-length csp) new-cap))))))

;; ensure distance between first element and end of internal cellvector is >= n.
;; does NOT change the length
(define (cellspan-reserve-right! csp len)
  (assert* 'cellspan-reserve-right! (fx>=? len 0))
  (let ((vec (cellspan-vec csp))
        (cap-right (cellspan-capacity-right csp)))
    (cond
      ((fx<=? len cap-right)
       ; nothing to do
       (void))
      ((fx<=? len (cellvector-length vec))
        ; cellvector is large enough, move elements to the front
        (let ((len (cellspan-length csp)))
          (cellvector-copy! vec (cellspan-beg csp) vec 0 len)
          (cellspan-beg-set! csp 0)
          (cellspan-end-set! csp len)))
      (else
       ; cellvector is too small, reallocate it
       (let ((new-cap (fxmax 8 len (fx* 2 cap-right))))
         (cellspan-reallocate-right! csp (cellspan-length csp) new-cap))))))

;; grow or shrink cellspan on the left (front), set length to n
(define (cellspan-resize-left! csp len)
  (assert* 'cellspan-resize-left! (fx>=? len 0))
  (cellspan-reserve-left! csp len)
  (assert* 'cellspan-resize-left! (fx>=? (cellspan-capacity-left csp) len))
  (cellspan-beg-set! csp (fx- (cellspan-end csp) len)))

;; grow or shrink cellspan on the right (back), set length to n
(define (cellspan-resize-right! csp len)
  (assert* 'cellspan-resize-right! (fx>=? len 0))
  (cellspan-reserve-right! csp len)
  (assert* 'cellspan-resize-right! (fx>=? (cellspan-capacity-right csp) len))
  (cellspan-end-set! csp (fx+ len (cellspan-beg csp))))


;; each c must be a character or cell
(define cellspan-insert-left!
  (case-lambda
    ((csp)
      (void))
    ((csp c1)
      (cellspan-resize-left! csp (fx1+ (cellspan-length csp)))
      (cellspan-set! csp 0 c1))
    ((csp c1 c2)
      (cellspan-resize-left! csp (fx+ 2 (cellspan-length csp)))
      (cellspan-set! csp 0 c1)
      (cellspan-set! csp 1 c2))
    ((csp . c-list)
      (cellspan-resize-left! csp (fx+ (cellspan-length csp) (length c-list)))
      (do ((pos 0 (fx1+ pos))
           (tail c-list (cdr tail)))
          ((null? tail))
        (cellspan-set! csp pos (car tail))))))


;; each c must be a character or cell
(define cellspan-insert-right!
  (case-lambda
    ((csp)
      (void))
    ((csp c1)
      (let ((len (cellspan-length csp)))
        (cellspan-resize-right! csp (fx1+ len))
        (cellspan-set! csp len c1)))
    ((csp c1 c2)
      (let ((len (cellspan-length csp)))
        (cellspan-resize-right! csp (fx+ 2 len))
        (cellspan-set! csp len c1)
        (cellspan-set! csp (fx1+ len) c2)))
    ((csp . c-list)
      (let ((len (cellspan-length csp)))
        (cellspan-resize-right! csp (fx+ len (length c-list)))
        (do ((pos len (fx1- pos))
             (tail c-list (cdr tail)))
            ((null? tail))
          (cellspan-set! csp pos (car tail)))))))


;; insert range [start, end) of cellspan csp-src at the beginning of cellspan csp-dst
(define cellspan-insert-left/cellspan!
  (case-lambda
    ((csp-dst csp-src src-start src-end)
      (assert* 'cellspan-insert-left/cellspan! (fx<=?* 0 src-start src-end (cellspan-length csp-src)))
      (assert-not* 'cellspan-insert-left/cellspan! (eq? csp-dst csp-src))
      (when (fx<? src-start src-end)
        (let ((src-n (fx- src-end src-start)))
          (cellspan-resize-left! csp-dst (fx+ src-n (cellspan-length csp-dst)))
          (cellspan-copy! csp-src src-start csp-dst 0 src-n))))
    ((csp-dst csp-src)
      (cellspan-insert-left/cellspan! csp-dst csp-src 0 (cellspan-length csp-src)))))



;; append range [start, end) of cellspan csp-src at the end of cellspan csp-dst
(define cellspan-insert-right/cellspan!
  (case-lambda
    ((csp-dst csp-src src-start src-end)
      (assert* 'cellspan-insert-right/cellspan! (fx<=?* 0 src-start src-end (cellspan-length csp-src)))
      (assert-not* 'cellspan-insert-right/cellspan! (eq? csp-dst csp-src))
      (when (fx<? src-start src-end)
        (let ((pos (cellspan-length csp-dst))
              (src-n (fx- src-end src-start)))
          (cellspan-resize-right! csp-dst (fx+ pos src-n))
          (cellspan-copy! csp-src src-start csp-dst pos src-n))))
    ((csp-dst csp-src)
      (cellspan-insert-right/cellspan! csp-dst csp-src 0 (cellspan-length csp-src)))))


;; erase n elements at the left (front) of cellspan
(define (cellspan-delete-left! csp n)
  (assert* 'cellspan-delete-left! (fx<=? 0 n (cellspan-length csp)))
  (unless (fxzero? n)
    (cellspan-beg-set! csp (fx+ n (cellspan-beg csp)))))

;; erase n elements at the right (back) of cellspan
(define (cellspan-delete-right! csp n)
  (assert* 'cellspan-delete-right! (fx<=? 0 n (cellspan-length csp)))
  (unless (fxzero? n)
    (cellspan-end-set! csp (fx- (cellspan-end csp) n))))


;; iterate on cellspan elements in range [start, end) and return the index
;; of first element that causes (predicate elem) to return truish.
;;
;; Return #f if no such element is found.
(define cellspan-index
  (case-lambda
    ((sp start end predicate)
      (assert* 'cellspan-index (fx<=?* 0 start end (cellspan-length sp)))
      (do ((i start (fx1+ i)))
          ((or (fx>=? i end) (predicate (cellspan-ref sp i)))
            (if (fx>=? i end) #f i))))
    ((sp predicate)
      (cellspan-index sp 0 (cellspan-length sp) predicate))))


;; iterate on cellspan elements in range [start, end) and return
;; the index of first cellspan element equal to character ch.
;;
;; Return #f if no such element is found.
(define cellspan-index/char
  (case-lambda
    ((sp start end ch)
      (cellspan-index sp start end (lambda (e) (char=? ch (cell->char e)))))
    ((sp ch)
      (cellspan-index sp (lambda (e) (char=? ch (cell->char e)))))))


;; create and return a closure that iterates on elements of cellspan csp.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in cellspan csp and #t,
;; or (values #<unspecified> #f) if end of cellspan is reached.
(define in-cellspan
  (case-lambda
    ((csp start end step)
      (assert* 'in-cellspan (fx<=?* 0 start end (cellspan-length csp)))
      (assert* 'in-cellspan (fx>=? step 0))
      (let ((%in-cellspan ; name shown when displaying the closure
              (lambda ()
                (if (fx<? start end)
                  (let ((elem (cellspan-ref csp start)))
                    (set! start (fx+ start step))
                    (values elem #t))
                  (values #\nul #f)))))
        %in-cellspan))
    ((csp start end)
      (in-cellspan csp start end 1))
    ((csp)
      (in-cellspan csp 0 (cellspan-length csp) 1))))


(define (cellspan-iterate csp proc)
  (let ((start (cellspan-beg csp))
        (end   (cellspan-end csp))
        (s     (cellspan-vec csp)))
    (do ((i start (fx1+ i)))
      ((or (fx>=? i end) (not (proc (fx- i start) (cellspan-ref s i))))
        (fx>=? i end)))))


(define cellspan-write
  (case-lambda
    ((csp start end port)
      (assert* 'cellspan-display! (fx<=?* 0 start end (cellspan-length csp)))
      (let ((offset (cellspan-beg csp)))
        (cellvector-write (cellspan-vec csp) (fx+ offset start) (fx+ offset end) port)))
    ((csp port)
      (cellvector-write (cellspan-vec csp) (cellspan-beg csp) (cellspan-end csp) port))))


;; customize how cellspan objects are printed
(record-writer (record-type-descriptor %cellspan)
  (lambda (csp port writer)
    (display "(string->cellspan " port)
    (cellspan-write csp port)
    (display ")" port)))

) ; close library
