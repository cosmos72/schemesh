;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers misc (0 8 1))
  (export
    in-bytevector list->bytevector subbytevector
    bytevector-fill-range! bytevector-index/u8 bytevector-compare
    bytevector<=? bytevector<? bytevector>=? bytevector>? bytevector-iterate

    in-exact-range in-fixnum-range in-flonum-range in-range

    in-fxvector
    in-flvector ; requires Chez Scheme >= 10.0.0
    in-vector vector-copy! subvector vector-fill-range! vector-iterate vector->hashtable! vector-range->list)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)         bytevector foreign-procedure fx1+ fx1- fxvector-length fxvector-ref
                               import include meta-cond library-exports scheme-version)
    (only (schemesh bootstrap) assert* generate-pretty-temporaries raise-errorf))


(include "containers/bytevector.ss")
(include "containers/vector.ss")



;; create and return a closure that returns exact real numbers in the range [start, end)
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in range [start, end) and #t,
;; or (values #<unspecified> #f) if end of range is reached.
;;
;; If step is zero, the closure may never reach end of range.
(define in-exact-range
  (case-lambda
    ((start end step)
      (assert* 'in-range (exact? start))
      (assert* 'in-range (exact? end))
      (assert* 'in-range (exact? step))
      (if (< step 0)
        (lambda ()
          (if (> start end)
            (let ((ret start))
              (set! start (+ start step))
              (values ret #t))
            (values end #f)))
        (lambda ()
          (if (< start end)
            (let ((ret start))
              (set! start (+ start step))
              (values ret #t))
            (values end #f)))))
    ((start end)
      (in-exact-range start end 1))
    ((end)
      (in-exact-range 0 end 1))))


;; create and return a closure that returns fixnums in the range [start, end)
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in range [start, end) and #t,
;; or (values #<unspecified> #f) if end of range is reached.
;;
;; If step is zero, the closure may never reach end of range.
(define in-fixnum-range
  (case-lambda
    ((start end step)
      (assert* 'in-fixnum-range (fixnum? start))
      (assert* 'in-fixnum-range (fixnum? end))
      (assert* 'in-fixnum-range (fixnum? step))
      (if (fx>=? step 0)
        (lambda ()
          (if (fx<? start end)
            (let ((ret start))
              (set! start (fx+ start step))
              (values ret #t))
            (values end #f)))
        (lambda ()
          (if (fx>? start end)
            (let ((ret start))
              (set! start (fx+ start step))
              (values ret #t))
            (values end #f)))))
    ((start end)
      (in-fixnum-range start end 1))
    ((end)
      (in-fixnum-range 0 end 1))))


;; create and return a closure that returns inexact real numbers in the range [start, end)
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in range [start, end) and #t,
;; or (values #<unspecified> #f) if end of range is reached.
;;
;; If step is zero or a very small inexact real, the closure may never reach end of range.
(define in-flonum-range
  (case-lambda
    ((start end step)
      (let ((start (real->flonum start))
            (end   (real->flonum end))
            (step  (real->flonum step)))
        (if (fl>=? step 0.0)
          (lambda ()
            (if (fl<? start end)
              (let ((ret start))
                (set! start (fl+ start step))
                (values ret #t))
              (values end #f)))
          (lambda ()
            (if (fl>? start end)
              (let ((ret start))
                (set! start (fl+ start step))
                (values ret #t))
              (values end #f))))))
    ((start end)
      (in-flonum-range start end 1.0))
    ((end)
      (in-flonum-range 0.0 end 1.0))))


;; create and return a closure that returns exact or inexact real numbers in the range [start, end)
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in range [start, end) and #t,
;; or (values #<unspecified> #f) if end of range is reached.
;;
;; If step is zero or a very small inexact real, the closure may never reach end of range.
;;
;; Implementation:
;;  if all arguments are fixnums, calls (in-fixnum-range)
;;  otherwise, if all arguments are exact, calls (in-exact-range)
;;  otherwise calls (in-flonum-range)
(define in-range
  (case-lambda
    ((start end step)
      (assert* 'in-range (real? start))
      (assert* 'in-range (real? end))
      (assert* 'in-range (real? step))
      (cond
        ((and (fixnum? start) (fixnum? end) (fixnum? step))
          (in-fixnum-range start end step))
        ((and (exact? start) (exact? end) (exact? step))
          (in-exact-range start end step))
        (else
          (in-flonum-range start end step))))
    ((start end)
      (in-range start end 1))
    ((end)
      (in-range 0 end 1))))


) ; close library
