;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file vscreen/all.ss


;; type vhistory is a gbuffer containing vlines elements (the history itself)
(define-record-type (%vhistory %make-vhistory vhistory?)
  (parent %gbuffer)
  (fields
    (mutable path vhistory-path %vhistory-path-set!)) ; #f or string path where to load/save history
  (nongenerative %vhistory-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define (vhistory . vals)
  (for-list ((val vals)) (assert-vlines? 'vhistory val))
  (%make-vhistory (span) (list->span vals) #f))


(define (make-vhistory n)
  ; optimization: (vhistory-ref/cow) returns a copy-on-write clone of i-th vline,
  ; thus we can reuse the same empty (vlines) for all elements
  (%make-vhistory (span) (make-span n (vlines)) #f))


(define (vhistory-path-set! hist path)
  (when (and path (not (string? path)))
    (raise-assertf 'vhistory-path-set! "~s is not #f or string" path))
  (%vhistory-path-set! hist path))


(define vhistory-empty? gbuffer-empty?)
(define vhistory-length gbuffer-length)
(define vhistory-clear! gbuffer-clear!)

;; iterate on vhistory lines, and call (proc i lines) on each one.
;; Stops iterating if (proc ...) returns #f.
;;
;; Returns #t if all calls to (proc i lines) returned truish,
;; otherwise returns #f.
;;
;; The implementation of (proc ...) can call directly or indirectly functions
;; that inspect the vhistory without modifying it,
;; and can also inspect lines.
;;
;; It must NOT call any function that modifies the lines or vhistory
;; (set elements, insert or erase elements, change the size or capacity, etc).
(define vhistory-iterate gbuffer-iterate)

;; return a copy-on-write clone of i-th vlines in history
(define (vhistory-ref/cow hist idx)
  (vlines-copy-on-write (gbuffer-ref hist idx)))


;; if i is in range, set i-th vlines in history to a shallow copy of lines.
;; otherwise append shallow copy of lines to history.
;; do NOT insert lines in history if they are equal to another vlines at a nearby index.
;; returns two values:
;;   the inserted shallow copy of lines - or the existing lines that prevented insertion
;;   the index where lines were actually stored - or the index of existing lines that prevented insertion
(define (vhistory-set*! hist idx lines)
  (assert-vlines? 'vhistory-set*! lines)
  (let* ((len       (gbuffer-length hist))
         (idx-clamp (fxmax 0 (fxmin idx len)))
         (idx-eq    (%vlines-find-nearby hist idx-clamp lines))
         (idx       (or idx-eq idx-clamp))
         (lines (if idx-eq
                  (gbuffer-ref hist idx-eq)
                  ; make a shallow copy of lines. Also helps in case lines is not
                  ; a vlines but a subclass of it, for example a vscreen
                  (vlines-shallow-copy lines))))
    (unless idx-eq
      (if (fx>=? idx len)
        (gbuffer-insert-at! hist idx lines)
        (gbuffer-set! hist idx lines)))
    (values lines idx)))


;; remove empty vlines before index idx in vhistory.
;; stop as soon as a non-empty vlines is found.
(define (vhistory-delete-empty-lines! hist idx)
  (let ((i (fx1- (fxmin idx (vhistory-length hist)))))
    (while (and (fx>=? i 0) (%vlines-empty? (gbuffer-ref hist i)))
      (gbuffer-delete! hist i (fx1+ i))
      (set! i (fx1- i)))
    i))


(define (%vlines-empty? lines)
  (or (vlines-empty? lines)
      (and (fx=? 1 (vlines-length lines))
           (vline-empty? (vlines-ref lines 0)))))


;; search for vlines equal to lines
;; at vhistory indexes (fx1- idx) idx (fx1+ idx)
;;
;; return index of equal vlines if found,
;; otherwise return #f
(define (%vlines-find-nearby hist idx lines)
  (let* ((len   (gbuffer-length hist))
         (start (fxmax 0 (fx1- idx)))
         (end   (fxmin len (fx+ 2 idx)))
         (ret   #f))
    (do ((i start (fx1+ i)))
        ((or ret (fx>=? i end)) ret)
      (when (vlines-equal/chars? (gbuffer-ref hist i) lines)
        (set! ret i)))))

;; search for first vlines in range [start, end) that begins with same characters
;; as the range [0 0, prefix-x prefix-y) of prefix-lines.
;;
;; return index of such vlines if found,
;; otherwise return #f
(define (vhistory-index/starts-with hist start end prefix-lines prefix-x prefix-y)
  (let ((start (fxmax 0 start))
        (end   (fxmin end (vhistory-length hist))))
    (do ((i start (fx1+ i)))
        ((or (fx>=? i end) (vlines-starts-with? (gbuffer-ref hist i) prefix-lines prefix-x prefix-y))
         (if (fx<? i end) i #f)))))


;; search for last vlines in range [start, end) that begins with same characters
;; as the range [0 0, prefix-x prefix-y) of prefix-lines.
;;
;; return index of such vlines if found,
;; otherwise return #f
(define (vhistory-index-right/starts-with hist start end prefix-lines prefix-x prefix-y)
  (let ((start (fxmax 0 start))
        (end   (fxmin end (vhistory-length hist))))
    (do ((i (fx1- end) (fx1- i)))
      ((or (fx<? i start) (vlines-starts-with? (gbuffer-ref hist i) prefix-lines prefix-x prefix-y))
       (if (fx>=? i start) i #f)))))
