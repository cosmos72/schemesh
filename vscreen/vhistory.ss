;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file vscreen/all.ss


;; Type vhistory is a gbuffer containing vlines elements (the history itself).
;;
;; Each gbuffer element is a cons (mutable-vlines . immutable-vlines)
;;   where the former can be accessed and modified by the user,
;;   while the latter is saved to disk by (vhistory-file-save) and (vhistory-port-save)
;;
;; This allows user to edit history items, while still saving the original unedited items
;;
(define-record-type (%vhistory %make-vhistory vhistory?)
  (parent %gbuffer)
  (fields
    (mutable path vhistory-path %vhistory-path-set!) ; #f or string path where to load/save history
    (mutable last-temporary? %vhistory-last-temporary? vhistory-last-temporary-set!))
  (nongenerative %vhistory-7c46d04b-34f4-4046-b5c7-b63753c1be40))


(define (vhistory . vals)
  (let* ((n (do ((i 0 (fx1+ i))
                 (l vals (cdr l)))
                ((null? l) i)
              (let ((val (car vals)))
                (assert-vlines? 'vhistory val))))
         (vec (make-vector n)))
    (do ((i 0 (fx1+ i))
         (l vals (cdr l)))
        ((null? l))
      (let ((val (car vals)))
        (vector-set! vec i (cons val val))))
    (%make-vhistory (span) (vector->span* vec) #f #f)))


(define (vhistory-path-set! hist path)
  (when (and path (not (string? path)))
    (raise-assertf 'vhistory-path-set! "~s is not #f or string" path))
  (%vhistory-path-set! hist path))

(define vhistory-empty? gbuffer-empty?)

(define vhistory-length gbuffer-length)

(define (vhistory-clear! hist)
  (gbuffer-clear! hist)
  (vhistory-last-temporary-set! hist #f))


;; return i-th vlines in history
;; prerequisite: (fx<? -1 idx (vhistory-length gb))
(define (vhistory-ref hist idx)
  (car (gbuffer-ref hist idx)))


;; return #t if i-th vlines in history was set with (vhistory-set!) AFTER the vlines was added to history
;; prerequisite: (fx<? -1 idx (vhistory-length gb))
(define (vhistory-ref-edited? hist idx)
  (let ((pair (gbuffer-ref hist idx)))
    (not (vlines-string=? (car pair) (cdr pair)))))


;; remove elements in range [start, end) from history
;; prerequisite: (fx<=? 0 start end (vhistory-length gb))
(define (vhistory-delete! hist start end)
  (let ((len (vhistory-length hist)))
    (gbuffer-delete! hist start end)
    (when (and (fx<? start len) (fx=? end len))
      (vhistory-last-temporary-set! hist #f))))


;; insert vlines into history at position idx
;; prerequisite: (fx<=? 0 idx (vhistory-length gb))
(define (vhistory-insert-at! hist idx lines)
  (assert-vlines? 'vhistory-insert-at! lines)
  (let ((last? (fx=? idx (vhistory-length hist))))
    (gbuffer-insert-at! hist idx (cons lines lines))
    (when last?
      (vhistory-last-temporary-set! hist #f))))


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
(define vhistory-iterate
  (case-lambda
    ((hist start end proc)
      (assert* 'vhistory-iterate (fx<=?* 0 start end (vhistory-length hist)))
      (assert* 'vhistory-iterate (procedure? proc))
      (let %vhistory-iterate ((hist hist) (proc proc) (ret #t) (i start) (n end))
        (if (fx<? i n)
          (let ((ret (proc i (vhistory-ref hist i))))
            (and ret (%vhistory-iterate hist proc ret (fx1+ i) n)))
          ret)))
    ((hist proc)
      (vhistory-iterate hist 0 (vhistory-length hist) proc))))


;; return a copy-on-write clone of i-th vlines in history
;; prerequisite: (fx<? -1 idx (vhistory-length gb))
(define (vhistory-ref/cow hist idx)
  (vlines-copy-on-write (vhistory-ref hist idx)))


;; remove consecutive duplicates at end of vhistory.
;;
;; limitation: removes immutable lines too, even if they are not duplicates
(define (vhistory-delete-duplicates-right! hist)
  (let* ((old-len (vhistory-length hist))
         (new-len (fx+ 2
           (let %loop ((i (fx- old-len 2)))
             (if (fx>=? i 0)
               (let ((line1 (vhistory-ref hist i))
                     (line2 (vhistory-ref hist (fx1+ i))))
                 (if (vlines-string=? line1 line2)
                   (%loop (fx1- i))
                   i))
               i)))))
     (unless (fx=? new-len old-len)
       (vhistory-delete! hist new-len old-len))))


(define (vhistory-last-temporary? hist)
  (and (%vhistory-last-temporary? hist) (not (vhistory-empty? hist))))


;; step 1:
;;   if temporary? is #f and (vhistory-last-temporary?) is truish, then:
;;     remove last vhistory element and set (vhistory-last-temporary?) to #f
;;
;; step 2:
;;   if lines are NOT equal to last vlines in history, then:
;;     if (vhistory-last-temporary?) is #f, append a shallow copy of lines to history;
;;     otherwise overwrite last history entry with a shallow copy of lines.
;;
;; step 3:
;;   set (vhistory-last-temporary?) to temporary?
;;
;; return the stored shallow copy of lines - or the existing lines that prevented storing them
(define (vhistory-maybe-insert-right! hist lines temporary?)
  ;; (debugf "> vhistory-maybe-insert-right! history ~s" hist)
  (when (and (not temporary?) (vhistory-last-temporary? hist))
    (let ((len (vhistory-length hist)))
      (vhistory-delete! hist (fx1- len) len)))

  (let* ((len    (vhistory-length hist))
         (idx-eq (if temporary? #f (%vhistory-find-nearby hist len lines)))
         (lines  (if idx-eq (vhistory-ref hist idx-eq)
                            (vlines-shallow-copy lines))))
    ;; (debugf ". vhistory-maybe-insert-right! lines ~s, temporary? ~s, vhistory-last-temporary? ~s, idx-eq ~s, len ~s" lines temporary? (vhistory-last-temporary? hist) idx-eq len)
    (unless idx-eq
      (let ((idx (if (vhistory-last-temporary? hist) (fx1- len) len)))
        (gbuffer-insert-at! hist idx (cons lines lines))))
    (vhistory-last-temporary-set! hist (and temporary? #t)) ;; store #f or #t, not arbitrary datum
    ;; (debugf "< vhistory-maybe-insert-right! lines ~s, history ~s" lines hist)
    lines))


;; store a shallow copy of vlines into history at index idx.
;;
;; return unspecified value
;; prerequisite: (fx<? -1 idx (vhistory-length gb))
(define (vhistory-set! hist idx lines)
  (assert* 'vhistory-set! (vhistory? hist))
  (assert-vlines? 'vhistory-set! lines)
  ;; (debugf "vhistory-set! idx ~s, len ~s, lines ~s" idx (vhistory-length hist) lines)
  (set-car! (gbuffer-ref hist idx) (vlines-shallow-copy lines)))


;; remove consecutive empty vlines before index idx in vhistory.
;; stop as soon as a non-empty vlines is found.
;; return index of non-empty vline found, or -1
;;
;; limitation: removes immutable lines too, even if they are not empty
;; prerequisite: (fx<=? 0 idx (vhistory-length gb))
(define (vhistory-delete-empty-lines! hist idx)
  (let* ((old-len (vhistory-length hist))
         (pos
           (let %loop ((i (fx1- (fxmin idx old-len))))
             (if (and (fx>=? i 0) (%vlines-empty? (vhistory-ref hist i)))
               (%loop (fx1- i))
               i))))
    (vhistory-delete! hist (fx1+ pos) old-len)
    pos))


(define (%vlines-empty? lines)
  (or (vlines-empty? lines)
      (and (fx=? 1 (vlines-length lines))
           (vline-empty? (vlines-ref lines 0)))))


;; search for vlines equal to lines
;; at vhistory indexes (fx1- idx) idx (fx1+ idx)
;;
;; return index of equal vlines if found,
;; otherwise return #f
(define (%vhistory-find-nearby hist idx lines)
  (let* ((len   (gbuffer-length hist))
         (start (fxmax 0   (fx1- idx)))
         (end   (fxmin len (fx+ 2 idx))))
    (let %loop ((i start))
      (and (fx<? i end)
           (if (vlines-string=? (vhistory-ref hist i) lines)
             i
             (%loop (fx1+ i)))))))

;; search for first vlines in range [start, end) that begins with same characters
;; as the range [0 0, prefix-x prefix-y) of prefix-lines, ignoring vlines that are equal to prefix-lines.
;;
;; return index of such vlines if found,
;; otherwise return #f
(define (vhistory-index/starts-with hist start end prefix-lines prefix-x prefix-y)
  (let ((start (fxmax 0 start))
        (end   (fxmin end (gbuffer-length hist))))
    (let %loop ((i start))
      (cond
        ((fx>=? i end)
          #f)
        ((and (vlines-starts-with? (vhistory-ref hist i) prefix-lines prefix-x prefix-y)
              (not (vlines-string=? (vhistory-ref hist i) prefix-lines))) ;; ignore lines equal to prefix-lines
          i)
        (else
          (%loop (fx1+ i)))))))


;; search for last vlines in range [start, end) that begins with same characters
;; as the range [0 0, prefix-x prefix-y) of prefix-lines, ignoring vlines that are equal to prefix-lines.
;;
;; return index of such vlines if found,
;; otherwise return #f
(define (vhistory-index-right/starts-with hist start end prefix-lines prefix-x prefix-y)
  (let ((start (fxmax 0 start))
        (end   (fxmin end (gbuffer-length hist))))
    (let %loop ((i (fx1- end)))
      (cond
        ((fx<? i start)
          #f)
        ((and (vlines-starts-with? (vhistory-ref hist i) prefix-lines prefix-x prefix-y)
              (not (vlines-string=? (vhistory-ref hist i) prefix-lines))) ;; ignore lines equal to prefix-lines
          i)
        (else
          (%loop (fx1- i)))))))
