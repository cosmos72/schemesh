;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit vscreen (0 1))
  (export
    make-vscreen vscreen?
    vscreen-cursor-x   vscreen-cursor-x-set!   vscreen-cursor-y   vscreen-cursor-y-set!
    vscreen-width      vscreen-width-set!      vscreen-height     vscreen-height-set!
    vscreen-prompt-len vscreen-prompt-len-set!)

  (import
    (rnrs)
    (only (chezscheme) fx1+ fx1- record-writer)
    (schemesh bootstrap)       ;; while
    (schemesh containers span)
    (schemesh containers charline)
    (schemesh containers charlines))


;; copy-pasted from containers/gbuffer.ss
(define-record-type
  (%gbuffer %make-gbuffer %gbuffer?)
  (fields
     (mutable left)
     (mutable right))
  (nongenerative #{%gbuffer ejch98ka4vi1n9dn4ybq4gzwe-0}))


;; copy-pasted from containers/charlines.ss
(define-record-type
  (%charlines %make-charlines %charlines?)
  (parent %gbuffer)
  (fields
    (mutable dirty-y-start)
    (mutable dirty-y-end))
  (nongenerative #{%charlines lf2lr8d65f8atnffcpi1ja7l0-439}))


;; vscreen is an in-memory representation of user-typed input
;; and how it is split in multiple lines, each limited by screen width
(define-record-type
  (%vscreen %make-vscreen vscreen?)
  (parent %charlines)
  (fields
    (mutable cursor-x   vscreen-cursor-x   vscreen-cursor-x-set!  ) ;; cursor x position
    (mutable cursor-y   vscreen-cursor-y   vscreen-cursor-y-set!  ) ;; cursor y position
    (mutable width      vscreen-width      vscreen-width-set!     ) ;; screen width
    (mutable height     vscreen-height     vscreen-height-set!    ) ;; screen height
    (mutable prompt-len vscreen-prompt-len vscreen-prompt-len-set!) ;; prompt length
  )
  ; (nongenerative #{%vscreen jwb9hnxrmryx5cyqntydbbdqu-526})
)

(define (assert-vscreen? who screen)
  (unless (vscreen? screen)
    (assertion-violation who "not a vscreen" screen)))


;; create a vscreen
(define (make-vscreen)
  (%make-vscreen (span) (span (charline)) (greatest-fixnum) 0 0 0 80 24 0))


;; return vscreen line at specified y, or #f if y is out of range
(define (vscreen-line-at-y screen y)
  (if (fx<? -1 y (charlines-length screen))
    (charlines-ref screen y)
    #f))

;; return vscreen width i.e. maximum charline length at specified y
;; it is equal (vscreen-width), except when y = 0 where prompt length must be subtracted
(define (vscreen-width-at-y screen y)
  (fx- (vscreen-width screen)
       (if (fxzero? y) (vscreen-prompt-len screen) 0)))

;; append characters to vscreen line at y removing them from the beginning of line at y+1
;; and repeat with lines *without* an implicit newline at y+2... until line is refilled
;; up to vscreen width
(define (vscreen-underflow-at-y screen y)
  (let ((line1 (vscreen-line-at-y screen y)))
    (when (and line1 (not (charline-nl? line1)))
      (let ((n (fx- (vscreen-width-at-y screen) (charline-length line1))))
        (while (and (fx>? n 0) (fx<? (fx1+ y) (charlines-length screen)))
          (let* ((line2 (vscreen-line-at-y screen (fx1+ y)))
                 (i     (fxmin n (charline-length line2))))
            (assert-charline? "vscreen-erase/right!" line2)
            ;; insert chars into line1
            (charline-insert-at/cbuf! line1 (charline-length line1) line2 0 i)
            ;; remove chars from line2
            (charline-erase-at! line2 0 i)
            (set! n (fx- n i))
            (when (charline-empty? line2)
              (when (charline-nl? line2)
                (charline-nl?-set! line1 #t)
                (set! n 0)) ;; newline found, stop refilling line1
              ;; we consumed all chars from line2, erase it
              (charlines-erase-at/cline! screen (fx1+ y)))))))))


;; erase n characters to the right of vscreen cursor.
;; if the implicit newline of a charline is erased, the following line(s)
;; are merged into the current charline and reflowed according to vscreen width
(define (vscreen-erase-right! screen n)
  (when (fx>? n 0)
    (let ((x (vscreen-cursor-x screen))
          (y (vscreen-cursor-y screen)))
      (while (and (fx>? n 0) (fx<? -1 y (charlines-length screen)))
        (let* ((line (charlines-ref screen y))
               (len  (charline-length line))
               (i    (fxmin n (fx- len x))))
          (when (fx>? i 0)
            ;; erase i characters
            (charline-erase-at! line x i)
            (set! n (fx- n i))
            (set! len (fx- len i)))
          (when (and (fx>? n 0) (charline-nl? line))
            ;; also erase the implicit newline
            (charline-nl?-set! line #t)
            (set! n (fx1- n)))
          (when (fx>=? x len)
            ;; erased until the end of line, continue with next line
            (if (and (fxzero? len) (not charline-nl? line))
              ;; line is now empty, remove it
              (charlines-erase-at/cline! screen y)
              ;; line is not empty, move to next line
              (begin
                (set! x 0)
                (set! y (fx1+ y))))))))
    (vscreen-underflow-at-y screen (vscreen-cursor-y screen))))





;; customize how "vscreen" objects are printed
(record-writer (record-type-descriptor %charlines)
  (lambda (sp port writer)
    (display "#<vscreen>" port)))

) ; close library
