;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh lineedit io (0 1))
  (export open-charline-input-port open-charlines-input-port)
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme) fx1+)
    (schemesh bootstrap)      ; until
    (schemesh lineedit charlines))

; helper for input port wrapping a charline
(define-record-type icport
  (fields
    (immutable line)  ; charline to read from
    (mutable   pos))  ; position in charline
  (nongenerative #{icport do8t0druatc9fhaize8s4a1wd-20}))

;; Read logical #\newline from a charline wrapped inside an icport:
;;
;; if (charline-length line) equals line-pos, and (charline-nl? line) is true,
;; and str-pos is smaller than (string-length str),
;;
;; then write #\newline into str at index str-pos, update position of icport and return #t;
;; otherwise do nothing and return #f.
(define (icport-maybe-read-newline p str str-pos line line-pos)
  (if (and (charline-nl? line)
           (fx=? line-pos (charline-length line))
           (fx<? str-pos (string-length str)))
    (begin
      (string-set! str str-pos #\newline)
      (icport-pos-set! p (fx1+ line-pos))
      #t)
    #f))


(define (icport-read-string p str start n)
  (assert (fx>=? start 0))
  (assert (fx>=? n 0))
  (assert (fx<=? (fx+ start n) (string-length str)))
  (let* ((line     (icport-line p))
         (line-pos (icport-pos p))
         (line-len (charline-length line))
         (ret-n    (fxmin n (fx- line-len line-pos))))
    (when (fx>? ret-n 0)
      (do ((i 0 (fx1+ i)))
          ((fx>=? i ret-n)
            (icport-pos-set! p (fx+ ret-n line-pos)))
        (string-set! str (fx+ i start) (charline-ref line (fx+ i line-pos)))))
    (when (and (fx>=? ret-n 0)
               (icport-maybe-read-newline p str (fx+ start ret-n) line line-pos))
      (set! ret-n (fx1+ ret-n)))
    (fxmax 0 ret-n)))


(define icport-position icport-pos)

(define (icport-position-set! p pos)
  (unless (and (fixnum? pos)
               (fx>=? pos 0)
               (fx<=? pos (charline-length (icport-line p))))
    (raise (make-i/o-invalid-position-error pos)))
  (icport-pos-set! p pos))

; create an input port wrapping a charline
(define (open-charline-input-port cgb)
  (assert (charline? cgb))
  (let ((p (make-icport cgb 0)))
    (make-custom-textual-input-port
      "charline-input-port"
      (lambda (str start n) (icport-read-string p str start n))
      (lambda ()            (icport-position     p))
      (lambda (pos)         (icport-position-set! p pos))
      #f))) ; nothing to do on (close-input-port)


; helper for input port wrapping a charlines
(define-record-type iport
  (fields
    (mutable   x)      ; position in y-th charline
    (mutable   y)      ; position in charlines
    (immutable lines)) ; charlines to read from
  (nongenerative #{iport cy8auoivds3jpsu99eergcoco-20}))

(define (iport-read-char p)
  (let* ((x (iport-x p))
         (y (iport-y p))
         (lines     (iport-lines p))
         (line      (charlines-ref lines y))
         (line-len  (charline-length line)))
    (cond
      ((fx<? x line-len)
        (iport-x-set! p (fx1+ x))
        (charline-ref line x))
      ((and (fx=? x line-len) (charline-nl? line))
        (iport-x-set! p (fx1+ x))
        #\newline)
      ((fx<? (fx1+ y) (charlines-length lines))
        ; end-of-line reached, go to next line
        (iport-x-set! p 0)
        (iport-y-set! p (fx1+ y))
        (iport-read-char p))
      (#t #f)))) ; end-of-file reached

(define (iport-read-string p str start n)
  (assert (fx>=? start 0))
  (assert (fx>=? n 0))
  (assert (fx<=? (fx+ start n) (string-length str)))
  (let ((i 0)
        (eof #f))
    (until (or (fx>=? i n) eof)
      (let ((ch (iport-read-char p)))
        (if (char? ch)
          (begin
            (string-set! str (fx+ i start) ch)
            (set! i (fx1+ i)))
          (set! eof #t))))
    i))

(define (iport-position p)
  (cons (iport-x p) (iport-y p)))

(define (iport-position-set! p pos)
  (unless (and (pair? pos) (fixnum? (car pos)) (fixnum? (cdr pos)))
    (raise (make-i/o-invalid-position-error pos)))
  (let ((y (cdr pos))
        (lines (iport-lines p)))
    (assert (fx>=? y 0))
    (assert (fx<? y (charlines-length lines)))
    (let ((x (car pos))
          (line (charlines-ref lines y)))
      (assert (fx>=? x 0))
      (assert (fx<=? x (charline-length line)))
      (iport-x-set! p x)
      (iport-y-set! p y))))

; create an input port wrapping a charlines
(define open-charlines-input-port
  (let ((empty-lines (charlines (charline))))
    (lambda (lines)
      (charlines-iterate lines
        (lambda (i elem)
          (assert (charline? elem))))
      (let ((p (make-iport 0 0 (if (charlines-empty? lines) empty-lines lines))))
        (make-custom-textual-input-port
          "charlines-input-port"
          (lambda (str start n) (iport-read-string p str start n))
          (lambda ()            (iport-position     p))
          (lambda (pos)         (iport-position-set! p pos))
          #f))))) ; nothing to do on (close-input-port)

) ; close library