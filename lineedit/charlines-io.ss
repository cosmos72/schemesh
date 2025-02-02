;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh lineedit charlines io (0 7 2))
  (export open-charline-input-port open-charlines-input-port)
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme) fx1+)
    (schemesh bootstrap)      ; until
    (schemesh containers charline)
    (schemesh containers charlines))

; helper for input port wrapping a charline
(define-record-type icport
  (fields
    (immutable line)  ; charline to read from
    (mutable   pos))  ; position in charline
  (nongenerative #{icport do8t0druatc9fhaize8s4a1wd-20}))

(define (icport-read-string p str start n)
  (assert* 'icport-read-string (fx>=? start 0))
  (assert* 'icport-read-string (fx>=? n 0))
  (assert* 'icport-read-string (fx<=? (fx+ start n) (string-length str)))
  (let* ((line     (icport-line p))
         (line-pos (icport-pos p))
         (line-len (charline-length line))
         (ret-n    (fxmin n (fx- line-len line-pos))))
    (when (fx>? ret-n 0)
      (do ((i 0 (fx1+ i)))
          ((fx>=? i ret-n)
            (icport-pos-set! p (fx+ ret-n line-pos)))
        (string-set! str (fx+ i start) (charline-ref line (fx+ i line-pos)))))
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
  (assert* 'open-charline-input-port (charline? cgb))
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
      ((fx<? (fx1+ y) (charlines-length lines))
        ; end-of-line reached, go to next line
        (iport-x-set! p 0)
        (iport-y-set! p (fx1+ y))
        (iport-read-char p))
      (#t #f)))) ; end-of-file reached

(define (iport-read-string p str start n)
  (assert* 'iport-read-string (fx>=? start 0))
  (assert* 'iport-read-string (fx>=? n 0))
  (assert* 'iport-read-string (fx<=? (fx+ start n) (string-length str)))
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
    (assert* 'iport-position-set! (fx<? -1 y (charlines-length lines)))
    (let ((x (car pos))
          (line (charlines-ref lines y)))
      (assert* 'iport-position-set! (fx<=? 0 x (charline-length line)))
      (iport-x-set! p x)
      (iport-y-set! p y))))

; create an input port wrapping a charlines
(define open-charlines-input-port
  (let ((empty-lines (charlines (charline))))
    (lambda (lines)
      (charlines-iterate lines
        (lambda (i elem)
          (assert* 'open-charlines-input-port (charline? elem))))
      (let ((p (make-iport 0 0 (if (charlines-empty? lines) empty-lines lines))))
        (make-custom-textual-input-port
          "charlines-input-port"
          (lambda (str start n) (iport-read-string p str start n))
          (lambda ()            (iport-position     p))
          (lambda (pos)         (iport-position-set! p pos))
          #f))))) ; nothing to do on (close-input-port)

) ; close library
