;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh io (0 1))
  (export open-chargbuffer-input-port open-gbuffer-of-chargbuffers-input-port)
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme) fx1+)
    (schemesh bootstrap) ; until
    (schemesh containers))

; helper for input port wrapping a chargbuffer
(define-record-type icport
  (fields
    (immutable source) ; chargbuffer to read from
    (mutable   pos))   ; position in chargbuffer
  (nongenerative #{icport do8t0druatc9fhaize8s4a1wd-20}))

(define (icport-read-string p str start n)
  (assert (fx>=? start 0))
  (assert (fx>=? n 0))
  (assert (fx<=? (fx+ start n) (string-length str)))
  (let* ((source     (icport-source p))
         (source-pos (icport-pos p))
         (source-len (chargbuffer-length source))
         (ret-n  (fxmin n (fx- source-len source-pos))))
    (if (fx<=? ret-n 0)
      0
      (do ((i 0 (fx1+ i)))
           ((fx>=? i ret-n)
             (icport-pos-set! p (fx+ ret-n source-pos))
             ret-n)
        (string-set! str (fx+ i start) (chargbuffer-ref source (fx+ i source-pos)))))))

(define icport-position icport-pos)

(define (icport-position-set! p pos)
  (unless (and (fixnum? pos)
               (fx>=? pos 0)
               (fx<=? pos (chargbuffer-length (icport-source p))))
    (raise (make-i/o-invalid-position-error pos)))
  (icport-pos-set! p pos))

; create an input port wrapping a chargbuffer
(define (open-chargbuffer-input-port cgb)
  (assert (chargbuffer? cgb))
  (let ((p (make-icport cgb 0)))
    (make-custom-textual-input-port
      "chargbuffer-input-port"
      (lambda (str start n) (icport-read-string p str start n))
      (lambda ()            (icport-position     p))
      (lambda (pos)         (icport-position-set! p pos))
      #f))) ; nothing to do on (close-input-port)


; helper for input port wrapping a gbuffer of chargbuffers
(define-record-type iport
  (fields
    (mutable   x)        ; position in y-th chargbuffer
    (mutable   y)        ; position in gbuffer
    (immutable sources)) ; gbuffer of chargbuffers to read from
  (nongenerative #{iport cy8auoivds3jpsu99eergcoco-20}))

(define (iport-read-char p)
  (let* ((x (iport-x p))
         (y (iport-y p))
         (sources (iport-sources p))
         (source  (gbuffer-ref sources y)))
    (cond
      ((fx<? x (chargbuffer-length source))
        (iport-x-set! p (fx1+ x))
        (chargbuffer-ref source x))
      ((fx<? (fx1+ y) (gbuffer-length sources))
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
        (sources (iport-sources p)))
    (assert (fx>=? y 0))
    (assert (fx<? y (gbuffer-length sources)))
    (let ((x (car pos))
          (source (gbuffer-ref sources y)))
      (assert (fx>=? x 0))
      (assert (fx<=? x (chargbuffer-length source)))
      (iport-x-set! p x)
      (iport-y-set! p y))))

; create an input port wrapping a gbuffer containing chargbuffers
(define open-gbuffer-of-chargbuffers-input-port
  (let ((empty-gb (gbuffer (chargbuffer))))
    (lambda (gb)
      (gbuffer-iterate gb
        (lambda (i elem)
          (assert (chargbuffer? elem))))
      (let ((p (make-iport 0 0 (if (gbuffer-empty? gb) empty-gb gb))))
        (make-custom-textual-input-port
          "gbuffer-input-port"
          (lambda (str start n) (iport-read-string p str start n))
          (lambda ()            (iport-position     p))
          (lambda (pos)         (iport-position-set! p pos))
          #f))))) ; nothing to do on (close-input-port)

) ; close library
