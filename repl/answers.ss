;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh repl answers (0 8 3))
  (export repl-answers-display repl-answers repl-answers-append! repl-answers-clear! repl-answers-max-length)
  (import
    (rnrs)
    (only (chezscheme) fx1+ void)
    (only (schemesh bootstrap) assert*)
    (schemesh containers span))


;; return span containing all recent values produced by code evaluated at REPL,
;; or n-th recent value produced by code evaluated at REPL,
;; or (void) if n is out of range.
(define repl-answers
  (let ((h (make-span 0)))
    (case-lambda
      (()
        h)
      ((n)
        (if (fx<? -1 n (span-length h))
          (span-ref h n)
          (void))))))


;; set or retrieve maximum length of (repl-answers)
(define repl-answers-max-length
  (let ((max-len 1000))
    (case-lambda
      (()
        max-len)
      ((new-max-len)
        (assert* 'repl-answers-max-length (fixnum? new-max-len))
        (assert* 'repl-answers-max-length (fx>=? new-max-len 0))
        (set! max-len new-max-len)))))


;; append obj to (repl-answers)
(define (repl-answers-append! obj)
  (let ((h (repl-answers))
        (max-len (repl-answers-max-length)))
    (when (fx>? (span-length h) max-len)
      (span-erase-left! (fx- (span-length h) max-len)))
    (span-insert-right! h obj)))


;; clear (repl-answers)
(define (repl-answers-clear!)
  (span-clear! (repl-answers)))


;; display (repl-answers) to port,
;; which defaults to (current-output-port)
(define repl-answers-display
  (case-lambda
    ((port)
      (let ((h (repl-answers)))
        (do ((i 0 (fx1+ i))
             (n (span-length h)))
            ((fx>=? i n))
          (write      i port)
          (write-char #\tab port)
          (write      (span-ref h i) port)
          (newline    port))))
    (()
      (repl-answers-display (current-output-port)))))

) ; close library
