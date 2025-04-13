;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh repl history (0 8 3))
  (export display-history history history-append! history-clear! history-ref)
  (import
    (rnrs)
    (only (chezscheme) fx1+ void)
    (schemesh containers span))


;; return span of recent values produced by code evaluated at REPL.
(define history
  (let ((h (make-span 0)))
    (lambda () h)))


;; append obj to recent values produced by code evaluated at REPL.
(define (history-append! obj)
  (let ((h (history)))
    (when (fx>? (span-length h) 1000)
      (span-erase-left! (fx- (span-length h) 1000)))
    (span-insert-right! h obj)))


;; clear recent values produced by code evaluated at REPL.
(define (history-clear!)
  (span-clear! (history)))


;; return n-th recent value produced by code evaluated at REPL,
;; or (void) if n is out of range.
(define (history-ref n)
  (let ((h (history)))
    (if (fx<? -1 n (span-length h))
      (span-ref h n)
      (void))))


;; display recent values produced by code evaluated at REPL to port,
;; which defaults to (current-output-port)
(define display-history
  (case-lambda
    ((port)
      (let ((h (history)))
        (do ((i 0 (fx1+ i))
             (n (span-length h)))
            ((fx>=? i n))
          (write      i port)
          (write-char #\tab port)
          (write      (span-ref h i) port)
          (newline    port))))
    (()
      (display-history (current-output-port)))))

) ; close library
