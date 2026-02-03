;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file repl/repl.ss


;; return span containing all recent values produced by code evaluated at REPL,
;; or n-th recent value produced by code evaluated at REPL,
;; or (void) if n is out of range.
(define repl-answers
  (let ((ans (make-span 0)))
    (case-lambda
      (()
        ans)
      ((n)
        (let ((len (span-length ans)))
          (cond
            ((not (fixnum? n))
              (void))
            ((fx<? -1 n len)
              (span-ref ans n))
            ((fx<? -1 (- n) len)
              (span-ref ans (fx+ len n)))
            (else
              (void))))))))


;; set or retrieve maximum length of (repl-answers)
(define repl-answers-max-length
  (let ((max-len 128))
    (case-lambda
      (()
        max-len)
      ((new-max-len)
        (assert* 'repl-answers-max-length (fixnum? new-max-len))
        (assert* 'repl-answers-max-length (fx>=? new-max-len 0))
        (set! max-len new-max-len)))))


;; append obj to (repl-answers)
(define (repl-answers-append! obj)
  (let* ((ans (repl-answers))
         (len (span-length ans))
         (max-len (repl-answers-max-length))
         (delta (fx- len max-len)))
    (when (fx>? delta 0)
      (span-fill! ans 0 delta (void)) ; helps GC
      (span-delete-left! ans delta))
    (span-insert-right! ans obj)))


;; clear (repl-answers)
(define (repl-answers-clear!)
  (span-clear! (repl-answers)))


;; display (repl-answers) to port,
;; which defaults to (current-output-port)
(define repl-answers-display
  (case-lambda
    ((port)
      (let ((ans (repl-answers)))
        (do ((i 0 (fx1+ i))
             (n (span-length ans)))
            ((fx>=? i n))
          (write      i port)
          (write-char #\tab port)
          (write      (span-ref ans i) port)
          (newline    port))))
    (()
      (repl-answers-display (current-output-port)))))
