;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;; if this file is loaded multiple times, only the first one has any effect.
;; implementation note:
;; this is done by setting the top-level symbol sh-persistent-parameters
;; only if it's not bound yet, and by retrieving its value if it's bound.

(library (schemesh bootstrap parameters)
  (export sh-make-parameter sh-make-thread-parameter)
  (import
    (rnrs)
    (only (chezscheme)
                       interaction-environment top-level-bound? top-level-value)
    (schemesh bootstrap raise))


;; portable reimplementation of Chez Scheme (make-parameter)
(define sh-make-parameter
  (case-lambda
    ((initial-value updater-proc)
      (let ((current-value (updater-proc initial-value)))
        (case-lambda
          (() current-value)
          ((new-value) (set! current-value (updater-proc new-value))))))
    ((initial-value)
      (sh-make-parameter initial-value (lambda (x) x)))))


;; approximate reimplementation of Chez Scheme make-thread-parameter:
;; calls (make-thread-parameter) if available,
;; otherwise calls (sh-make-parameter) above.
(define sh-make-thread-parameter
  (if (top-level-bound? 'make-thread-parameter)
        (top-level-value 'make-thread-parameter)
        sh-make-parameter))



) ; close library
