;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh posix signal (0 8 1))
  (export signal-raise signal-number->name signal-name->number
          signal-consume-sigchld signal-consume-sigtstp signal-consume-sigwinch
          signal-init-sigwinch signal-restore-sigwinch)
  (import
    (rnrs)
    (only (chezscheme) assertion-violationf foreign-procedure logbit?
                       procedure-arity-mask void)
    (only (schemesh bootstrap)            assert* sh-make-thread-parameter)
    (only (schemesh containers hashtable) eq-hashtable hashtable-transpose))

(define signal-table-number->name
  (apply eq-hashtable ((foreign-procedure "c_signals_list" () ptr))))

(define signal-table-name->number
  (hashtable-transpose signal-table-number->name (make-eq-hashtable)))

(define (signal-number->name number)
  (hashtable-ref signal-table-number->name number #f))

;; convert a signal name (must be a symbol) to signal number.
;; return #f if signal name was not found.
(define (signal-name->number name)
  (hashtable-ref signal-table-name->number name #f))

; (signal-raise signal-name) calls C functions sigaction(sig, SIG_DFL),
; then calls C function raise(sig)
; i.e. sends specified signal to the thread itself.
;
; Returns < 0 if signal-name is unknown, or if C function raise() fails with C errno != 0.
(define signal-raise
  (let ((c-signal-raise (foreign-procedure "c_signal_raise" (int) int))
        (c-errno-einval ((foreign-procedure "c_errno_einval" () int))))
    (lambda (signal-name)
      (let ((signal-number (signal-name->number signal-name)))
        (if (fixnum? signal-number)
          (c-signal-raise signal-number)
          c-errno-einval)))))

(define signal-consume-sigchld   (foreign-procedure "c_sigchld_consume" () ptr))
(define signal-consume-sigtstp   (foreign-procedure "c_sigtstp_consume" () ptr))
(define signal-consume-sigwinch  (foreign-procedure "c_sigwinch_consume" () ptr))

(define signal-init-sigwinch
  (let ((c-signal-init-sigwinch (foreign-procedure "c_sigwinch_init" () int)))
    (lambda ()
      (assert* 'signal-init-sigwinch (fxzero? (c-signal-init-sigwinch))))))

(define signal-restore-sigwinch (foreign-procedure "c_sigwinch_restore" () int))

) ; close library
