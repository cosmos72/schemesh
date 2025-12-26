;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k posix signal (0 9 2))
  (export countdown make-duration
          &received-signal make-received-signal raise-condition-received-signal
          received-signal? received-signal-name

          signal-raise signal-setdefault signal-number->name signal-name->number
          signal-name-is-usually-fatal?
          signal-consume-sigwinch signal-init-sigwinch signal-restore-sigwinch)
  (import
    (rnrs)
    (only (chezscheme) assertion-violationf foreign-procedure format fx1- integer-length logbit?
                       make-continuation-condition make-format-condition make-time
                       procedure-arity-mask time? time-nanosecond time-second time-type void)
    (only (scheme2k bootstrap)            assert* check-interrupts debugf raise-assertf  with-locked-objects)
    (only (scheme2k containers hashtable) alist->eq-hashtable hashtable-transpose))


(define c-errno-einval ((foreign-procedure "c_errno_einval" () int)))

;; condition that wraps a C signal name represented as a symbol
(define-condition-type &received-signal &condition make-received-signal received-signal?
  (name received-signal-name))


;; raise a condition object that recognizably contains signal-name
(define (raise-condition-received-signal who signal-name message . message-args)
  (call/cc
    (lambda (k)
      (raise
        (condition
          (make-error)
          (make-continuation-condition k)
          (make-non-continuable-violation)
          (make-received-signal signal-name)
          (make-who-condition (if (symbol? who) who (format #f "~s" who)))
          (make-format-condition)
          (make-message-condition (or message "received signal: ~s"))
          (make-irritants-condition (if message message-args (list signal-name))))))))


;; create and return time object with type 'time-duration.
;;
;; two-arguments version accepts seconds and nanoseconds where both are exact integers
;;
;; one-argument version accepts:
;;  either an exact or inexact real, indicating the number of seconds
;;  or a time object with type 'time-duration, which is returned verbatim
;;
;; Added in scheme2k 0.9.3
(define make-duration
  (case-lambda
    ((seconds nanoseconds)
      (make-time 'time-duration nanoseconds seconds))
    ((duration)
      (cond
        ((real? duration)
          (let* ((seconds (exact (floor duration)))
                 (ns      (exact (round (* 1e9 (- duration seconds))))))
            (make-time 'time-duration ns seconds)))
        ((and (time? duration) (eq? 'time-duration (time-type duration)))
          duration)
        (else
          (raise-assertf 'make-duration "(or (real? duration) (eq? 'time-duration (time-type duration)))" duration))))))


;; Pause for user-specified duration.
;;
;; The duration to pause include only the interval the caller's process or job is running:
;; if suspended with CTRL+Z or SIGTSTP, the suspended interval is not counted.
;;
;; This effectively works as a countdown from NUMBER seconds to zero,
;; that can be suspended with CTRL+Z or SIGTSTP and resumed by continuing this program.
;;
;; duration must be one of:
;; * an exact or inexact real, indicating the number of seconds (non-integer values are supported too)
;; * a pair (seconds . nanoseconds) where both are exact integers
;; * a time object with type 'time-duration
;;
;; returns 0 on success, or < 0 on errors.
(define countdown
  (let ((c-countdown (foreign-procedure __collect_safe "c_countdown" (ptr) int)))
    (lambda (duration)
      (let ((pair (%duration->pair duration)))
        (with-locked-objects (pair)
          (let %countdown ((pair pair))
            (check-interrupts)
            (let ((ret (c-countdown pair)))
              (if (eqv? 1 ret)
                (%countdown pair)
                ret))))))))

;; convert one of:
;; * an exact or inexact real, indicating the number of seconds
;; * a pair (seconds . nanoseconds) where both are exact integers
;; * a time object with type 'time-duration
;;
;; to a mutable pair (seconds_int64 . nanoseconds_int32)
(define (%duration->pair duration)
  (cond
    ((real? duration)
      (let* ((seconds (exact (floor duration)))
             (ns      (exact (round (* 1e9 (- duration seconds))))))
        (%make-c-duration seconds ns)))
    ((pair? duration)
      (%make-c-duration (car duration) (cdr duration)))
    ((and (time? duration) (eq? 'time-duration (time-type duration)))
      (%make-c-duration (time-second duration) (time-nanosecond duration)))
    (else
      #f)))


;; if seconds and nanoseconds are exact integers suitable for C nanosleep(), return a pair (seconds . nanoseconds)
;; otherwise return #f
(define (%make-c-duration s ns)
  (cond
    ((not (and (integer? s)  (exact? s)
               (integer? ns) (exact? ns)))
      ;; (debugf "make-c-duration not exact integer: s=~s ns=~s" s ns)
      #f)
    ((and (>= s 0) (<= (integer-length s) 63) (<= 0 ns 999999999))
      ;; (debugf "make-c-duration ok: s=~s ns=~s" s ns)
      (cons s ns))
    (else
      ;; normalize nanoseconds to the range [0, 1e9) and try again
      (let-values (((carry ns) (div-and-mod ns 1000000000)))
        (let ((s (+ s carry)))
          ;; (debugf "make-c-duration normalized: s=~s ns=~s" s ns)
          (if (and (>= s 0) (<= (integer-length s) 63) (<= 0 ns 999999999))
            (cons s ns)
            #f))))))



(define signal-table-number->name
  (alist->eq-hashtable ((foreign-procedure "c_signals_list" () ptr))))

(define signal-table-name->number
  (hashtable-transpose signal-table-number->name (make-eqv-hashtable)))

;; convert a signal number (must be a fixnum) to signal name.
;; return #f if signal number was not found.
(define (signal-number->name number)
  (hashtable-ref signal-table-number->name number #f))

;; convert a signal name (must be a symbol) to signal number.
;; return #f if signal name was not found.
(define (signal-name->number name)
  (hashtable-ref signal-table-name->number name #f))

(define (signal-name-is-usually-fatal? name)
  (if (memq name '(sigabrt sigbus sigfpe #|sighup|# sigill sigint sigkill #|sigpipe|#
                   sigquit sigsegv sigterm #|sigstkflt|# sigxcpu #|sigxfsz|#))
    #t
    #f))

;; (signal-raise signal-name) calls C functions sigaction(sig, SIG_DFL),
;; then calls C function pthread_kill(pthread_self(), sig)
;; i.e. sends specified signal to the thread itself.
;;
;; Returns 0 if successful, or < 0 if signal-name is unknown, or if C function raise() fails with C errno != 0.
(define signal-raise
  (let ((c-signal-raise (foreign-procedure "c_signal_send_thread_self" (int int) int)))
    (lambda (signal-name)
      (let ((signal-number (signal-name->number signal-name)))
        (if (fixnum? signal-number)
          (c-signal-raise signal-number 1) ; 1 = unset signal handler
          c-errno-einval)))))


;; (signal-setdefault signal-name) calls C functions sigaction(sig, SIG_DFL)
;; i.e. restores default handler for specified signal.
;;
;; Returns < 0 if signal-name is unknown, or if C function sigaction() fails with C errno != 0.
(define signal-setdefault
  (let ((c-signal-setdefault (foreign-procedure "c_signal_setdefault" (int) int)))
    (lambda (signal-name)
      (let ((signal-number (signal-name->number signal-name)))
        (if (fixnum? signal-number)
          (c-signal-setdefault signal-number)
          c-errno-einval)))))


(define signal-consume-sigwinch (foreign-procedure "c_signal_consume_sigwinch" () ptr))

(define signal-init-sigwinch
  (let ((c-signal-init-sigwinch (foreign-procedure "c_signal_init_sigwinch" () int)))
    (lambda ()
      (assert* 'signal-init-sigwinch (fxzero? (c-signal-init-sigwinch))))))

(define signal-restore-sigwinch (foreign-procedure "c_signal_restore_sigwinch" () int))

) ; close library
