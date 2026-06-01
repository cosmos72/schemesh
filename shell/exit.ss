;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file shell/job.ss

(define child-wire-status-shm
  (begin
    (s-fd-allocate) ; mark highest fd as reserved: used by tty_fd
    (let ((shm (wire-shm-open (s-fd->int (s-fd-allocate)))))
      (and (wire-shm? shm) shm))))


(define (posix-exit-is-exact? status)
  ;; POSIX exit() is limited to 8-bit value 0 ... 255
  (or (eq? (void) status) ;; will exit(0)
      (and (eq? 'failed (status->kind status))
           (let ((n (status->value status)))
             (and (fixnum? n) (fx<=? n 1 255)))))) ;; will exit(1 ... 255)


;; serialize status to shared memory with (wire-shm-insert! ... (datum->wire status))
;; then terminate process by calling (posix-exit status)
(define sh-exit
  (let ((c-pid-get (foreign-procedure "c_pid_get" () int)))
    (lambda (status)
      ;; (debugf "sh-exit ~s" status)
      (dynamic-wind
        void
        (lambda ()
          (unless (posix-exit-is-exact? status)
            (wire-shm-insert! child-wire-status-shm (c-pid-get) (datum->wire status))))
        (lambda ()
          (posix-exit status))))))


;; hashtable containing collected pid -> status
;; where status was saved to shared memory by child process with (sh-exit) above
;; and retrieved by main shell process via (wire-shm-delete!)
(define child-wire-status-table (make-eqv-hashtable))

(define child-wire-status-mutex (box #f))


;; receive from shared memory and deserialize all available entries pid -> status
;; must be called with child-wire-status-mutex already locked
(define (child-wire-status-locked-collect shm ht)
  (let-values (((key value) (wire-shm-delete! shm)))
    ;; (debugf "child-wire-status-locked-collect ~s ~s ~s" shm key value)
    (when (and key (bytevector? value))
      (let-values (((status len) (wire->datum value)))
        (when (and (status? status) (eqv? len (bytevector-length value)))
          (hashtable-set! ht key status)))
      (child-wire-status-locked-collect shm ht))))


;; consume and return deserialized wire status for specified pid.
;; return #f if not found
(define (child-wire-status-consume pid)
  ;; NOT reentrant, and often called from interrupts
  (and
    (box-cas-strong! child-wire-status-mutex #f #t)
    (begin (memory-order-acquire) #t)
    (dynamic-wind
      disable-interrupts
      (lambda ()
        (let ((ht child-wire-status-table))
          (child-wire-status-locked-collect child-wire-status-shm ht)
          (let ((status (hashtable-ref ht pid #f)))
            (when status
              (hashtable-delete! ht pid))
            status)))
      (lambda ()
        (enable-interrupts)
        (memory-order-release)
        (box-cas-strong! child-wire-status-mutex #t #f)))))
  