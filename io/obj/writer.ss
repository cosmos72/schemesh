;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file io/obj/obj.ss

(define-record-type obj-writer
  (fields
    put-proc
    close-box            ; box containing #f or procedure
    (mutable result)     ; returned by (close-proc tx)
    (mutable eof?))      ; boolean
  (protocol
    (lambda (new)
      (lambda (put-proc close-proc)
        (%make-obj-writer new put-proc close-proc))))
  (nongenerative %obj-writer-7c46d04b-34f4-4046-b5c7-b63753c1be39))


;; called internally by make-obj-writer: create and return a obj-writer
(define (%make-obj-writer new put-proc close-proc)
  (assert* 'make-obj-writer (procedure? put-proc))
  (assert* 'make-obj-writer (logbit? 2 (procedure-arity-mask put-proc)))
  (when close-proc
    (assert* 'make-obj-writer (procedure? close-proc))
    (assert* 'make-obj-writer (logbit? 1 (procedure-arity-mask close-proc))))
  (new put-proc  (box close-proc) (void) #f))


;; call (put-proc tx obj) to write one more value.
;; each call will return the value returned by (put-proc tx),
;; or raise a condition in case of I/O errors or after (obj-writer-close tx) has been called.
(define (obj-writer-put tx obj)
  (assert* 'obj-writer-put (obj-writer? tx))
  (if (obj-writer-eof? tx)
    (raise-errorf 'obj-writer-put "not permitted on closed obj-writer ~s" tx)
    ((obj-writer-put-proc tx) tx obj)))


;; call (close-proc tx) to release any resource held by the obj-writer.
;; return the value returned by (close-proc tx), which is expected to be the value accumulated into the obj-writer.
;;
;; further calls to (obj-writer-close tx) on the same tx have no effect:
;; they do not call (close-proc tx) again, and always return the same value as the first call.
(define (obj-writer-close tx)
  (obj-writer-eof?-set! tx #t)
  (assert* 'obj-writer-close (obj-writer? tx))
  (let* ((close-box  (obj-writer-close-box tx))
         (close-proc (unbox close-box))
         (consumed?  (and close-proc (box-cas! close-box close-proc #f))))
    (when consumed?
      (obj-writer-result-set! tx (close-proc tx))))
  (obj-writer-result tx))


;; create and return a obj-writer that does nothing
;; each call to (obj-writer-put tx obj) will discard obj,
;; or raise a condition after (obj-writer-close tx) has been called.
(define (discard-writer const)
  (let ((%discard-writer-put ;; name shown when displaying the closure
          (lambda (tx obj) (void))))
    (make-obj-writer %discard-writer-put #f)))


;; create and return a obj-writer that is already closed and cannot be written to:
;; each call to (obj-writer-put tx obj) will raise a condition.
(define (full-writer const)
  (let* ((%full-writer-put ;; name shown when displaying the closure
           (lambda (tx obj) (void)))
         (tx (make-obj-writer %full-writer-put #f)))
    (obj-writer-close tx)
    tx))


;; create and return a obj-writer accumulates values into an internal list.
;; each call to (obj-writer-put tx obj) will add an element to such list,
;; or raise a condition after (obj-writer-close tx) has been called.
;;
;; (obj-writer-close tx) will return the accumulated list, which can be modified by the caller.
(define (list-writer)
  (let* ((l '())
         (%list-writer-put ;; name shown when displaying the closure
           (lambda (tx obj)
             (set! l (cons obj l))))
         (%list-writer-close ;; name shown when displaying the closure
           (lambda (tx)
             (set! l (reverse! l))
             l)))
    (make-obj-writer %list-writer-put %list-writer-close)))
