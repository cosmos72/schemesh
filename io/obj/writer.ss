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
    (mutable result))    ; returned by (close-proc tx)
  (protocol
    (lambda (new)
      (lambda (put-proc close-proc)
        (%make-obj-writer new put-proc close-proc))))
  (nongenerative %obj-writer-7c46d04b-34f4-4046-b5c7-b63753c1be40))


;; called internally by make-obj-writer: create and return an obj-writer
(define (%make-obj-writer new put-proc close-proc)
  (assert* 'make-obj-writer (procedure? put-proc))
  (assert* 'make-obj-writer (logbit? 2 (procedure-arity-mask put-proc)))
  (when close-proc
    (assert* 'make-obj-writer (procedure? close-proc))
    (assert* 'make-obj-writer (logbit? 1 (procedure-arity-mask close-proc))))
  (new put-proc (box (or close-proc void1)) (void)))


;; return #t if obj-writer was closed,
;; otherwise return #f
;;
;; Calling (obj-writer-put tx obj) on a closed obj-writer always raises a condition.
(define (obj-writer-eof? tx)
  (assert* 'obj-writer-put (obj-writer? tx))
  (not (unbox (obj-writer-close-box tx))))


;; Put one more value into an obj-writer.
;;   Internally calls (put-proc tx obj).
;;   Each call will return the value returned by (put-proc tx) - which is usually unspecified -
;;   or raise a condition in case of I/O errors.
;;
;; Calling (obj-writer-put tx obj) on a closed obj-writer always raises a condition.
(define (obj-writer-put tx obj)
  (assert* 'obj-writer-put (obj-writer? tx))
  (if (obj-writer-eof? tx)
    (raise-errorf 'obj-writer-put "not permitted on closed obj-writer ~s" tx)
    ((obj-writer-put-proc tx) tx obj)))


;; Close an obj-writer or subtype.
;; Calls (close-proc tx) to release any resource owned by the obj-writer.
;; Returns the value returned by (close-proc tx), which is expected to be
;; the value accumulated into the obj-writer, if any.
;;
;; Multiple calls to (obj-writer-close rx) on the same obj-writer are allowed:
;; they are equivalent to a single call, and always return the same value as the first call.
;;
;; Note: if obj-writer or subtype constructor accepts as argument some EXISTING resource(s),
;;   for example an input port, then the obj-writer does not OWN the resource(s) - it merely borrows them -
;;   and closing the obj-writer must NOT close those resource(s).
;;
;; If instead obj-writer or subtype creates some resource(s) internally,
;; or is specifically instructed to take ownership of some existing resource(s)
;; (for example by passing a truish value for an optional constructor argument close?),
;; then it OWNS such resource(s) and closing the obj-writer MUST close them.
;;
;; Implementation note: calls the close-proc stored in obj-writer at its creation,
;;   to release any resource OWNED by the obj-writer.
;;   close-proc is guaranteed to be called at most once per obj-writer,
;;   even if (obj-writer-close rx) is called concurrently on the same object from multiple threads.
(define (obj-writer-close tx)
  (assert* 'obj-writer-close (obj-writer? tx))
  (let* ((close-box  (obj-writer-close-box tx))
         (close-proc (unbox close-box)))
    (when (and close-proc (box-cas! close-box close-proc #f))
      (obj-writer-result-set! tx (close-proc tx))))
  (obj-writer-result tx))


;; create and return an obj-writer that does nothing
;; each call to (obj-writer-put tx obj) will discard obj,
;; or raise a condition after (obj-writer-close tx) has been called.
(define (discard-writer)
  (let ((%discard-writer-put ;; name shown when displaying the closure
          (lambda (tx obj) (void))))
    (make-obj-writer %discard-writer-put #f)))


;; create and return an obj-writer that is already closed and cannot be written to:
;; each call to (obj-writer-put tx obj) will raise a condition.
(define (full-writer)
  (let* ((%full-writer-put ;; name shown when displaying the closure
           (lambda (tx obj) (void)))
         (tx (make-obj-writer %full-writer-put #f)))
    (obj-writer-close tx)
    tx))


;; create and return an obj-writer accumulates values into an internal list.
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
             (reverse! l))))
    (make-obj-writer %list-writer-put %list-writer-close)))


;; create and return an obj-writer accumulates values into an internal vector.
;; each call to (obj-writer-put tx obj) will add an element to such vector,
;; or raise a condition after (obj-writer-close tx) has been called.
;;
;; (obj-writer-close tx) will return the accumulated vector, which can be modified by the caller.
(define (vector-writer)
  (let* ((l '())
         (%vector-writer-put ;; name shown when displaying the closure
           (lambda (tx obj)
             (set! l (cons obj l))))
         (%vector-writer-close ;; name shown when displaying the closure
           (lambda (tx)
             (list-reverse->vector l))))
    (make-obj-writer %vector-writer-put %vector-writer-close)))
