;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.



;; UTF-8b text input and/or output port reading from/writing to a file descriptor returned by a closure.
(define-record-type tport
  (fields
    proc   ; fd-proc
    bspan) ; bytespan buffer
  (nongenerative #{tport ncfagw2gtqduasri71nnqdxvw-0}))


(define (tport-read p str start n)
  (assert* 'fd-redir-textual-input-port-read (fx>=? start 0))
  (assert* 'fd-redir-textual-input-port-read (fx>=? n 0))
  (assert* 'fd-redir-textual-input-port-read (fx<=? (fx+ start n) (string-length str)))
  ;; TODO implement
  0)


(define (tport-write p str start n)
  (assert* 'fd-redir-textual-output-port-write (fx>=? start 0))
  (assert* 'fd-redir-textual-output-port-write (fx>=? n 0))
  (assert* 'fd-redir-textual-output-port-write (fx<=? (fx+ start n) (string-length str)))
  (if (fxzero? n)
    0
    (let ((bsp (tport-bspan p)))
      (bytespan-insert-right/string! bsp str start (fx+ start n))
      (do ()
          ((bytespan-empty? bsp) n)
        (let ((sent (fd-write ((tport-proc p)) (bytespan-peek-data bsp) (bytespan-peek-beg bsp) (bytespan-peek-end bsp))))
          (assert* 'fd-redir-textual-output-port-write (fx>=? sent 0))
          (bytespan-erase-left! bsp sent))))))


;; create and return an UTF-8b text input port that redirectably reads from a file descriptor.
;;
;; fd-proc must be a no-argument procedure that returns an integer file descriptor;
;; the returned file descriptor *may* change from one call to the next.
(define (open-fd-redir-textual-input-port name fd-proc)
  (assert* 'open-fd-redir-textual-input-port (procedure? fd-proc))
  (let* ((bsp (make-bytespan 0))
         (port (make-tport fd-proc bsp)))
    (bytespan-reserve-right! bsp 8192)
    (make-custom-textual-input-port
      name
      (lambda (bv start n) (tport-read port bv start n))
      #f    ; no pos
      #f    ; no pos-set!
      #f))) ; do nothing on close


;; create and return a binary input/output port that redirectably reads from/writes to a file descriptor.
;;
;; fd-in-proc and fd-out-proc must be no-argument procedures that return an integer file descriptor;
;; the returned file descriptor *may* change from one call to the next.
(define open-fd-redir-textual-input/output-port
  (case-lambda
    ((name fd-in-proc fd-out-proc)
      (assert* 'open-fd-redir-textual-input-port (procedure? fd-in-proc))
      (assert* 'open-fd-redir-textual-input-port (procedure? fd-out-proc))
      (let* ((bsp1 (make-bytespan 0))
             (bsp2 (make-bytespan 0))
             (port1 (make-tport fd-in-proc bsp1))
             (port2 (make-tport fd-out-proc bsp2)))
        (bytespan-reserve-right! bsp1 8192)
        (bytespan-reserve-right! bsp2 8192)
        (make-custom-textual-input/output-port
          name
          (lambda (bv start n) (tport-read port1 bv start n))
          (lambda (bv start n) (tport-write port2 bv start n))
          #f    ; no pos: there is no "single" position, in/out file descriptors may differ and may be non-seekable
          #f    ; no pos-set!
          #f))) ; do nothing on close
    ((name fd-proc)
      (open-fd-redir-textual-input/output-port name fd-proc fd-proc))))


;; create and return a binary output port that redirectably writes to a file descriptor.
;;
;; fd-proc must be a no-argument procedure that returns an integer file descriptor;
;; the returned file descriptor *may* change from one call to the next.
(define (open-fd-redir-textual-output-port name fd-proc)
  (assert* 'open-fd-redir-textual-output-port (procedure? fd-proc))
  (let* ((bsp (make-bytespan 0))
         (port (make-tport fd-proc bsp)))
    (bytespan-reserve-right! bsp 8192)
    (make-custom-textual-output-port
      name
      (lambda (bv start n) (tport-write port bv start n))
      #f    ; no pos
      #f    ; no pos-set!
      #f))) ; do nothing on close
