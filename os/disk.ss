;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.


;;; collect information about mounted file systems

(define-record-type (disk-reader %make-disk-reader disk-reader?)
  (parent reader)
  (fields
    (mutable handle)     ; #f or integer containing C handle
    bvec)                ; bytevector, used as buffer for C function c_disk_get()
  (protocol
    (lambda (args->new)
      (lambda (handle)
        ((args->new %disk-reader-get %disk-reader-skip %disk-reader-close)
          handle (make-bytevector (fx* 10 8))))))
  (nongenerative %disk-reader-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define make-disk-reader
  (let ((c-disk-open (foreign-procedure "c_disk_open" () ptr)))
    (lambda ()
      (let ((obj (c-disk-open)))
        (when (and (integer? obj) (exact? obj) (< obj 0))
          (raise-c-errno 'make-disk-reader 'c_disk_open obj))
        (%make-disk-reader obj)))))


;; called by (reader-get)
(define %disk-reader-get
  (let ((c-disk-get (foreign-procedure "c_disk_get" (ptr ptr) ptr)))
    (lambda (rx)
      (let ((handle (disk-reader-handle rx)))
        (if handle
          (let* ((bvec (disk-reader-bvec rx))
                 (ret (c-disk-get handle bvec)))
            (cond
              ((pair? ret)
                (values (c->disk-entry rx ret bvec) #t))
              ((not ret)
                (%disk-reader-get rx)) ;; error parsing /proc/self/mountinfo, skip it
              ((eqv? 0 ret)
                (values #f #f)) ;; disk-reader is exhausted
              (else
                (raise-c-errno 'disk-reader-get 'statvfs ret handle))))
          (values #f #f)))))) ;; disk-reader is closed


;; called by (reader-skip)
(define %disk-reader-skip
  (let ((c-disk-skip (foreign-procedure "c_disk_skip" (ptr) int)))
    (lambda (rx)
      (let ((handle (disk-reader-handle rx)))
        (if handle
          (let ((err (c-disk-skip handle)))
            (unless (and (fixnum? err) (fx>=? err 0))
              (raise-c-errno 'disk-reader-skip 'readdir err handle))
            (fx>? err 0))
          #f))))) ;; disk-reader is closed


;; called by (reader-close)
(define %disk-reader-close
  (let ((c-disk-close (foreign-procedure "c_disk_close" (ptr) void)))
    (lambda (rx)
      (let ((handle (disk-reader-handle rx)))
        (when handle
          (disk-reader-handle-set! rx #f)
          (c-disk-close handle))))))


;; info about a running disk.
;; TODO validate args in (make-disk-entry)
(define-record-type disk-entry
  (fields
    (mutable id)           ; (void) or exact integer
    (mutable device)       ; (void) or string
    (mutable mount-point)  ; (void) or string
    (mutable type)         ; (void) or string, file system type
    (mutable byte-total)   ; (void) or exact integer
    (mutable byte-free)    ; (void) or exact integer
    (mutable byte-avail)   ; (void) or exact integer
    (mutable inode-total)  ; (void) or exact integer
    (mutable inode-free)   ; (void) or exact integer
    (mutable inode-avail)  ; (void) or exact integer
    (mutable block-size)   ; (void) or exact integer
    (mutable dev)          ; (void) or exact integer
    (mutable flags))       ; (void) or exact integer
  (nongenerative %disk-entry-7c46d04b-34f4-4046-b5c7-b63753c1be41))


(define (c->disk-entry rx l bvec)
  (let* ((device      (car l))
         (rest?       (pair? (cdr l)))
         (mount-point (and rest? (cadr l)))
         (type        (and rest? (cddr l))))
    (make-disk-entry
      (bvec-ref/u64 bvec 0)           ; id,          uint64
      (or device      "")             ; device,      string
      (or mount-point "")             ; mount-point, string
      (or type        "")             ; type,        string
      (bvec-ref/u64 bvec (fx* 1 8))   ; byte-total,  uint64
      (bvec-ref/u64 bvec (fx* 2 8))   ; byte-free,   uint64
      (bvec-ref/u64 bvec (fx* 3 8))   ; byte-avail,  uint64
      (bvec-ref/u64 bvec (fx* 4 8))   ; inode-total, uint64
      (bvec-ref/u64 bvec (fx* 5 8))   ; inode-free,  uint64
      (bvec-ref/u64 bvec (fx* 6 8))   ; inode-avail, uint64
      (bvec-ref/u64 bvec (fx* 7 8))   ; block-size,  uint64
      (bvec-ref/u64 bvec (fx* 8 8))   ; dev,         uint64
      (bvec-ref/u64 bvec (fx* 9 8))))); flags,       uint64

(define (deserialize-disk-entry plist)
  plist)
