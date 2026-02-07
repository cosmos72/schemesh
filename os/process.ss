;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.


;;; collect information about system processes
;;;
(library (scheme2k os (0 9 3))
  (export make-process-reader process-reader process-reader? process-reader-get process-reader-eof? process-reader-close process-reader-skip)
  (import
    (rnrs)
    (only (chezscheme)                   foreign-procedure record-writer void)
    (only (scheme2k bootstrap)           assert*)
    (only (scheme2k io obj)              obj-reader obj-reader-get obj-reader-eof? obj-reader-close obj-reader-skip)
    (only (scheme2k posix)               raise-c-errno))


(define-record-type (process-reader %make-process-reader process-reader?)
  (parent obj-reader)
  (fields
    (mutable handle)    ; #f or integer containing C DIR*
    bvec)               ; bytevector, used as buffer for C function c_process_get()
  (protocol
    (lambda (args->new)
      (lambda (handle)
        ((args->new %process-reader-get %process-reader-skip %process-reader-close)
          handle (make-bytevector (fx* 17 8))))))
  (nongenerative %process-reader-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define make-process-reader
  (let ((c-process-open (foreign-procedure "c_process_open" () ptr)))
    (lambda ()
      (let ((obj (c-process-open)))
        (unless (and (integer? obj) (exact? obj) (> obj 0))
          (raise-c-errno 'make-process-reader 'opendir obj "/proc"))
        (%make-process-reader obj)))))


(define (process-reader-eof? rx)
  (assert* 'process-reader-eof? (process-reader? rx))
  (obj-reader-eof? rx))


(define (process-reader-close rx)
  (assert* 'process-reader-close (process-reader? rx))
  (obj-reader-close rx))


(define (process-reader-get rx)
  (assert* 'process-reader-get (process-reader? rx))
  (obj-reader-get rx))


(define (process-reader-skip rx)
  (assert* 'process-reader-skip (process-reader? rx))
  (obj-reader-skip rx))


;; called by (process-reader-get) and (obj-reader-get)
(define %process-reader-get
  (let ((c-process-get (foreign-procedure "c_process_get" (ptr ptr) ptr)))
    (lambda (rx)
      (let ((handle (process-reader-handle rx)))
        (if handle
          (let ((bvec (process-reader-bvec rx)))
            (let ((ret (c-process-get handle bvec)))
              (cond
                ((pair? ret)
                  (values (c->process-entry ret bvec) #t))
                ((eqv? 0 ret)
                  (values #f #f)) ;; process-reader is exhausted
                (else
                  (raise-c-errno 'process-reader-get 'readdir ret handle)))))
          (values #f #f)))))) ;; process-reader is closed


;; called by (process-reader-skip) and (obj-reader-skip)
(define %process-reader-skip
  (let ((c-process-skip (foreign-procedure "c_process_skip" (ptr) int)))
    (lambda (rx)
      (let ((handle (process-reader-handle rx)))
        (if handle
          (let ((err (c-process-skip handle)))
            (unless (and (fixnum? err) (fx>=? err 0))
              (raise-c-errno 'process-reader-skip 'readdir err handle))
            (fx>? err 0))
          #f))))) ;; process-reader is closed


;; called by (process-reader-close) and (obj-reader-close)
(define %process-reader-close
  (let ((c-process-close (foreign-procedure "c_process_close" (ptr) void)))
    (lambda (rx)
      (let ((handle (process-reader-handle rx)))
        (when handle
          (process-reader-handle-set! rx #f)
          (c-process-close handle))))))


(define-record-type process-entry
  (fields
    (mutable pid)           ; (void) or exact integer
    (mutable name)          ; string
    (mutable tty)           ; (void) or string
    (mutable status)        ; (void) or char
    (mutable user)          ; (void) or immutable string
    (mutable group)         ; (void) or immutable string
    (mutable uid)           ; (void) or exact integer
    (mutable gid)           ; (void) or exact integer
    (mutable ppid)          ; (void) or exact integer
    (mutable pgrp)          ; (void) or exact integer
    (mutable sid)           ; (void) or exact integer
    (mutable flags)         ; (void) or exact integer
    (mutable mem-resident)  ; (void) or exact integer
    (mutable mem-virtual)   ; (void) or exact integer
    (mutable start-time)    ; (void) or time-utc
    (mutable user-time)     ; (void) or time-duration
    (mutable sys-time)      ; (void) or time-duration
    (mutable priority)      ; (void) or exact integer
    (mutable nice)          ; (void) or exact integer
    (mutable num-threads)   ; (void) or exact integer
    (mutable min-fault)     ; (void) or exact integer
    (mutable maj-fault))    ; (void) or exact integer
  ; (nongenerative %process-entry-7c46d04b-34f4-4046-b5c7-b63753c1be39)
)

#|
(define (if-uid->username rx uid)
  (if (and (integer? uid) (exact? uid))
    ;; use short-lived cache uid -> username stored in process-reader
    ;; reduces syscall clutter
    (let ((cache (or (process-reader-uid-cache rx)
                     (let ((ht (make-eqv-hashtable)))
                       (process-reader-uid-cache-set! rx ht)
                       ht))))
      (or (hashtable-ref cache uid #f)
          (let* ((xname (uid->username uid))
                 (name  (if (string? xname) (string->immutable-string xname) (void))))
            (hashtable-set! cache uid name) ;; also cache lookup failures
            name)))
    (void)))


(define (if-gid->groupname rx gid)
  (if (and (integer? gid) (exact? gid))
    ;; use short-lived cache gid -> groupname stored in process-reader
    ;; reduces syscall clutter
    (let ((cache (or (process-reader-gid-cache rx)
                     (let ((ht (make-eqv-hashtable)))
                       (process-reader-gid-cache-set! rx ht)
                       ht))))
      (or (hashtable-ref cache gid #f)
          (let* ((xname (gid->groupname gid))
                 (name  (if (string? xname) (string->immutable-string xname) (void))))
            (hashtable-set! cache gid name) ;; also cache lookup failures
            name)))
    (void)))
|#


(define (c->process-entry l bvec)
  (make-process-entry
    (bytevector-u64-native-ref bvec 0)                  ; pid
    (car l)       ; process name, string
    (cadr l)      ; tty, #f or string,
    (caddr l)     ; status, char,   
    (void)        ; TODO: user name, string
    (void)        ; TODO: group name, string
    (bytevector-u64-native-ref bvec (fx* 1 8))          ; uid
    (bytevector-u64-native-ref bvec (fx* 2 8))          ; gid
    (bytevector-u64-native-ref bvec (fx* 3 8))          ; ppid
    (bytevector-u64-native-ref bvec (fx* 4 8))          ; pgrp
    (bytevector-u64-native-ref bvec (fx* 5 8))          ; sid
    (bytevector-u64-native-ref bvec (fx* 6 8))          ; flags
    (bytevector-u64-native-ref bvec (fx* 7 8))          ; mem-resident
    (bytevector-u64-native-ref bvec (fx* 8 8))          ; mem-virtual
    (bytevector-ieee-double-native-ref bvec (fx* 9 8))  ; start-time
    (bytevector-ieee-double-native-ref bvec (fx* 10 8)) ; user-time
    (bytevector-ieee-double-native-ref bvec (fx* 11 8)) ; system-time
    (bytevector-u64-native-ref bvec (fx* 12 8))         ; priority
    (bytevector-s64-native-ref bvec (fx* 13 8))         ; nice
    (bytevector-u64-native-ref bvec (fx* 14 8))         ; num-threads
    (bytevector-u64-native-ref bvec (fx* 15 8))         ; min-fault
    (bytevector-u64-native-ref bvec (fx* 16 8))))       ; maj-fault


;; customize how "process-reader" objects are printed
(record-writer (record-type-descriptor process-reader)
  (lambda (rx port writer)
    (put-string port "(make-process-reader)")))


#|
;; customize how "process-entry" objects are printed
(record-writer (record-type-descriptor process-entry)
  (lambda (e port writer)
    (put-string port "(make-process-entry ")
    (writer (process-entry-name e) port)
    (put-char port #\space)
    (writer (process-entry-type e) port)
    (put-char port #\space)
    (writer (process-entry-size e) port)
    (put-char port #\space)
    (writer (process-entry-target e) port)
    (put-char port #\space)
    (writer (process-entry-mode e) port)
    (put-char port #\space)
    (writer (process-entry-accessed e) port)
    (put-char port #\space)
    (writer (process-entry-modified e) port)
    (put-char port #\space)
    (writer (process-entry-inode-changed e) port)
    (put-char port #\space)
    (writer (process-entry-user e) port)
    (put-char port #\space)
    (writer (process-entry-group e) port)
    (put-char port #\space)
    (writer (process-entry-uid e) port)
    (put-char port #\space)
    (writer (process-entry-gid e) port)
    (put-char port #\space)
    (writer (process-entry-inode e) port)
    (put-char port #\space)
    (writer (process-entry-nlink e) port)
    (put-string port ")")))
|#

) ; close library
