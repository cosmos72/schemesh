;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.


;;; collect information about system processes
;;;
(library (scheme2k os (0 9 3))
  (export make-process-reader process-reader process-reader?
          make-process-entry  process-entry  process-entry?)
  (import
    (rnrs)
    (only (chezscheme)                   1+ foreign-procedure fx1+ fx1- make-time record-writer string->immutable-string void)
    (only (scheme2k bootstrap)           assert*)
    (only (scheme2k containers list)     plist-ref)
    (only (scheme2k io obj)              reader reader-get reader-eof? reader-close reader-skip)
    (only (scheme2k io wire)             wire-register-rtd-reflect)
    (only (scheme2k posix fd)            raise-c-errno)
    (only (scheme2k posix fs)            gid->groupname uid->username)
    (only (scheme2k reflect)             reflect-info-set-autodetect!))


(define-record-type (process-reader %make-process-reader process-reader?)
  (parent reader)
  (fields
    (mutable handle)     ; #f or integer containing C DIR*
    bvec                 ; bytevector, used as buffer for C function c_process_get()
    (mutable uid-cache)  ; #f or eqv-hashtable uid -> user name
    (mutable gid-cache)) ; #f or eqv-hashtable gid -> group name
  (protocol
    (lambda (args->new)
      (lambda (handle)
        ((args->new %process-reader-get %process-reader-skip %process-reader-close)
          handle (make-bytevector (fx1+ (fx* 24 8))) #f #f))))
  (nongenerative %process-reader-7c46d04b-34f4-4046-b5c7-b63753c1be40))


(define make-process-reader
  (let ((c-process-open (foreign-procedure "c_process_open" () ptr)))
    (lambda ()
      (let ((obj (c-process-open)))
        (unless (and (integer? obj) (exact? obj) (> obj 0))
          (raise-c-errno 'make-process-reader 'opendir obj "/proc"))
        (%make-process-reader obj)))))


;; called by (reader-get)
(define %process-reader-get
  (let ((c-process-get (foreign-procedure "c_process_get" (ptr ptr) ptr)))
    (lambda (rx)
      (let ((handle (process-reader-handle rx)))
        (if handle
          (let ((bvec (process-reader-bvec rx)))
            (let ((ret (c-process-get handle bvec)))
              (cond
                ((pair? ret)
                  (values (c->process-entry rx ret bvec) #t))
                ((not ret)
                  (%process-reader-get rx)) ;; error parsing /proc/pid/stat, skip it
                ((eqv? 0 ret)
                  (values #f #f)) ;; process-reader is exhausted
                (else
                  (raise-c-errno 'process-reader-get 'readdir ret handle)))))
          (values #f #f)))))) ;; process-reader is closed


;; called by (reader-skip)
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


;; called by (reader-close)
(define %process-reader-close
  (let ((c-process-close (foreign-procedure "c_process_close" (ptr) void)))
    (lambda (rx)
      (let ((handle (process-reader-handle rx)))
        (when handle
          (process-reader-handle-set! rx #f)
          (c-process-close handle))))))


;; info about a running process.
;; TODO validate args in (make-process-entry)
(define-record-type process-entry
  (fields
    (mutable pid)           ; (void) or exact integer
    (mutable name)          ; string
    (mutable tty)           ; (void) or string
    (mutable state)         ; (void) or symbol
    (mutable user)          ; (void) or immutable string
    (mutable group)         ; (void) or immutable string
    (mutable uid)           ; (void) or exact integer
    (mutable gid)           ; (void) or exact integer
    (mutable ppid)          ; (void) or exact integer
    (mutable pgrp)          ; (void) or exact integer
    (mutable sid)           ; (void) or exact integer
  ; (mutable flags)         ; (void) or exact integer REMOVED
    (mutable mem-rss)       ; (void) or exact integer
    (mutable mem-virtual)   ; (void) or exact integer
    (mutable start-time)    ; (void) or time-utc
    (mutable user-time)     ; (void) or time-duration
    (mutable sys-time)      ; (void) or time-duration
    (mutable iowait-time)   ; (void) or time-duration
    (mutable priority)      ; (void) or exact integer
    (mutable nice)          ; (void) or exact integer
  ; (mutable rt-priority)   ; (void) or exact integer REMOVED
  ; (mutable rt-policy)     ; (void) or exact integer REMOVED
    (mutable threads)       ; (void) or exact integer
    (mutable min-fault)     ; (void) or exact integer
    (mutable maj-fault))    ; (void) or exact integer
  (nongenerative %process-entry-7c46d04b-34f4-4046-b5c7-b63753c1be40))


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

(define (u8->symbol u8)
  (string->symbol (string (integer->char u8))))

(define-syntax bvec-ref/s64 (identifier-syntax bytevector-s64-native-ref))
(define-syntax bvec-ref/u64 (identifier-syntax bytevector-u64-native-ref))


(define (c->process-entry rx l bvec)
  (let ((uid    (bvec-ref/u64 bvec (fx* 1 8)))
        (gid    (bvec-ref/u64 bvec (fx* 2 8))))
    (make-process-entry
      (bvec-ref/s64 bvec 0)             ; pid,          int64
      (car l)                           ; process name, string
      (or (cdr l) "")                   ; tty,          string or (void)
      (u8->symbol
        (bytevector-u8-ref bvec (fx* 24 8))) ; state,   symbol
      (if-uid->username rx uid)         ; user name,    string or (void)
      (if-uid->username rx gid)         ; group name,   string or (void)
      uid                               ; uid,          uint64
      gid                               ; gid,          uint64
      (bvec-ref/s64 bvec (fx* 3 8))     ; ppid,         int64
      (bvec-ref/s64 bvec (fx* 4 8))     ; pgrp,         int64
      (bvec-ref/s64 bvec (fx* 5 8))     ; sid,          int64
    ; (bvec-ref/u64 bvec (fx* 6 8))     ; flags,        uint64 REMOVED
      (bvec-ref/u64 bvec (fx* 7 8))     ; mem-rss,      uint64
      (bvec-ref/u64 bvec (fx* 8 8))     ; mem-virtual,  uint64
      (make-time 'time-utc
        (bvec-ref/u64 bvec (fx*  9 8))  ; start-time-utc, ns
        (bvec-ref/s64 bvec (fx* 10 8))) ; start-time-utc, s
      (make-time 'time-duration
        (bvec-ref/u64 bvec (fx* 11 8))  ; user-time, ns
        (bvec-ref/s64 bvec (fx* 12 8))) ; user-time, s
      (make-time 'time-duration
        (bvec-ref/u64 bvec (fx* 13 8))  ; system-time, ns
        (bvec-ref/s64 bvec (fx* 14 8))) ; system-time, s
      (make-time 'time-duration
        (bvec-ref/u64 bvec (fx* 15 8))  ; iowait-time, ns
        (bvec-ref/s64 bvec (fx* 16 8))) ; iowait-time, s
      (bvec-ref/s64 bvec (fx* 17 8))    ; priority,     int64
      (bvec-ref/s64 bvec (fx* 18 8))    ; nice,         int64
    ; (bvec-ref/u64 bvec (fx* 19 8))    ; rt-priority,  uint64 REMOVED
    ; (bvec-ref/u64 bvec (fx* 20 8))    ; rt-policy,    uint64 REMOVED
      (bvec-ref/s64 bvec (fx* 21 8))    ; threads,      int64
      (bvec-ref/u64 bvec (fx* 22 8))    ; min-fault,    uint64
      (bvec-ref/u64 bvec (fx* 23 8))))) ; maj-fault,    uint64


;; customize how "process-reader" objects are printed
(record-writer (record-type-descriptor process-reader)
  (lambda (rx port writer)
    (put-string port "#<process-reader")
    (put-string port (if (reader-eof? rx) " eof>" " ok>"))))


;; customize how "process-entry" objects are printed
(record-writer (record-type-descriptor process-entry)
  (lambda (e port writer)
    (put-string port "(make-process-entry ")
                            (writer (process-entry-pid e) port)
    (put-char port #\space) (writer (process-entry-name e) port)
    (put-char port #\space) (writer (process-entry-tty e) port)
    (put-char port #\space) (writer (process-entry-state e) port)
    (put-char port #\space) (writer (process-entry-user e) port)
    (put-char port #\space) (writer (process-entry-group e) port)
    (put-char port #\space) (writer (process-entry-uid e) port)
    (put-char port #\space) (writer (process-entry-gid e) port)
    (put-char port #\space) (writer (process-entry-ppid e) port)
    (put-char port #\space) (writer (process-entry-pgrp e) port)
    (put-char port #\space) (writer (process-entry-sid e) port)
  ; (put-char port #\space) (writer (process-entry-flags e) port) ; REMOVED
    (put-char port #\space) (writer (process-entry-mem-rss e) port)
    (put-char port #\space) (writer (process-entry-mem-virtual e) port)
    (put-char port #\space) (writer (process-entry-start-time e) port)
    (put-char port #\space) (writer (process-entry-user-time e) port)
    (put-char port #\space) (writer (process-entry-sys-time e) port)
    (put-char port #\space) (writer (process-entry-iowait-time e) port)
    (put-char port #\space) (writer (process-entry-priority e) port)
    (put-char port #\space) (writer (process-entry-nice e) port)
  ; (put-char port #\space) (writer (process-entry-rt-priority e) port) ; REMOVED
  ; (put-char port #\space) (writer (process-entry-rt-policy e) port) ; REMOVED
    (put-char port #\space) (writer (process-entry-threads e) port)
    (put-char port #\space) (writer (process-entry-min-fault e) port)
    (put-char port #\space) (writer (process-entry-maj-fault e) port)
    (put-string port ")")))


(let ((rtd (record-type-descriptor process-entry))
      (tag-process-entry 239))

  ;; customize visible reflect fields for `process-entry` objects.
  ;; do NOT register a deserializer that calls (make-process-entry), because it alters incoming fields order
  (reflect-info-set-autodetect! rtd #f)

  ;; customize how `wire` library serializes/deserializes `process-entry` objects
  (wire-register-rtd-reflect rtd tag-process-entry make-process-entry))


) ; close library
