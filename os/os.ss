;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.


;;; collect information about system processes
;;;
(library (scheme2k os (1 0 0))
  (export make-disk-reader    disk-reader    disk-reader?    make-disk-entry    disk-entry    disk-entry?
          make-process-reader process-reader process-reader? make-process-entry process-entry process-entry?)
  (import
    (rnrs)
    (only (chezscheme)                   1+ foreign-procedure fx1+ fx1- include make-time record-writer string->immutable-string void)
    (only (scheme2k bootstrap)           assert*)
    (only (scheme2k containers list)     plist-update!)
    (only (scheme2k io obj)              reader reader-get reader-eof? reader-close reader-skip)
    (only (scheme2k io wire)             wire-register-rtd-reflect)
    (only (scheme2k posix base)          raise-c-errno)
    (only (scheme2k posix fs)            c-dev-major c-dev-minor gid->groupname uid->username)
    (only (scheme2k reflect)             make-reflect-info-autodetect reflect-info-set!))


(define-syntax bvec-ref/s64 (identifier-syntax bytevector-s64-native-ref))
(define-syntax bvec-ref/u64 (identifier-syntax bytevector-u64-native-ref))

(define (u8->symbol u8)
  (string->symbol (string (integer->char u8))))

(include "os/disk.ss")
(include "os/process.ss")

(include "os/disk_init.ss")
(include "os/process_init.ss")



) ; close library
