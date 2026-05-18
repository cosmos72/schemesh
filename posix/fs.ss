;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k posix fs (1 0 0))
  (export
      c-make-dev c-dev-major c-dev-minor

      dir-entry-name dir-entry-type dir-entry-size dir-entry-link dir-entry-modified dir-entry-accessed
      dir-entry-status-changed dir-entry-mode dir-entry-user dir-entry-group dir-entry-uid dir-entry-gid
      dir-entry-dev dir-entry-rdev dir-entry-inode dir-entry-nlink

      make-dir-entry  dir-entry  dir-entry?
      make-dir-reader dir-reader dir-reader? dir-reader-options dir-reader-path
      make-fs-reader  fs-reader  fs-reader?  fs-reader-path-list

      directory-list directory-list-type directory-sort!
      file-delete file-rename file-stat file-type mkdir
      gid->groupname uid->username)
  (import
    (rename (rnrs)                         (fxarithmetic-shift-right fx>>))
    (rnrs mutable-pairs)
    (rnrs mutable-strings)
    (only (chezscheme)                     debug-condition display-condition foreign-procedure format fx1+ fx1- fxlogbit?
                                           include logbit? make-continuation-condition make-format-condition
                                           procedure-arity-mask record-writer reverse! sort! string->immutable-string time? void)
    (only (scheme2k bootstrap)             assert* catch raise-assertf raise-errorf try)
    (only (scheme2k containers bytevector) bytevector<?)
    (only (scheme2k containers charspan)   charspan?)
    (only (scheme2k containers list)       for-list plist-update!)
    (only (scheme2k containers span)       list->span span-ref span-clear! span-delete-right! span-empty? span-insert-right! span-length)
    (only (scheme2k containers string)     string-prefix? string-suffix?)
    (only (scheme2k containers time)       make-time-utc)
    (only (scheme2k containers utf8b)      string->utf8b)
    (only (scheme2k conversions)           bytevector0->string text? text->bytevector text->bytevector0 text->string)
    (only (scheme2k io obj)                reader reader? reader-get reader-eof? reader-close reader-skip)
    (only (scheme2k posix fd)              c-errno->string raise-c-errno warn-c-errno)
    (only (scheme2k reflect)               make-reflect-info-autodetect reflect-info-set!))


(include "posix/fs-posix.ss")
(include "posix/fs-dir.ss")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; recursively scan a directory tree: streaming API
;;;
;;; Scheme reimplementation of POSIX program "find"

(define (text-list? path-list)
  (do ((l path-list (cdr l)))
      ((or (null? l) (not (text? (car l))))
        (null? l))))


(define-record-type (fs-reader %make-fs-reader fs-reader?)
  (parent reader)
  (fields
    path-list          ; initial path list
    accept-entry-proc? ; procedure for deciding whether to accept a dir-entry
    recurse-dir-proc?  ; procedure for deciding whether to recurse into a directory
    stack              ; span of dir-entry and/or dir-readers
    uid-cache          ; eqv-hashtable uid -> user name
    gid-cache)         ; eqv-hashtable gid -> group name
  (protocol
    (lambda (args->new)
      (let ((%%make-fs-reader ;; shown when displaying procedure
              (lambda (path-list accept-entry-proc? recurse-dir-proc?)
                ;; (assert* 'make-fs-reader (text-list? path-list)) ; redundant, checked by (map text->string path-list) below
                (assert* 'make-fs-reader (procedure? accept-entry-proc?))
                (assert* 'make-fs-reader (logbit? 1 (procedure-arity-mask accept-entry-proc?)))
                (assert* 'make-fs-reader (procedure? recurse-dir-proc?))
                (assert* 'make-fs-reader (logbit? 2 (procedure-arity-mask recurse-dir-proc?)))
                ((args->new %fs-reader-get #f %fs-reader-close)
                   (map text->string path-list)
                   accept-entry-proc?
                   recurse-dir-proc?
                   (list->span (map text->bytevector0 path-list))
                   (make-eqv-hashtable)
                   (make-eqv-hashtable)))))
        %%make-fs-reader)))
  (nongenerative %fs-reader-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define make-fs-reader
  (case-lambda
    ((path-or-list accept-entry-proc? recurse-dir-proc?)
      (%make-fs-reader (if (or (null? path-or-list) (pair? path-or-list))
                         path-or-list
                         (list path-or-list))
                       (or accept-entry-proc? (lambda (entry) #t))
                       (or recurse-dir-proc?  (lambda (entry depth) (eq? 'dir (dir-entry-type entry))))))
    ((path-or-list accept-entry-proc?)
      (make-fs-reader path-or-list accept-entry-proc? #f))
    ((path-or-list)
      (make-fs-reader path-or-list #f #f))))


(define (%make-dir-reader1 path uid-cache gid-cache)
  (let ((rx (make-dir-reader path (dir-reader-options catch dir-path-as-prefix dir-hide-dot-dotdot))))
    (cond
      ((dir-reader? rx)
        (dir-reader-uid-cache-set! rx uid-cache)
        (dir-reader-gid-cache-set! rx gid-cache)
        rx)
      (else
        (warn-c-errno 'make-dir-reader 'opendir rx path)
        #f))))


(define (fs-reader-stack-push-dir! rx stack entry)
  (let ((dir (%make-dir-reader1 (dir-entry-name entry)
                                (fs-reader-uid-cache rx)
                                (fs-reader-gid-cache rx))))
    (when dir
      (span-insert-right! stack dir))))


(define (%fs-reader-get rx)
  (let* ((stack (fs-reader-stack rx))
         (top   (if (span-empty? stack) #f (span-ref stack 0)))
         (%fs-reader-process
           (lambda (rx entry)
             (when (and (memq (dir-entry-type entry) '(dir symlink))
                        ((fs-reader-recurse-dir-proc? rx) entry (span-length stack)))
              ;; next call to (%fs-reader-get) will recurse into subdirectory
              (fs-reader-stack-push-dir! rx stack entry))
            (if ((fs-reader-accept-entry-proc? rx) entry)
              (values entry #t)
              ;; skip entry and retry
              (%fs-reader-get rx)))))
    (cond
      ((not top)
        (values #f #f))
      ((bytevector? top)
        (let ((datum (file-stat top '(symlinks catch))))
          (span-delete-right! stack 1)
          (cond
            ((dir-entry? datum)
              (%fs-reader-process rx datum))
            (else
              (warn-c-errno 'fs-reader-get 'lstat (or datum c-errno-enoent) (bytevector0->string top))
              ;; skip entry and retry
              (%fs-reader-get rx)))))
      ((dir-reader? top)
        (let-values (((entry ok?) (reader-get top)))
          (cond
            (ok?
              (%fs-reader-process rx entry))
            (else
              ;; dir is exhausted. pop it and retry
              (span-delete-right! stack 1)
              (%fs-reader-get rx))))))))


(define (%fs-reader-close rx)
  ;; close any dir-reader still open
  (let* ((stack (fs-reader-stack rx))
         (n     (span-length stack)))
    (do ((i (fx1- n) (fx1- i)))
        ((fx<? i 0))
      (let ((e (span-ref stack i)))
        (when (reader? e)
          (reader-close e))))
    (span-clear! stack)))


;; customize how "dir-reader" objects are printed
(record-writer (record-type-descriptor dir-reader)
  (lambda (rx port writer)
    (put-string port "#<dir-reader")
    (put-string port (if (reader-eof? rx) " eof " " ok "))
    (writer (dir-reader-path rx) port)
    (put-char port #\>)))


;; customize how "fs-reader" objects are printed
(record-writer (record-type-descriptor fs-reader)
  (lambda (rx port writer)
    (put-string port "#<fs-reader")
    (put-string port (if (reader-eof? rx) " eof " " ok "))
    (writer (fs-reader-path-list rx) port)
    (put-char port #\>)))


;; customize how "dir-entry" objects are printed
(record-writer (record-type-descriptor dir-entry)
  (let ((accessors (vector dir-entry-name     dir-entry-type     dir-entry-size     dir-entry-link
                           dir-entry-modified dir-entry-accessed dir-entry-status-changed   dir-entry-mode
                           dir-entry-user     dir-entry-group    dir-entry-uid      dir-entry-gid)))
    (lambda (e port writer)
      (put-string port "(make-dir-entry")
      (do ((i 0 (fx1+ i))
           (n (vector-length accessors)))
          ((fx>=? i n))
        (put-char port #\space)
        (writer ((vector-ref accessors i) e) port))
      (put-char port #\space) (write-c-make-dev (dir-entry-dev e) port writer)
      (put-char port #\space) (write-c-make-dev (dir-entry-rdev e) port writer)
      (put-char port #\space) (writer (dir-entry-inode e) port)
      (put-char port #\space) (writer (dir-entry-nlink e) port)
      (put-string port ")"))))

;; customize visible reflect fields for `dir-entry` objects.
;; the deserializer does NOT call (make-dir-entry), because it alters incoming fields order:
;; it only converts string->symbol the field dir-entry-type
(let* ((rtd (record-type-descriptor dir-entry))
       (type-sym (record-type-name rtd)))
  (reflect-info-set! rtd (make-reflect-info-autodetect rtd type-sym) type-sym deserialize-dir-entry))

) ; close library
