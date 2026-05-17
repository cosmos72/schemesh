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
    (only (chezscheme)                     console-error-port debug-condition display-condition foreign-procedure fx1+ fxlogbit?
                                           include logbit? make-continuation-condition make-format-condition procedure-arity-mask
                                           record-writer reverse! sort! string->immutable-string time? void)
    (only (scheme2k bootstrap)             assert* catch raise-assertf raise-errorf try)
    (only (scheme2k containers bytevector) bytevector<?)
    (only (scheme2k containers charspan)   charspan?)
    (only (scheme2k containers list)       for-list plist-update!)
    (only (scheme2k containers string)     string-prefix? string-suffix?)
    (only (scheme2k containers time)       make-time-utc)
    (only (scheme2k containers utf8b)      string->utf8b)
    (only (scheme2k conversions)           text->bytevector text->bytevector0 text->string)
    (only (scheme2k io obj)                reader reader-get reader-eof? reader-close reader-skip)
    (only (scheme2k posix fd)              c-errno->string raise-c-errno)
    (only (scheme2k reflect)               make-reflect-info-autodetect reflect-info-set!))


(include "posix/fs-posix.ss")
(include "posix/fs-dir.ss")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; recursively scan a directory tree: streaming API
;;;
;;; Scheme reimplementation of POSIX program "find"

(define (%make-dir-reader1 path uid-cache gid-cache)
  (try
    (let ((rx (make-dir-reader path (dir-reader-options dir-path-as-prefix dir-hide-dot-dotdot))))
      (dir-reader-uid-cache-set! rx uid-cache)
      (dir-reader-gid-cache-set! rx gid-cache)
      rx)
    (catch (ex)
      (debug-condition ex)
      (let ((out (console-error-port)))
        (put-string out "\x1b;[1;31m; ")
        (display-condition ex out)
        (put-string out "\x1b;[m\n")
        (flush-output-port out))
      #f)))


(define (%make-dir-reader-list path-list uid-cache gid-cache dirs)
  (if (null? path-list)
    (reverse! dirs)
    ;; TODO: on exception, close already-created dir-readers
    (let ((dir (%make-dir-reader1 (car path-list) uid-cache gid-cache)))
      (%make-dir-reader-list (cdr path-list) uid-cache gid-cache (if dir (cons dir dirs) dirs)))))


(define-record-type (fs-reader %make-fs-reader fs-reader?)
  (parent reader)
  (fields
    path-list          ; initial path list
    accept-entry-proc? ; procedure for deciding whether to accept a dir-entry
    recurse-dir-proc?  ; procedure for deciding whether to recurse into a directory
    (mutable dirs)     ; list of dir-readers
    uid-cache          ; eqv-hashtable uid -> user name
    gid-cache)         ; eqv-hashtable gid -> group name
  (protocol
    (lambda (args->new)
      (let ((%%make-fs-reader ;; shown when displaying procedure
              (lambda (path-list accept-entry-proc? recurse-dir-proc?)
                (assert* 'make-fs-reader (list? path-list))
                (assert* 'make-fs-reader (procedure? accept-entry-proc?))
                (assert* 'make-fs-reader (logbit? 1 (procedure-arity-mask accept-entry-proc?)))
                (assert* 'make-fs-reader (procedure? recurse-dir-proc?))
                (assert* 'make-fs-reader (logbit? 1 (procedure-arity-mask recurse-dir-proc?)))
                (let ((uid-cache (make-eqv-hashtable))
                      (gid-cache (make-eqv-hashtable))
                      (%new      (args->new %fs-reader-get #f %fs-reader-close)))
                  (%new path-list
                        accept-entry-proc? recurse-dir-proc?
                        (%make-dir-reader-list path-list uid-cache gid-cache '())
                        uid-cache
                        gid-cache)))))
        %%make-fs-reader)))
  (nongenerative %fs-reader-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define make-fs-reader
  (case-lambda
    ((path-or-list accept-entry-proc? recurse-dir-proc?)
      (%make-fs-reader (if (or (null? path-or-list) (pair? path-or-list))
                         path-or-list
                         (list path-or-list))
                       (or accept-entry-proc? (lambda (entry) #t))
                       (or recurse-dir-proc?  (lambda (entry) (eq? 'dir (dir-entry-type entry))))))
    ((path-or-list accept-entry-proc?)
      (make-fs-reader path-or-list accept-entry-proc? #f))
    ((path-or-list)
      (make-fs-reader path-or-list #f #f))))


(define (%fs-reader-get rx)
  (let* ((dirs (fs-reader-dirs rx))
         (dir  (and (pair? dirs) (car dirs))))
    (if (not dir)
      (values #f #f)
      (let-values (((entry ok?) (reader-get dir)))
        (cond
          (ok?
            (when (and (memq (dir-entry-type entry) '(dir symlink))
                       ((fs-reader-recurse-dir-proc? rx) entry))
              ;; next call to (%fs-reader-get) will recurse into subdirectory
              (fs-reader-dirs-set! rx (cons (%make-dir-reader1 (dir-entry-name entry)
                                                               (fs-reader-uid-cache rx)
                                                               (fs-reader-gid-cache rx))
                                            dirs)))
            (if ((fs-reader-accept-entry-proc? rx) entry)
              (values entry #t)
              ;; skip entry and retry
              (%fs-reader-get rx)))
          (else
            ;; dir is exausted. pop it and retry
            (fs-reader-dirs-set! rx (cdr dirs))
            (%fs-reader-get rx)))))))


(define (%fs-reader-close rx)
  ;; close any still-open dir-reader
  (do ((l (fs-reader-dirs rx) (cdr l)))
      ((null? l))
    (reader-close (car l))))


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
