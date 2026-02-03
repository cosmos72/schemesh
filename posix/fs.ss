;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k posix fs (0 9 3))
  (export
      (rename (make-dir-reader dir)) make-dir-reader
      dir-reader dir-reader? dir-reader-path dir-reader-eof? dir-reader-close dir-reader-get

      directory-list directory-list-type directory-sort!
      file-delete file-rename file-type mkdir)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)           foreign-procedure make-continuation-condition make-format-condition record-writer sort! void)
    (only (scheme2k bootstrap)   assert* catch raise-assertf try)
    (only (scheme2k containers)  bytevector<? charspan? for-list string->utf8b)
    (only (scheme2k conversions) text->bytevector text->bytevector0 text->string)
    (only (scheme2k io obj)      obj-reader obj-reader-get obj-reader-eof? obj-reader-close)
    (only (scheme2k posix fd)    c-errno->string raise-c-errno))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; streaming API

(define-record-type (dir-reader %make-dir-reader dir-reader?)
  (parent obj-reader)
  (fields
    (mutable handle)  ; #f or integer containing C DIR*
    vec               ; vector, used as buffer for C function c_dir_next()              
    path)             ; directory being read
  (protocol
    (lambda (args->new)
      (lambda (handle path)
        ((args->new %dir-reader-get %dir-reader-close) handle (make-vector 14 (void)) path))))
  (nongenerative %dir-reader-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define make-dir-reader
  (let ((c-dir-open (foreign-procedure "c_dir_open" (ptr) ptr)))
    (case-lambda
      ((path)
        (let ((obj (c-dir-open (text->bytevector0 path))))
          (unless (and (integer? obj) (exact? obj) (> obj 0))
            (raise-c-errno 'make-dir-reader 'opendir obj path))
          (%make-dir-reader obj (text->string path)))))))


(define (dir-reader-eof? rx)
  (assert* 'dir-reader-eof? (dir-reader? rx))
  (obj-reader-eof? rx))


(define (dir-reader-close rx)
  (assert* 'dir-reader-close (dir-reader? rx))
  (obj-reader-close rx))


(define (dir-reader-get rx)
  (assert* 'dir-reader-get (dir-reader? rx))
  (obj-reader-get rx))


;; called by (dir-reader-get) -> (obj-reader-get)
(define %dir-reader-get
  (let ((c-dir-next (foreign-procedure "c_dir_next" (void* ptr unsigned) int)))
    (lambda (rx)
      (let ((handle (dir-reader-handle rx)))
        (if handle
          (let ((vec (dir-reader-vec rx)))
            (vector-fill! vec (void))
             (let ((err (c-dir-next handle vec #x3fff)))
               (unless (and (fixnum? err) (fx>=? err 0))
                 (raise-c-errno 'dir-reader-get 'readdir err handle)))
            (values vec #t))
          (values #f #f))))))
          

;; called by (dir-reader-close) -> (obj-reader-close)
(define %dir-reader-close
  (let ((c-dir-close (foreign-procedure "c_dir_close" (void*) void)))
    (lambda (rx)
      (let ((handle (dir-reader-handle rx)))
        (when handle
          (dir-reader-handle-set! rx #f)
          (c-dir-close handle))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; low-level API

(define c-errno-einval ((foreign-procedure "c_errno_einval" () int)))


(define (%find-and-convert-fixnum-option caller options key default)
  (let ((option (memq key options)))
    (if option
      (let ((value (if (null? (cdr option)) '() (cadr option))))
        (unless (fixnum? value)
          (raise-assertf caller "expecting a fixnum after option '~a, found ~s"
            key value))
        value)
      default)))


;; Create a directory.
;; WARNING: Chez Scheme also defines a function (mkdir) with different options
;;
;; Mandatory first argument dirpath must be a bytevector, string or charspan.
;; Optional second argument options must be a list, containing zero or more:
;;   'catch - errors will be ignored instead of raising a condition
;;   'mode followed by a fixnum - specifies the owner, group and others initial permissions
;;            on the directory - see "man 2 mkdir" for details.
;;
;; On success, returns (void)
;; On error:
;;   if options contain 'catch, returns an integer error code
;;   otherwise raises an exception.
;;
;; Differences between (mkdir) and Chez Scheme (mkdir):
;; 1. (mkdir) also accepts bytevectors, bytespans or charspans, not only strings.
;; 3. (mkdir) converts strings and charspans to UTF-8b bytevectors instead of UTF-8.
;; 4. (mkdir) accepts option 'catch, instead Chez Scheme (mkdir) always raises an exception of failure.
;; 5. (mkdir) returns (void) on success and error code on failure, instead of an unspecified value.
(define mkdir
  (let ((c-mkdir (foreign-procedure "c_mkdir" (ptr int) int)))
    (case-lambda
      ((dirpath options)
        (let ((err (c-mkdir (text->bytevector0 dirpath)
                            (%find-and-convert-fixnum-option 'mkdir options 'mode #o777))))
          (cond
            ((and (fixnum? err) (fxzero? err))
              (void))
            ((memq 'catch options)
              (if (fixnum? err) err c-errno-einval))
            (else
              (raise-c-errno 'mkdir 'mkdir err dirpath)))))
      ((dirpath)
        (mkdir dirpath '())))))


;; Delete a file or directory.
;; Mandatory first argument path must be a bytevector, string or charspan.
;; Optional second argument options must be a list containing zero or more:
;;   'catch    - on error, return numeric c-errno instead of raising a condition
;;
;; On success, returns (void)
;; On error:
;;   if options contain 'catch, returns an integer error code
;;   otherwise raises an exception.
;;
;; Differences between (file-delete) and Chez Scheme (delete-file):
;; 1. (file-delete) also deletes empty directories
;; 2. (file-delete) also accepts bytevectors, bytespans or charspans, not only strings.
;; 3. (file-delete) converts strings and charspans to UTF-8b bytevectors instead of UTF-8.
;; 4. (file-delete) accepts optional list instead of optional boolean argument errors?
;; 5. (file-delete) returns (void) on success and error code on failure, instead of a boolean
(define file-delete
  (let ((c-file-delete (foreign-procedure "c_file_delete" (ptr) int)))
    (case-lambda
      ((path options)
        (let ((err (c-file-delete (text->bytevector0 path))))
          (cond
            ((and (fixnum? err) (fxzero? err))
              (void))
            ((memq 'catch options)
              (if (fixnum? err) err c-errno-einval))
            (else
              (raise-c-errno 'file-delete 'remove err path)))))
      ((path)
        (file-delete path '())))))

;; Move or rename a file or directory from old-path to new-path.
;; Both old-path and new-path are mandatory and each one must be a bytevector, string or charspan.
;; Optional third argument options must be a list containing zero or more:
;;   'catch    - on error, return numeric c-errno instead of raising a condition
;;
;; Note: to move a file or directory into an existing directory,
;;       new-path must contain the directory path followed by "/SOME_NEW_NAME"
;;
;; On success, returns (void)
;; On error:
;;   if options contain 'catch, returns an integer error code
;;   otherwise raises an exception.
;;
;; Differs from Chez Scheme (rename-file) in several aspects:
;; 1. (file-rename) also accepts bytevectors, bytespans and charspans, not only strings.
;; 2. (file-rename) converts strings and charspans to UTF-8b, instead of UTF-8.
;; 3. (file-rename) also accepts option 'catch, while Chez (rename-file) always raises an exception on failure.
;; 4. (file-rename) returns (void) on success and error code on failure, instead of an unspecified value.
(define file-rename
  (let ((c-file-rename (foreign-procedure "c_file_rename" (ptr ptr) int)))
    (case-lambda
      ((old-path new-path options)
        (let ((err (c-file-rename (text->bytevector0 old-path) (text->bytevector0 new-path))))
          (cond
            ((and (fixnum? err) (fxzero? err))
              (void))
            ((memq 'catch options)
              (if (fixnum? err) err c-errno-einval))
            (else
              (raise-c-errno 'file-rename 'rename err old-path new-path)))))
      ((old-path new-path)
        (file-rename old-path new-path '())))))


(define c-type->file-type
  (let ((file-types '#(unknown blockdev chardev dir fifo file socket symlink)))
    (lambda (c-type)
      (if (fx<=? 0 c-type 7) (vector-ref file-types c-type) 'unknown))))


;; Check existence and type of a filesystem path.
;; Mandatory first argument path must be a bytevector, string, bytespan or charspan.
;; Second optional argument options must be a list containing zero or more:
;;   'catch    - return numeric c-errno instead of raising a condition on C functions error
;;   'symlinks - returned filenames that are symlinks will have type 'symlink
;;               instead of the type of the file they point to.
;;
;; If file exists, return its type which is one of:
;;   'unknown 'blockdev 'chardev 'dir 'fifo 'file 'socket 'symlink
;; Returns #f if file does not exist.
;;
(define file-type
  (let ((c-file-type (foreign-procedure "c_file_type" (ptr int) ptr)))
    (case-lambda
      ((path options)
        (let* ((symlinks? (memq 'symlinks options))
               (ret (c-file-type (text->bytevector0 path)
                                 (if symlinks? 1 0))))
          (cond
            ((and (fixnum? ret) (fx>=? ret 0))
              (c-type->file-type ret))
            ((eq? ret #f)
              #f)
            ((memq 'catch options)
              (if (fixnum? ret) ret c-errno-einval))
            (else
              (raise-c-errno 'file-type (if symlinks? 'lstat 'stat) ret path)))))
      ((path)
        (file-type path '())))))


(define (%find-and-convert-text-option caller options key)
  (let ((option (memq key options)))
    (if option
      (let ((value (if (null? (cdr option)) '() (cadr option))))
        (unless (or (bytevector? value) (string? value) (charspan? value))
          (raise-assertf caller "expecting a bytevector, string or charspan after option '~a, found ~s"
            key value))
        (text->bytevector value))
      #vu8())))





;; List contents of a filesystem directory, in arbitrary order.
;; WARNING: Chez Scheme also defines a function (directory-list) with different options.
;;
;; Mandatory first argument dirpath must be a bytevector, string, bytespan or charspan.
;; Optional second argument options must be a list containing zero or more:
;;   'append-slash - if a returned type is 'dir then a '/' will be appended
;;            to corresponding filename
;;   'bytes - each returned filename will be a bytevector, not a string
;;   'catch - errors will be ignored instead of raising a condition
;;   'symlinks - returned filenames that are symlinks will have type 'symlink
;;               instead of the type of the file they point to.
;;   'types  - each returned list element will be a pair (filename . type)
;;             where filename is a bytevector or string and type is a symbol:
;;             see below for possible values
;;   'prefix followed by a charspan, string or bytevector, indicating the filter-prefix:
;;            only filenames that start with such filter-prefix will be returned.
;;   'suffix followed by a charspan, string or bytevector, indicating the filter-suffix:
;;            only filenames that end with such filter-suffix will be returned.
;;
;; if option 'type is specified, returns a list of pairs (filename . type) where:
;;  each filename is a either a bytevector (if options contain 'bytes) or a string
;;  each type is one of: 'unknown 'blockdev 'chardev 'dir 'fifo 'file 'socket 'symlink
;;
;; if option 'type is not specified, returns a list of filename where:
;;  each filename is a either a bytevector (if options contain 'bytes) or a string
(define directory-list
  (let ((c-directory-list (foreign-procedure "c_directory_list" (ptr ptr ptr int) ptr)))
    (case-lambda
      ((dirpath options)
        ; (debugf "directory-list dir=~s, options=~s" dirpath options)
        (let ((ret (c-directory-list
                     (text->bytevector0 dirpath)
                     (%find-and-convert-text-option 'directory-list options 'prefix)
                     (%find-and-convert-text-option 'directory-list options 'suffix)
                     (fxior (if (memq 'symlinks options) 1 0)
                            (if (memq 'append-slash options) 2 0)
                            (if (memq 'bytes    options) 4 0)
                            (if (memq 'types    options) 8 0)))))
          (cond
            ((null? ret)
              ret)
            ((pair? ret)
              (when (memq 'types options)
                (for-list ((entry ret))
                  (set-cdr! entry (c-type->file-type (cdr entry)))))
              ret)
            ((memq 'catch options)
              '())
            (else
              (raise-c-errno 'directory-list 'opendir ret dirpath)))))
      ((dirpath)
        (directory-list dirpath '())))))


;; List contents of a filesystem directory, in arbitrary order.
;; Mandatory first argument dirpath must be a bytevector, string or charspan.
;; Optional second argument options must be a list containing the same options described in (directory-list)
;; with the difference that option 'types is always considered to be present.
;;
;; returns a list of pairs (filename . type) where:
;;   each filename is a either a bytevector (if options contain 'bytes) or a string
;;   each type is one of: 'unknown 'blockdev 'chardev 'dir 'fifo 'file 'socket 'symlink
(define directory-list-type
  (case-lambda
    ((dirpath)
       (directory-list dirpath '(types)))
    ((dirpath options)
       (directory-list dirpath (cons 'types options)))))



;; in-place sort dir-list, which must have the same structure as the output
;; of (directory-list) or (directory-list-type)
;; i.e. it must be a possibly empty list of strings, or list of bytevectors,
;; or list or pairs (filename . type) where all filenames are either strings or bytevectors:
;; mixtures are not allowed.
(define (directory-sort! dir-list)
  (if (or (null? dir-list) (null? (cdr dir-list)))
    dir-list
    (sort!
      (let ((elem (car dir-list)))
        (cond
          ((string? elem)
            string<?)
          ((bytevector? elem)
            bytevector<?)
          ((and (pair? elem) (string? (car elem)))
            (lambda (entry1 entry2)
              (string<? (car entry1) (car entry2))))
          ((and (pair? elem) (bytevector? (car elem)))
            (lambda (entry1 entry2)
              (bytevector<? (car entry1) (car entry2))))
          (else
            (raise-assertf 'directory-sort! "expecting a list of string, bytevectors, or pairs (key . value)\
              where all keys are bytevector or string, found list element ~s"
              elem))))
      dir-list)))


;; customize how "dir-reader" objects are printed
(record-writer (record-type-descriptor dir-reader)
  (lambda (rx port writer)
    (put-string port "(make-dir-reader ")
    (write (dir-reader-path rx) port)
    (put-string port ")")))


) ; close library
