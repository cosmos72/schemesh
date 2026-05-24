;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file posix/fs.ss

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wrappers for low-level POSIX API

(define c-errno-einval ((foreign-procedure "c_errno_einval" () int))) ;; integer, not a procedure
(define c-errno-enoent ((foreign-procedure "c_errno_enoent" () int))) ;; integer, not a procedure

(define c-make-dev  (foreign-procedure "c_make_dev"  (unsigned unsigned) unsigned-64))
(define c-dev-major (foreign-procedure "c_dev_major" (unsigned-64) unsigned))
(define c-dev-minor (foreign-procedure "c_dev_minor" (unsigned-64) unsigned))


(define chez-cd-wrapper
  (let ((chez-cd (let ()
                   (import (prefix (only (chezscheme) cd) chez:))
                   chez:cd)))
    (case-lambda
      (()           (chez-cd))
      ((new-dir)    (chez-cd (text->string new-dir))))))


;; intentionally same name as Chez Scheme (cd)
(define cd
  (let ((proc          chez-cd-wrapper)
        (can-set-proc? #t))
    (case-lambda
      (()           (proc))
      ((new-dir)    (proc new-dir))
      ((_ new-proc) (assert* 'cd can-set-proc?)
                    (assert* 'cd (procedure? new-proc))
                    (set! can-set-proc? #f)
                    (set! proc new-proc)))))


;; intentionally same name as Chez Scheme (current-directory)
(define current-directory cd)


;; convert bytevector, bytespan, string or charspan path
;; to bytevector0 C path.
;;
;; does NOT assume that OS's idea of current directory matches our own:
;; each job may have its own current directory
(define (path->c-path0 text)
  (let ((c-path0 (text->bytevector0 text)))
    (assert* 'path->c-path0 (fx>? (bytevector-length c-path0) 1))
    (if (fx=? 47 (bytevector-u8-ref c-path0 0)) ; #\/
      c-path0
      (let ((c-prefix (string->utf8 (cd))))
        (if (fx=? 47 (bytevector-u8-ref c-prefix (fx1- (bytevector-length c-prefix))))
          (bytevector-append c-prefix c-path0)
          (bytevector-append c-prefix #vu8(47) c-path0))))))
        

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
        (let ((err (c-mkdir (path->c-path0 dirpath)
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
        (let ((err (c-file-delete (path->c-path0 path))))
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
        (let ((err (c-file-rename (path->c-path0 old-path) (path->c-path0 new-path))))
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
  (let ((file-types '#(unknown fifo chardev dir blockdev file symlink socket)))
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
;;   'unknown 'fifo 'chardev 'dir 'blockdev 'file 'symlink 'socket
;; Returns #f if file does not exist.
;;
(define file-type
  (let ((c-file-type (foreign-procedure "c_file_type" (ptr int) ptr)))
    (case-lambda
      ((path options)
        (let* ((symlinks? (memq 'symlinks options))
               (ret (c-file-type (path->c-path0 path)
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


;; Check existence and retrieve information for filesystem path.
;; Mandatory first argument path must be a bytevector, string, bytespan or charspan.
;; Second optional argument options must be a list containing zero or more:
;;   'catch    - return numeric c-errno instead of raising a condition on C functions error
;;   'symlinks - if path refers to a symlink, return info about the symlink itself.
;;               by default, return info about the file it points to.
;;
;; If file exists, return a dir-entry containing information about it.
;; Return #f if file does not exist.
;; Raise condition on errors, unless options contain 'catch
(define file-stat
  (let ((c-file-stat (foreign-procedure "c_file_stat" (ptr int ptr) ptr)))
    (case-lambda
      ((path options)
        (let* ((vec       (make-dir-entry-vector))
               (symlinks? (memq 'symlinks options))
               (err       (c-file-stat (path->c-path0 path) (if symlinks? 1 0) vec)))
          (cond
            ((and (fixnum? err) (fx>=? err 0))
              (vector->dir-entry #f vec))
            ((not err)
              #f) ; path does not exist
            ((memq 'catch options)
              (if (fixnum? err) err c-errno-einval))
            (else
              (raise-c-errno 'file-stat (if symlinks? 'lstat 'stat) err path)))))
      ((path)
        (file-stat path '())))))



(define (%find-and-convert-text-option caller options key)
  (let ((option (memq key options)))
    (if option
      (let ((value (if (null? (cdr option)) '() (cadr option))))
        (unless (text? value)
          (raise-assertf caller "expecting a bytevector, bytespan, string or charspan after option '~a, found ~s"
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
;;  each type is one of: 'unknown 'fifo 'chardev 'dir 'blockdev 'file 'symlink 'socket
;;
;; if option 'type is not specified, returns a list of filename where:
;;  each filename is a either a bytevector (if options contain 'bytes) or a string
(define directory-list
  (let ((c-directory-list (foreign-procedure "c_directory_list" (ptr ptr ptr int) ptr)))
    (case-lambda
      ((dirpath options)
        ; (debugf "directory-list dir=~s, options=~s" dirpath options)
        (let ((ret (c-directory-list
                     (path->c-path0 dirpath)
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
;;   each type is one of: 'unknown 'fifo 'chardev 'dir 'blockdev 'file 'symlink 'socket
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


;; return string on success, or c-errno integer < 0 on error
(define gid->groupname
  (let ((c-get-groupname (foreign-procedure "c_get_groupname" (int) ptr)))
    (lambda (gid)
      (if (fixnum? gid)
        (c-get-groupname gid)
        c-errno-einval))))


;; return string on success, or c-errno integer < 0 on error
(define uid->username
  (let ((c-get-username (foreign-procedure "c_get_username" (int) ptr)))
    (lambda (uid)
      (if (fixnum? uid)
        (c-get-username uid)
        c-errno-einval))))
