;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh posix dir (0 7 6))
  (export
      directory-list directory-list* directory-list-type directory-sort!
      file-delete file-rename file-type mkdir ok?)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) foreign-procedure make-continuation-condition make-format-condition sort! void)
    (only (schemesh bootstrap) catch raise-assertf try)
    (only (schemesh containers) bytevector<? charspan? list-iterate string->utf8b)
    (only (schemesh conversions) text->bytevector text->bytevector0)
    (only (schemesh posix fd) c-errno->string raise-c-errno))


(define c-errno-einval ((foreign-procedure "c_errno_einval" () int)))

;; return #t if return-status is either (void) or '(ok ...), that indicate success.
;; otherwise return #f
;;
;; intentionally identical to function (sh-ok?) exported by library (schemesh shell job)
(define (ok? return-status)
  (cond
    ((eq? return-status (void))
      #t)
    ((and (pair? return-status)
          (eq? 'ok (car return-status)))
      #t)
    (else
      #f)))


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
;; Further optional arguments can contain:
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
;; 1. (mkdir) also accepts bytevectors or charspans, not only strings.
;; 3. (mkdir) converts strings and charspans to UTF-8b bytevectors instead of UTF-8.
;; 4. (mkdir) accepts option 'catch, instead Chez Scheme (mkdir) always raises an exception of failure.
;; 5. (mkdir) returns (void) on success and error code on failure, instead of an unspecified value.
(define mkdir
  (let ((c-mkdir (foreign-procedure "c_mkdir" (ptr int) int)))
    (lambda (dirpath . options)
      (let ((err (c-mkdir (text->bytevector0 dirpath)
                          (%find-and-convert-fixnum-option 'mkdir options 'mode #o777))))
        (cond
          ((and (fixnum? err) (fxzero? err))
            (void))
          ((memq 'catch options)
            (if (fixnum? err) err c-errno-einval))
          (else
            (raise-c-errno 'mkdir 'mkdir err dirpath)))))))


;; Delete a file or directory.
;; Mandatory argument path must be a bytevector, string or charspan.
;; Further optional arguments can contain:
;;   'catch    - on error, return numeric c-errno instead of raising a condition
;;
;; On success, returns (void)
;; On error:
;;   if options contain 'catch, returns an integer error code
;;   otherwise raises an exception.
;;
;; Differences between (file-delete) and Chez Scheme (delete-file):
;; 1. (file-delete) also deletes empty directories
;; 2. (file-delete) also accepts bytevectors or charspans, not only strings.
;; 3. (file-delete) converts strings and charspans to UTF-8b bytevectors instead of UTF-8.
;; 4. (file-delete) accepts option 'catch instead of optional boolean argument errors?
;; 5. (file-delete) returns (void) on success and error code on failure, instead of a boolean
(define file-delete
  (let ((c-file-delete (foreign-procedure "c_file_delete" (ptr) int)))
    (lambda (path . options)
      (let ((err (c-file-delete (text->bytevector0 path))))
        (cond
          ((and (fixnum? err) (fxzero? err))
            (void))
          ((memq 'catch options)
            (if (fixnum? err) err c-errno-einval))
          (else
            (raise-c-errno 'file-delete 'remove err path)))))))


;; Move or rename a file or directory from old-path to new-path.
;; Both old-path and new-path are mandatory and each one must be a bytevector, string or charspan.
;; Further optional arguments can contain:
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
;; Differs from Chez Scheme (rename-file) in three aspects:
;; 1. (file-rename) also accepts bytevectors and charspans, not only strings.
;; 2. (file-rename) converts strings and charspans to UTF-8b, instead of UTF-8.
;; 3. (file-rename) also accepts option 'catch, while Chez (rename-file) always raises an exception on failure.
;; 4. (file-rename) returns (void) on success and error code on failure, instead of an unspecified value.
(define file-rename
  (let ((c-file-rename (foreign-procedure "c_file_rename" (ptr ptr) int)))
    (lambda (old-path new-path . options)
      (let ((err (c-file-rename (text->bytevector0 old-path) (text->bytevector0 new-path))))
        (cond
          ((and (fixnum? err) (fxzero? err))
            (void))
          ((memq 'catch options)
            (if (fixnum? err) err c-errno-einval))
          (else
            (raise-c-errno 'file-rename 'rename err old-path new-path)))))))


(define c-type->file-type
  (let ((file-types '#(unknown blockdev chardev dir fifo file socket symlink)))
    (lambda (c-type)
      (if (fx<=? 0 c-type 7) (vector-ref file-types c-type) 'unknown))))


;; Check existence and type of a filesystem path.
;; Mandatory first argument path must be a bytevector, string or charspan.
;; Further optional arguments can contain:
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
    (lambda (path . options)
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
            (raise-c-errno 'file-type (if symlinks? 'lstat 'stat) ret path)))))))


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
;; Mandatory first argument dirpath must be a bytevector, string or charspan.
;; Mandatory second argument options must be a list of options, see (directory-list) for details.
;;
;; if option 'types is specified, returns a list of pairs (filename . type) where:
;;   each filename is a either a bytevector (if options contain 'bytes) or a string
;;   each type is one of: 'unknown 'blockdev 'chardev 'dir 'fifo 'file 'socket 'symlink
;;
;; if option 'types is not specified, returns a list of filenames where:
;;   each filename is a either a bytevector (if options contain 'bytes) or a string
(define directory-list*
  (let ((c-directory-list (foreign-procedure "c_directory_list" (ptr ptr ptr int) ptr)))
    (lambda (dirpath options)
      ; (debugf "directory-list-type dir=~s, options=~s" dirpath options)
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
              (list-iterate ret
                (lambda (entry)
                  (set-cdr! entry (c-type->file-type (cdr entry))))))
            ret)
          ((memq 'catch options)
            '())
          (else
            (raise-c-errno 'directory-list-type 'opendir ret dirpath)))))))



;; List contents of a filesystem directory, in arbitrary order.
;; WARNING: Chez Scheme also defines a function (directory-list) with different options.
;;
;; Mandatory first argument dirpath must be a bytevector, string or charspan.
;; Further optional arguments can contain:
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
(define (directory-list dirpath . options)
  (directory-list* dirpath options))


;; List contents of a filesystem directory, in arbitrary order.
;; Mandatory first argument dirpath must be a bytevector, string or charspan.
;; Further optional arguments can contain the same options described in (directory-list)
;; with the difference that option 'types is always considered to be present.
;;
;; returns a list of pairs (filename . type) where:
;;   each filename is a either a bytevector (if options contain 'bytes) or a string
;;   each type is one of: 'unknown 'blockdev 'chardev 'dir 'fifo 'file 'socket 'symlink
(define (directory-list-type dirpath . options)
  (directory-list* dirpath (cons 'types options)))



;; in-place sort dir-list, which must have the same structure as the output
;; of (directory-list) (directory-list*) or (directory-list-type)
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


) ; close library
