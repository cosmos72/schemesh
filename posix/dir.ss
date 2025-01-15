;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh posix dir (0 1))
  (export directory-list directory-sort! file-stat)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) foreign-procedure sort!)
    (only (schemesh bootstrap) catch raise-assertf try)
    (only (schemesh containers) bytevector<? charspan? list-iterate string->utf8b)
    (only (schemesh conversions) text->bytevector text->bytevector0)
    (only (schemesh posix fd) raise-c-errno))

(define c-type->file-type
  (let ((file-types '#(unknown blockdev chardev dir fifo file socket symlink)))
    (lambda (c-type)
      (if (fx<=? 0 c-type 7) (vector-ref file-types c-type) 'unknown))))


;; Check existence and type of a filesystem path.
;; Mandatory first argument dirpath must be a bytevector, string or charspan.
;; Further optional arguments can contain:
;;   'catch    - return #f instead of raising a condition on C functions error
;;   'symlinks - returned filenames that are symlinks will have type 'symlink
;;               instead of the type of the file they point to.
;;
;; If file exists, return its type which is one of:
;;   'unknown 'blockdev 'chardev 'dir 'fifo 'file 'socket 'symlink
;; Returns #f if file does not exist.
;;
(define file-stat
  (let ((c-file-stat (foreign-procedure "c_file_stat" (ptr ptr) ptr)))
    (lambda (path . options)
      (let* ((symlinks? (memq 'symlinks options))
             (ret (c-file-stat (text->bytevector0 path) symlinks?)))
        (cond
          ((and (fixnum? ret) (fx>=? ret 0))
            (c-type->file-type ret))
          ((or (eq? ret #f) (memq 'catch options))
            #f)
          (#t
            (raise-c-errno 'file-stat (if symlinks? 'lstat 'stat) ret path)))))))


;; List contents of a filesystem directory, in arbitrary order.
;; Mandatory first argument dirpath must be a bytevector, string or charspan.
;; Further optional arguments can contain:
;;   'append-slash - if a returned type is 'dir then a '/' will be appended
;;            to corresponding filename
;;   'bytes - each returned filename will be a bytevector, not a string
;;   'catch - errors will be ignored instead of raising a condition
;;   'symlinks - returned filenames that are symlinks will have type 'symlink
;;               instead of the type of the file they point to.
;;   'prefix followed by a charspan, string or bytevector, indicating the filter-prefix:
;;            only filenames that start with such filter-prefix will be returned.
;;   'suffix followed by a charspan, string or bytevector, indicating the filter-suffix:
;;            only filenames that end with such filter-suffix will be returned.
;;
;; Returns a list of pairs (type . filename) where:
;;  each type is one of: 'unknown 'blockdev 'chardev 'dir 'fifo 'file 'socket 'symlink
;;  each filename is a either a bytevector (if options contain 'bytes) or a string
(define directory-list
  (let ((c-directory-list (foreign-procedure "c_directory_list" (ptr ptr ptr int) ptr)))
    (lambda (dirpath . options)
      (let* ((strings? (not (memq 'bytes options)))
             (ret (c-directory-list
                    (text->bytevector0 dirpath)
                    (%find-and-convert-option options 'prefix)
                    (%find-and-convert-option options 'suffix)
                    (fxior (if (memq 'symlinks options) 0 1)
                           (if (memq 'append-slash options) 2 0)
                           (if strings? 4 0)))))
        (cond
          ((null? ret)
            ret)
          ((pair? ret)
            (list-iterate ret
              (lambda (entry)
                (set-car! entry (c-type->file-type (car entry)))))
            ret)
          ((memq 'catch options)
            '())
          (#t
            (raise-c-errno 'directory-list 'opendir ret dirpath)))))))


(define (%find-and-convert-option options key)
  (let %again ((options options))
    (cond
      ((null? options)
        #vu8())
      ((eq? key (car options))
        (let ((value (if (null? (cdr options)) '() (cadr options))))
          (unless (or (bytevector? value) (string? value) (charspan? value))
            (raise-assertf 'directory-list "expecting a bytevector, string or charspan after option '~a, found ~s"
              key value))
          (text->bytevector value)))
      (#t
        (%again (cdr options))))))


;; in-place sort dir-list, which must have the same structure as the output of (directory-list)
;; i.e. it must be a possibly empty list of pairs (key . value)
;; where all values are either strings or bytevectors - mixtures are not allowed.
(define (directory-sort! dir-list)
  (if (null? dir-list)
    dir-list
    (sort!
      (let ((value (cdar dir-list)))
        (cond
          ((string? value)
            (lambda (entry1 entry2)
              (string<? (cdr entry1) (cdr entry2))))
          ((bytevector? value)
            (lambda (entry1 entry2)
              (bytevector<? (cdr entry1) (cdr entry2))))
          (#t
            (raise-assertf 'directory-sort! "expecting a list of pairs (key . value) where all values are bytevector or string, found value ~s"
              value))))
      dir-list)))

) ; close library
