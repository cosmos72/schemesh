;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh posix misc (0 1))
  (export c-hostname c-exit directory-list)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) foreign-procedure sort!)
    (only (schemesh bootstrap) catch raise-assertf try)
    (only (schemesh containers) bytevector<? charspan? list-iterate string->utf8b)
    (only (schemesh conversions) text->bytevector text->bytevector0)
    (only (schemesh posix fd) raise-c-errno))

(define c-exit (foreign-procedure "c_exit" (int) int))

(define c-hostname
  (let* ((hostname-or-error ((foreign-procedure "c_get_hostname" () ptr)))
         (hostname (if (string? hostname-or-error) hostname-or-error "???")))
    (lambda ()
      hostname)))


;; List contents of a filesystem directory;
;; mandatory first argument dirpath must be a bytevector, string or charspan.
;; futher optional arguments can contain:
;;   'bytes - each returned filename will be a bytevector, not a string
;;   'catch - errors be ignored instead of raising a condition
;;   'sort  - returned list will be sorted
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
  (let ((c-directory-list (foreign-procedure "c_directory_list" (ptr ptr ptr ptr ptr) ptr))
        (types '#(unknown blockdev chardev dir fifo file socket symlink)))
    (lambda (dirpath . options)
      (let* ((strings? (not (memq 'bytes options)))
             (ret (c-directory-list
                    (text->bytevector0 dirpath)
                    (%find-and-convert-option options 'prefix)
                    (%find-and-convert-option options 'suffix)
                    (memq 'symlinks options)
                    strings?)))
        (cond
          ((or (null? ret) (pair? ret))
            (list-iterate ret
              (lambda (entry)
                (let ((c-type (car entry)))
                  (set-car! entry
                    (if (fx<=? 0 c-type 7) (vector-ref types c-type) 'unknown)))))
            (if (memq 'sort options)
              (sort!
                (if strings?
                  (lambda (entry1 entry2)
                    (string<? (cdr entry1) (cdr entry2)))
                  (lambda (entry1 entry2)
                    (bytevector<? (cdr entry1) (cdr entry2))))
                ret)
              ret))
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


) ; close library
