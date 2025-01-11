;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh posix misc (0 1))
  (export c-hostname c-exit directory-list directory-list*)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) foreign-procedure sort!)
    (only (schemesh bootstrap) catch try)
    (only (schemesh containers) bytevector<? list-iterate string->utf8b)
    (only (schemesh conversions) text->bytevector0)
    (only (schemesh posix fd) raise-c-errno))

(define c-exit (foreign-procedure "c_exit" (int) int))

(define c-hostname
  (let* ((hostname-or-error ((foreign-procedure "c_get_hostname" () ptr)))
         (hostname (if (string? hostname-or-error) hostname-or-error "???")))
    (lambda ()
      hostname)))


;; List contents of a filesystem directory;
;; mandatory first argument dirpath must be a string or bytevector.
;; optional second argument filter-prefix must be a string or bytevector.
;; futher optional arguments can contain:
;;   'bytes - each returned filename will be a bytevector, not a string
;;   'catch - errors be ignored instead of throwing exceptions
;;   'sort  - returned list will be sorted
;;
;; Returns a list of pairs (type . filename) where:
;;  each type is one of: 'unknown 'blockdev 'chardev 'dir 'fifo 'file 'socket 'symlink
;;  each filename is a either a bytevector (if options contain 'bytes) or a string
;;
;; If filter-prefix is not empty, only returns filenames that start with filter-prefix
(define directory-list
  (case-lambda
    ((dirpath)                         (directory-list* dirpath #vu8()        '()))
    ((dirpath filter-prefix)           (directory-list* dirpath filter-prefix '()))
    ((dirpath filter-prefix . options) (directory-list* dirpath filter-prefix options))))


;; implementation of (directory-list)
(define directory-list*
  (let ((c-directory-list (foreign-procedure "c_directory_list" (ptr ptr ptr) ptr))
        (types '#(unknown blockdev chardev dir fifo file socket symlink)))
    (lambda (dirpath filter-prefix options)
      (let* ((strings? (not (memq 'bytes options)))
             (ret (c-directory-list (text->bytevector0 dirpath)
                   (if (bytevector? filter-prefix)
                     filter-prefix
                     (string->utf8b filter-prefix))
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


) ; close library
