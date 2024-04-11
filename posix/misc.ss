;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh posix misc (0 1))
  (export c-hostname c-exit directory-u8-list)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) foreign-procedure sort!)
    (only (schemesh containers) bytevector<? list-iterate string->utf8b)
    (only (schemesh conversions) text->bytevector0)
    (only (schemesh posix fd) raise-errno-condition))

(define c-exit (foreign-procedure "c_exit" (int) int))

(define c-hostname
  (let* ((hostname-or-error ((foreign-procedure "c_get_hostname" () scheme-object)))
         (hostname (if (string? hostname-or-error) hostname-or-error "???")))
    (lambda ()
      hostname)))

; implementation of (directory-u8-list)
(define %directory-u8-list
  (let ((c-directory-u8-list (foreign-procedure "c_directory_u8_list"
                     (scheme-object scheme-object) scheme-object))
        (types '#(unknown blockdev chardev dir fifo file socket symlink)))
    (lambda (dirpath filter-prefix)
      (let ((ret (c-directory-u8-list (text->bytevector0 dirpath)
                   (if (bytevector? filter-prefix)
                     filter-prefix
                     (string->utf8b filter-prefix)))))
        (unless (or (null? ret) (pair? ret))
          (raise-errno-condition 'directory-u8-list ret))
        (list-iterate ret
          (lambda (entry)
            (let ((c-type (car entry)))
              (set-car! entry
                (if (fx<=? 0 c-type 7) (vector-ref types c-type) 'unknown)))))
        (sort!
          (lambda (entry1 entry2)
            (bytevector<? (cdr entry1) (cdr entry2)))
          ret)))))


; List contents of a filesystem directory;
; mandatory first argument dirpath must be a string or bytevector.
; optional second argument filter-prefix must be a string or bytevector.
;
; Returns a sorted list of pairs (type . filename) where filename is a bytevector
; (because Unix filenames can contain arbitrary bytes, not just UTF-8)
; and type is one of: 'unknown 'blockdev 'chardev 'dir 'fifo 'file 'socket 'symlink
;
; If filter-prefix is not empty, only returns filenames that start with filter-prefix
(define directory-u8-list
  (case-lambda
    ((dirpath)               (%directory-u8-list dirpath #vu8()))
    ((dirpath filter-prefix) (%directory-u8-list dirpath filter-prefix))))

) ; close library
