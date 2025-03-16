;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh posix replacements (0 8 1))
  (export
      ;; the following functions *intentionally* conflict with R6RS and Chez Scheme
      ;; functions with the same names,
      ;;
      ;; because they are intended as replacements
      ;;
      delete-directory delete-file
      file-directory? file-exists? file-regular? file-symbolic-link?

      get-char get-datum get-line get-string-all get-string-n get-string-some)
  (import
    (rename (except (rnrs) delete-file file-exists?)
                               (get-char        r6rs:get-char)
                               (get-datum       r6rs:get-datum)
                               (get-line        r6rs:get-line)
                               (get-string-all  r6rs:get-string-all)
                               (get-string-n    r6rs:get-string-n))

    (rename (only (chezscheme) foreign-procedure format get-string-some
                               make-continuation-condition make-format-condition sort! void)
                               (get-string-some chez:get-string-some))

    (only (schemesh posix fd)  c-errno->string)
    (only (schemesh posix dir) file-type file-delete))


;;; read and return the next character from textual-input-port,
;;    which defaults to (current-input-port),
;;; or the eof object
(define get-char
  (case-lambda
    (()     (r6rs:get-char (current-input-port)))
    ((port) (r6rs:get-char port))))

;;; skip whitespace and comments from textual-input-port, which defaults to (current-input-port),
;;;    find the start of the external representation of a datum and return it.
;;; On end-of-file, return the eof object.
;;; On unexpected end-of-file while reading a datum, raise a condition with types &lexical and i/o-read.
(define get-datum
  (case-lambda
    (()     (r6rs:get-datum (current-input-port)))
    ((port) (r6rs:get-datum port))))


;;; If textual-input-port, which defaults to (current-input-port), is at end of file, the eof object is returned.
;;; Otherwise, read (as if with get-char) all of the characters available before the port is at end of file
;;; or a line-feed character has been read and returns a string containing all but the line-feed character
;;; of the characters read.
;;; The port's position is advanced past the characters read.
(define get-line
  (case-lambda
    (()     (r6rs:get-line (current-input-port)))
    ((port) (r6rs:get-line port))))


;;; If textual-input-port, which defaults to (current-input-port), is at end of file, the eof object is returned.
;;; Otherwise, read (as if with get-char) all of the characters available before the port is at end of file
;;; and return a string containing these characters.
;;; The port's position is advanced past the characters read.
(define get-string-all
  (case-lambda
    (()     (r6rs:get-string-all (current-input-port)))
    ((port) (r6rs:get-string-all port))))


;;; n must be an exact nonnegative integer.
;;; If textual-input-port, which defaults to (current-input-port), is at end of file, the eof object is returned.
;;; Otherwise, read (as if with get-char) as many characters, up to n, as are available before the port is at end of file,
;;; and returns a new (nonempty) string containing these characters.
;;; The port's position is advanced past the characters read.
(define get-string-n
  (case-lambda
    ((n)      (r6rs:get-string-n (current-input-port) n))
    ((port n) (r6rs:get-string-n port n))))


;;; If textual-input-port, which defaults to (current-input-port), is at end of file, the eof object is returned.
;;; Otherwise, reads (as if with get-char) at least one character and possibly more,
;;; and returns a string containing these characters.
;;;
;;; The port's position is advanced past the characters read.
;;;
;;; The maximum number of characters read by this operation is implementation-dependent.
;;;
;;; An exception to the "at least one character" guarantee occurs if the port is in nonblocking mode
;;; (see set-port-nonblocking!) and no input is ready. In this case, an empty string is returned.
(define get-string-some
  (case-lambda
    (()     (chez:get-string-some (current-input-port)))
    ((port) (chez:get-string-some port))))




(define c-errno-enotdir ((foreign-procedure "c_errno_enotdir" () int)))


;; Delete a file or directory.
;; Intentionally shadows R6RS and Chez Scheme (delete-file) ad provides compatible API and semantics,
;; adding support for non-UTF-8 paths.
;;
;; Mandatory first argument path must be a string, bytevector, bytespan or charspan.
;; Further optional argument can contain:
;;   truish - if operation fails, raise an &i/o-filename exception
;;   #f     - (default) if operation fails, return #f without raising exceptions
;;
;; If filesystem path exists, return #t.
;; Otherwise return #f or raise exception (depending on optional argument, see above).
(define delete-file
  (case-lambda
    ((path error?)
      (let ((err (file-delete path '(catch))))
        (cond
          ((eq? (void) err) #t)
          (error?           (%raise-io-filename-error 'delete-file path err))
          (else             #f))))
    ((path)
      (delete-file path #f))))


;; Delete a directory.
;; Intentionally shadows Chez Scheme (delete-directory) ad provides compatible API and semantics,
;; adding support for non-UTF-8 paths.
;;
;; Mandatory first argument path must be a string, bytevector, bytespan or charspan.
;; Further optional argument can contain:
;;   truish - if operation fails, raise an &i/o-filename exception
;;   #f     - (default) if operation fails, return #f without raising exceptions
;;
;; If filesystem path exists, return #t.
;; Otherwise return #f or raise exception (depending on optional argument, see above).
(define delete-directory
  (case-lambda
    ((path error?)
      (let ((err (if (eq? 'dir (file-type path '(catch symlinks)))
                   (file-delete path '(catch))
                   c-errno-enotdir)))
        (cond
          ((eq? (void) err) #t)
          (error?           (%raise-io-filename-error 'delete-directory path err))
          (else             #f))))
    ((path)
      (delete-directory path #f))))


(define (%raise-io-filename-error who path err)
  (call/cc
    (lambda (k)
      (raise
        (condition
          (make-i/o-filename-error path)
          (make-who-condition (if (symbol? who) who (format #f "~s" who)))
          (make-format-condition)
          (make-message-condition "failed for ~a: ~a")
          (make-irritants-condition (list path (c-errno->string err)))
          (make-continuation-condition k))))))


;; Check existence of a filesystem path.
;;
;; Intentionally shadows Chez Scheme (file-exists?) ad provides compatible API and semantics,
;; adding support for non-UTF-8 paths.
;;
;; Mandatory first argument path must be a string, bytevector, bytespan or charspan.
;; Further optional argument can contain:
;;   truish - (default) follow symbolic links
;;   #f     - don't follow symbolic links
;;
;; If filesystem path exists, return #t. Otherwise return #f.
(define file-exists?
  (case-lambda
    ((path)
      (symbol? (file-type path '(catch))))
    ((path follow?)
      (symbol? (if follow?
                 (file-type path '(catch))
                 (file-type path '(catch symlinks)))))))


;; Check if a filesystem path is a directory.
;;
;; Intentionally shadows Chez Scheme (file-directory?) ad provides compatible API and semantics,
;; adding support for non-UTF-8 paths.
;;
;; Mandatory first argument path must be a string, bytevector, bytespan or charspan.
;; Further optional argument can contain:
;;   truish - (default) follow symbolic links
;;   #f     - don't follow symbolic links
;;
;; If filesystem path exists and is a directory, return #t. Otherwise return #f.
(define file-directory?
  (case-lambda
    ((path)
      (eq? 'dir (file-type path '(catch))))
    ((path follow?)
      (eq? 'dir (if follow?
                  (file-type path '(catch))
                  (file-type path '(catch symlinks)))))))


;; Check if a filesystem path is a regular file.
;;
;; Intentionally shadows R6R6 and Chez Scheme (file-regular?) ad provides compatible API and semantics,
;; adding support for non-UTF-8 paths.
;;
;; Mandatory first argument path must be a bytevector, string, bytespan or charspan.
;; Further optional argument can contain:
;;   truish - (default) follow symbolic links
;;   #f     - don't follow symbolic links
;;
;; If filesystem path exists and is a regular file, return #t. Otherwise return #f.
(define file-regular?
  (case-lambda
    ((path)
      (eq? 'file (file-type path '(catch))))
    ((path follow?)
      (eq? 'file (if follow?
                   (file-type path '(catch))
                   (file-type path '(catch symlinks)))))))


;; Check if a filesystem path is a symbolic link.
;;
;; Intentionally shadows Chez Scheme (file-symbolic-link?) ad provides compatible API and semantics,
;; adding support for non-UTF-8 paths.
;;
;; Mandatory first argument path must be a bytevector, string or charspan.
;; Does not accept optional arguments. Never follows symbolic links (obviously).
;;
;; If filesystem path exists and is a symbolic link, return #t. Otherwise return #f.
(define (file-symbolic-link? path)
  (eq? 'symlink (file-type path '(catch symlinks))))


) ; close library
