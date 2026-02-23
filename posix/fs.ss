;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k posix fs (0 9 3))
  (export
      make-dir-entry  dir-entry  dir-entry?  dir-entry-type
      make-dir-reader dir-reader dir-reader? dir-reader-options dir-reader-path

      directory-list directory-list-type directory-sort!
      file-delete file-rename file-stat file-type mkdir
      gid->groupname uid->username)
  (import
    (rename (rnrs)                         (fxarithmetic-shift-right fx>>))
    (rnrs mutable-pairs)
    (rnrs mutable-strings)
    (only (chezscheme)                     foreign-procedure fx1+ fxlogbit? make-continuation-condition
                                           make-format-condition record-writer sort! string->immutable-string time? void)
    (only (scheme2k bootstrap)             assert* catch raise-assertf raise-errorf try)
    (only (scheme2k containers bytevector) bytevector<?)
    (only (scheme2k containers charspan)   charspan?)
    (only (scheme2k containers list)       for-list)
    (only (scheme2k containers string)     string-prefix? string-suffix?)
    (only (scheme2k containers time)       make-time-utc)
    (only (scheme2k containers utf8b)      string->utf8b)
    (only (scheme2k conversions)           text->bytevector text->bytevector0 text->string)
    (only (scheme2k io obj)                reader reader-get reader-eof? reader-close reader-skip)
    (only (scheme2k posix fd)              c-errno->string raise-c-errno)
    (only (scheme2k reflect)               reflect-info-set-autodetect!))


;; info about a filesystem entry: a file, dir, socket, pipe, symlink...
(define-record-type (dir-entry %make-dir-entry dir-entry?)
  (fields
    (mutable name)     ; string
    (mutable type)     ; (void) or symbol
    (mutable size)     ; (void) or size in bytes
    (mutable link)     ; (void) or #f or string: symlink target
    (mutable modified) ; (void) or time-utc
    (mutable accessed) ; (void) or time-utc
    (mutable inode-changed) ; (void) or time-utc
    (mutable mode)     ; (void) or POSIX permission string like "rwxr-xr--SST"
    (mutable user)     ; (void) or immutable string
    (mutable group)    ; (void) or immutable string
    (mutable uid)      ; (void) or exact integer
    (mutable gid)      ; (void) or exact integer
    (mutable inode)    ; (void) or exact integer
    (mutable nlink))   ; (void) or exact integer
  (nongenerative %dir-entry-7c46d04b-34f4-4046-b5c7-b63753c1be41))


(define (exact-integer-or-void? obj)
  (or (eq? (void) obj) (and (integer? obj) (exact? obj))))

(define (string-or-symbol-or-void? obj)
  (or (eq? (void) obj) (symbol? obj) (string? obj)))

(define (string-or-void? obj)
  (or (eq? (void) obj) (string? obj)))

(define (time-or-void? obj)
  (or (eq? (void) obj) (time? obj)))


(define (make-dir-entry name type size link modified accessed inode-changed mode user group uid gid inode nlink)
  (assert* 'make-dir-entry (string? name))
  (assert* 'make-dir-entry (string-or-symbol-or-void? type))
  (assert* 'make-dir-entry (exact-integer-or-void? size))
  (assert* 'make-dir-entry (string-or-void? link))
  (assert* 'make-dir-entry (time-or-void? modified))
  (assert* 'make-dir-entry (time-or-void? accessed))
  (assert* 'make-dir-entry (time-or-void? inode-changed))
  (assert* 'make-dir-entry (string-or-void? mode))
  (assert* 'make-dir-entry (string-or-void? user))
  (assert* 'make-dir-entry (string-or-void? group))
  (assert* 'make-dir-entry (exact-integer-or-void? uid))
  (assert* 'make-dir-entry (exact-integer-or-void? gid))
  (assert* 'make-dir-entry (exact-integer-or-void? inode))
  (assert* 'make-dir-entry (exact-integer-or-void? nlink))
  (let ((type (if (string? type) (string->symbol type) type)))
    (%make-dir-entry name type size link modified accessed inode-changed mode user group uid gid inode nlink)))


(define (make-dir-entry-vector)
  (make-vector 12 (void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; streaming API

(define-record-type (dir-reader %make-dir-reader dir-reader?)
  (parent reader)
  (fields
    (mutable handle)    ; #f or integer containing C DIR*
    vec                 ; vector, used as buffer for C function c_dir_get()
    (mutable uid-cache) ; #f or eqv-hashtable uid -> user name
    (mutable gid-cache) ; #f or eqv-hashtable gid -> group name
    path                ; directory being read
    opts)               ; fixnum, bitwise-or of: 1 = dir-hide-dot-files, 2 = dir-path-as-prefix
  (protocol
    (lambda (args->new)
      (lambda (handle path options)
        ((args->new %dir-reader-get %dir-reader-skip %dir-reader-close)
          handle (make-dir-entry-vector) #f #f path options))))
  (nongenerative %dir-reader-7c46d04b-34f4-4046-b5c7-b63753c1be42))


(define (dir-hide-dot-files? options)
  (fxlogbit? 0 options))


(define (dir-path-as-prefix? options)
  (fxlogbit? 1 options))


(define-syntax dir-reader-option
  (lambda (stx)
    (syntax-case stx ()
      ((_ option)
        (let ((sym (syntax->datum #'option)))
          (case sym
            ((dir-hide-dot-files) 1)
            ((dir-path-as-prefix) 2)
            (else
              (raise-errorf 'dir-reader-options "invalid syntax: (dir-reader-options ~s)" sym))))))))


(define-syntax dir-reader-options
  (syntax-rules ()
    ((_)
       0)
    ((_ option1)
      (dir-reader-option option1))
    ((_ option1 option2 ...)
      (fxior (dir-reader-option option1) (dir-reader-options option2 ...)))))


(define make-dir-reader
  (let ((c-dir-open (foreign-procedure "c_dir_open" (ptr) ptr)))
    (case-lambda
      ((path options)
        ;; (debugf ">make-dir-reader path ~s, options ~s" path options)
        (assert* 'make-dir-reader (fixnum? options))
        (assert* 'make-dir-reader (fx<=? 0 options 3))
        (let ((obj (c-dir-open (text->bytevector0 path))))
          (unless (and (integer? obj) (exact? obj) (> obj 0))
            (raise-c-errno 'make-dir-reader 'opendir obj path))
          (%make-dir-reader obj (text->string path) options)))
      ((path)
        (make-dir-reader path (dir-reader-options))))))



;; called by (reader-get)
(define %dir-reader-get
  (let ((c-dir-get (foreign-procedure "c_dir_get" (void* ptr unsigned) int)))
    (lambda (rx)
      (let ((handle (dir-reader-handle rx)))
        (if handle
          (let ((vec (dir-reader-vec rx)))
            (vector-fill! vec (void))
            (let ((err (c-dir-get handle vec #x3fff)))
              (unless (and (fixnum? err) (fx>=? err 0))
                (raise-c-errno 'dir-reader-get 'readdir err handle))
              (if (fx<=? err 0)
                (values #f #f) ;; dir-reader is exhausted
                (let ((ret (vector->dir-entry rx vec)))
                  (if ret
                    ;; return this dir entry
                    (values ret #t)
                    ;; skip this dir entry, it starts with a dot
                    (%dir-reader-get rx))))))
          (values #f #f)))))) ;; dir-reader is closed


;; called by (reader-skip)
(define %dir-reader-skip
  (let ((c-dir-skip (foreign-procedure "c_dir_skip" (void*) int)))
    (lambda (rx)
      (let ((handle (dir-reader-handle rx)))
        (if handle
          (let ((err (c-dir-skip handle)))
            (unless (and (fixnum? err) (fx>=? err 0))
              (raise-c-errno 'dir-reader-skip 'readdir err handle))
            (fx>? err 0))
          #f))))) ;; dir-reader is closed


;; called by (reader-close)
(define %dir-reader-close
  (let ((c-dir-close (foreign-procedure "c_dir_close" (void*) void)))
    (lambda (rx)
      (let ((handle (dir-reader-handle rx)))
        (when handle
          (dir-reader-handle-set! rx #f)
          (c-dir-close handle))))))


(define if-fixnum->type
  (let ((types '#(#f fifo char-device dir block-device file symlink socket)))
    (lambda (obj)
      (if (and (fixnum? obj) (fx<=? 1 obj 7))
        (vector-ref types obj)
        (void)))))


(define if-mode->string
  (let ((fullstr "rwxrwxrwxSST"))
    (lambda (mode)
      (if (fixnum? mode)
        (let ((n (if (fx<=? mode #o777) 9 12)))
          ;; (debugf "if-mode->string ~o" mode)
          (do ((str (make-string n #\-))
               (i 0 (fx1+ i)))
              ((fx>=? i n) str)
            (let ((bit (if (fx<? i 9)
                         (fx>> #o400 i)
                         (fx>> #o4000 (fx- i 9)))))
              (unless (fxzero? (fxand bit mode))
                (string-set! str i (string-ref fullstr i))))))
        (void)))))


(define (if-pair->time-utc obj)
  (if (pair? obj)
    (make-time-utc (car obj) (cdr obj))
    (void)))


(define (if-uid->username rx uid)
  (cond
    ((not (and (integer? uid) (exact? uid)))
      ;; no uid => no username
      (void))
    ((not rx)
      ;; no rx => convert uid to username without any cache
      (uid->username uid))
    (else
      ;; use short-lived cache uid -> username stored in dir-reader
      ;; reduces syscall clutter
      (let ((cache (or (dir-reader-uid-cache rx)
                       (let ((ht (make-eqv-hashtable)))
                         (dir-reader-uid-cache-set! rx ht)
                         ht))))
        (or (hashtable-ref cache uid #f)
            (let* ((xname (uid->username uid))
                   (name  (if (string? xname) (string->immutable-string xname) (void))))
              (hashtable-set! cache uid name) ;; also cache lookup failures
              name))))))


(define (if-gid->groupname rx gid)
  (cond
    ((not (and (integer? gid) (exact? gid)))
      ;; no gid => no groupname
      (void))
    ((not rx)
      ;; no rx => convert gid to groupname without any cache
      (gid->groupname gid))
    (else
      ;; use short-lived cache gid -> groupname stored in dir-reader
      ;; reduces syscall clutter
      (let ((cache (or (dir-reader-gid-cache rx)
                       (let ((ht (make-eqv-hashtable)))
                         (dir-reader-gid-cache-set! rx ht)
                         ht))))
        (or (hashtable-ref cache gid #f)
            (let* ((xname (gid->groupname gid))
                   (name  (if (string? xname) (string->immutable-string xname) (void))))
              (hashtable-set! cache gid name) ;; also cache lookup failures
              name))))))


(define (path-append a b)
  (if (string-suffix? a "/")
    (string-append a b)
    (string-append a "/" b)))


(define (vector->dir-entry rx vec)
  (let ((name (vector-ref vec 0)))
    (if (and rx
             (dir-hide-dot-files? (dir-reader-opts rx))
             (string-prefix? name "."))
      #f ; skip this dir entry, it starts with a dot
      (make-dir-entry
        (if (and rx (dir-path-as-prefix? (dir-reader-opts rx)))
          (path-append (dir-reader-path rx) name)
          name)
        (if-fixnum->type   (vector-ref vec 1))
        (vector-ref vec 2)
        (or (vector-ref vec 3) "") ; symlink target
        (if-pair->time-utc (vector-ref vec 4)) ; modified
        (if-pair->time-utc (vector-ref vec 6)) ; accessed
        (if-pair->time-utc (vector-ref vec 6)) ; inode-changed
        (if-mode->string   (vector-ref vec 7))
        (if-uid->username  rx (vector-ref vec 8))
        (if-gid->groupname rx (vector-ref vec 9))
        (vector-ref vec 8)
        (vector-ref vec 9)
        (vector-ref vec 10)
        (vector-ref vec 11)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; low-level API

(define c-errno-einval ((foreign-procedure "c_errno_einval" () int))) ;; integer, not a procedure


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
               (err       (c-file-stat (text->bytevector0 path) (if symlinks? 1 0) vec)))
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


;; customize how "dir-reader" objects are printed
(record-writer (record-type-descriptor dir-reader)
  (lambda (rx port writer)
    (put-string port "#<dir-reader")
    (put-string port (if (reader-eof? rx) " eof " " ok "))
    (writer (dir-reader-path rx) port)
    (put-string port ">")))


;; customize how "dir-entry" objects are printed
(record-writer (record-type-descriptor dir-entry)
  (let ((accessors (vector dir-entry-name     dir-entry-type     dir-entry-size    dir-entry-link
                           dir-entry-modified dir-entry-accessed dir-entry-inode-changed
                           dir-entry-mode     dir-entry-user     dir-entry-group   dir-entry-uid
                           dir-entry-gid      dir-entry-inode    dir-entry-nlink)))
    (lambda (e port writer)
      (put-string port "(make-dir-entry")
      (do ((i 0 (fx1+ i))
           (n (vector-length accessors)))
          ((fx>=? i n) (put-string port ")"))
        (put-char port #\space)
        (writer ((vector-ref accessors i) e) port)))))


;; customize visible reflect fields and deserializer for `dir-entry` objects
(reflect-info-set-autodetect! (record-type-descriptor dir-entry) make-dir-entry)


) ; close library
