;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file posix/fs.ss

;; info about a filesystem entry: a file, dir, socket, pipe, symlink...
(define-record-type (dir-entry %make-dir-entry dir-entry?)
  (fields
    (mutable name)     ; string
    (mutable type)     ; (void) or symbol
    (mutable size)     ; (void) or size in bytes
    (mutable link)     ; (void) or #f or string: symlink target
    (mutable depth)    ; (void) or fs-reader depth
    (mutable modified) ; (void) or time-utc
    (mutable accessed) ; (void) or time-utc
    (mutable status-changed) ; (void) or time-utc
    (mutable mode)     ; (void) or POSIX permission string like "rwxr-xr--SST"
    (mutable user)     ; (void) or immutable string
    (mutable group)    ; (void) or immutable string
    (mutable uid)      ; (void) or exact integer
    (mutable gid)      ; (void) or exact integer
    (mutable dev)      ; (void) or exact integer
    (mutable rdev)     ; (void) or exact integer
    (mutable inode)    ; (void) or exact integer
    (mutable nlink))   ; (void) or exact integer
  (nongenerative %dir-entry-7c46d04b-34f4-4046-b5c7-b63753c1be43))


(define (exact-integer-or-void? obj)
  (or (eq? (void) obj) (and (integer? obj) (exact? obj))))

(define (string-or-symbol-or-void? obj)
  (or (eq? (void) obj) (symbol? obj) (string? obj)))

(define (string-or-void? obj)
  (or (eq? (void) obj) (string? obj)))

(define (time-or-void? obj)
  (or (eq? (void) obj) (time? obj)))


(define (make-dir-entry name type size link depth modified accessed status-changed mode user group uid gid dev rdev inode nlink)
  (assert* 'make-dir-entry (string? name))
  (assert* 'make-dir-entry (string-or-symbol-or-void? type))
  (assert* 'make-dir-entry (exact-integer-or-void? size))
  (assert* 'make-dir-entry (string-or-void? link))
  (assert* 'make-dir-entry (exact-integer-or-void? depth))
  (assert* 'make-dir-entry (time-or-void? modified))
  (assert* 'make-dir-entry (time-or-void? accessed))
  (assert* 'make-dir-entry (time-or-void? status-changed))
  (assert* 'make-dir-entry (string-or-void? mode))
  (assert* 'make-dir-entry (string-or-void? user))
  (assert* 'make-dir-entry (string-or-void? group))
  (assert* 'make-dir-entry (exact-integer-or-void? uid))
  (assert* 'make-dir-entry (exact-integer-or-void? gid))
  (assert* 'make-dir-entry (exact-integer-or-void? dev))
  (assert* 'make-dir-entry (exact-integer-or-void? rdev))
  (assert* 'make-dir-entry (exact-integer-or-void? inode))
  (assert* 'make-dir-entry (exact-integer-or-void? nlink))
  (let ((type (if (string? type) (string->symbol type) type)))
    (%make-dir-entry name type size link depth modified accessed status-changed mode user group uid gid dev rdev inode nlink)))


(define (make-dir-entry-vector)
  (make-vector 14 (void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; read a directory: streaming API

(define-record-type (dir-reader %make-dir-reader dir-reader?)
  (parent reader)
  (fields
    (mutable handle)    ; #f or integer containing C DIR*
    vec                 ; vector, used as buffer for C function c_dir_get()
    (mutable uid-cache) ; #f or eqv-hashtable uid -> user name
    (mutable gid-cache) ; #f or eqv-hashtable gid -> group name
    path                ; directory being read
    opts)               ; fixnum, bitwise-or of: 1 = catch, 2 = dir-path-as-prefix, 4 = dir-hide-dot-files, 8 = dir-hide-dot-dotdot
  (protocol
    (lambda (args->new)
      (lambda (handle path options)
        ((args->new %dir-reader-get %dir-reader-skip %dir-reader-close)
          handle (make-dir-entry-vector) #f #f path options))))
  (nongenerative %dir-reader-7c46d04b-34f4-4046-b5c7-b63753c1be42))


(define (dir-catch-errors? options)
  (fxlogbit? 0 options))

(define (dir-path-as-prefix? options)
  (fxlogbit? 1 options))

(define (dir-hide-dot-files? options)
  (fxlogbit? 2 options))

(define (dir-hide-dot-dotdot? options)
  (fxlogbit? 3 options))



(define-syntax dir-reader-option
  (lambda (stx)
    (syntax-case stx ()
      ((_ option)
        (let ((sym (syntax->datum #'option)))
          (case sym
            ((catch)               1)
            ((dir-path-as-prefix)  2)
            ((dir-hide-dot-files)  4)
            ((dir-hide-dot-dotdot) 8)
            (else
              (raise-errorf 'dir-reader-options "invalid syntax: (dir-reader-options ~s)" sym))))))))


(define-syntax dir-reader-options
  (syntax-rules ()
    ((_)
      0)
    ((_ option1 option2 ...)
      (fxior (dir-reader-option option1) (dir-reader-option option2) ...))))


(define make-dir-reader
  (let ((c-dir-open (foreign-procedure "c_dir_open" (ptr) ptr)))
    (case-lambda
      ((path options)
        ;; (debugf ">make-dir-reader path ~s, options ~s" path options)
        (assert* 'make-dir-reader (fixnum? options))
        (assert* 'make-dir-reader (fx<=? 0 options 15))
        (let ((obj (c-dir-open (text->bytevector0 path))))
          (cond
            ((and (integer? obj) (exact? obj) (> obj 0))
              (%make-dir-reader obj (text->string path) options))
            ((dir-catch-errors? options)
              obj)
            (else
              (raise-c-errno 'make-dir-reader 'opendir obj path)))))
      ((path)
        (make-dir-reader path (dir-reader-options))))))



;; called by (reader-get)
(define %dir-reader-get
  (let ((c-dir-get       (foreign-procedure "c_dir_get" (void* ptr unsigned) int))
        (c-errno-eaccess ((foreign-procedure "c_errno_eaccess" () int)))
        (c-errno-eperm   ((foreign-procedure "c_errno_eperm" () int))))
    (lambda (rx)
      (let ((handle (dir-reader-handle rx)))
        (if handle
          (let ((vec (dir-reader-vec rx))
                (flags (cond ((dir-hide-dot-files?  (dir-reader-opts rx)) #x3fff)
                             ((dir-hide-dot-dotdot? (dir-reader-opts rx)) #x7fff)
                             (else                                        #xffff))))
            (vector-fill! vec (void))
            (let ((err (c-dir-get handle vec flags)))
              (cond
                ((and (fixnum? err) (fxzero? err))
                  (values #f #f)) ;; dir-reader is exhausted
                ((and (fixnum? err) (fx>? err 0))
                  (let ((ret (vector->dir-entry rx vec)))
                    (if ret
                      ;; return this dir entry
                      (values ret #t)
                      ;; skip this dir entry, it starts with a dot
                      (%dir-reader-get rx))))
                ((or (eqv? c-errno-eaccess err) (eqv? c-errno-eperm err))
                  (warn-c-errno 'dir-reader-get 'readdir err (dir-reader-path rx))
                  ;; directory is unreadable, reading it further produces an infinite loop
                  (values #f #f))
                (else
                  (raise-c-errno 'dir-reader-get 'readdir err (dir-reader-path rx))))))
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
    (make-dir-entry
      (if (and rx (dir-path-as-prefix? (dir-reader-opts rx)))
        (path-append (dir-reader-path rx) name)
        name)                                   ; name
      (if-fixnum->type   (vector-ref vec 1))    ; type
      (vector-ref vec 2)                        ; size
      (or (vector-ref vec 3) "")                ; symlink target
      (void)                                    ; depth
      (if-pair->time-utc (vector-ref vec 4))    ; modified
      (if-pair->time-utc (vector-ref vec 6))    ; accessed
      (if-pair->time-utc (vector-ref vec 6))    ; status-changed
      (if-mode->string   (vector-ref vec 7))    ; mode
      (if-uid->username  rx (vector-ref vec 8)) ; username
      (if-gid->groupname rx (vector-ref vec 9)) ; groupname
      (vector-ref vec 8)      ; uid
      (vector-ref vec 9)      ; gid
      (vector-ref vec 10)     ; dev
      (vector-ref vec 11)     ; rdev
      (vector-ref vec 12)     ; inode
      (vector-ref vec 13)))) ; nlink


;; only convert string->symbol the field dir-entry-type
(define (deserialize-dir-entry plist)
  (plist-update! plist 'type (lambda (val)
                               (if (string? val)
                                 (string->symbol val)
                                 val)))
  plist)


(define (write-c-make-dev dev port writer)
  (cond
    ((and (number? dev) (exact? dev))
      (put-string port "(c-make-dev ") (writer (c-dev-major dev) port)
      (put-char port #\space)          (writer (c-dev-minor dev) port)
      (put-string port ")"))
    (else
      (writer dev port))))

