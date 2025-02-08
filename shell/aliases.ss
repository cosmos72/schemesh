;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file job.ss


;; find and return the definition of specified alias name,
;; or #f if not found
(define (sh-alias-ref name)
  (hashtable-ref (sh-aliases) name #f))


;; given a command line prog-and-args i.e. a list of strings,
;; extract the first string and expand the corresponding alias.
;; Return the list of strings produced by alias expansion.
;; Return the unmodified prog-and-args if no corresponding alias is found.
;;
;; Note: aliases are expanded recursively, i.e. if the expansion produced by an alias
;; starts with an alias name, it is expanded again.
;; To avoid infinite recursion, each alias name is only expanded at most once.
;;
;; Note: for sanity, (sh-aliases-expand) ignores aliases for "builtin"
(define (sh-aliases-expand prog-and-args)
  (assert-string-list? 'sh-aliases-expand prog-and-args)
  (aliases-expand prog-and-args '("builtin"))) ; suppress alias expansion for "builtin"


(define (aliases-expand prog-and-args suppressed-name-list)
  (if (null? prog-and-args)
    prog-and-args
    (let* ((name  (car prog-and-args))
           (alias (sh-alias-ref name)))
      ;; try to recursively expand output of alias expansion,
      ;; but suppress expansion of already-expanded names
      (if (and alias (not (member name suppressed-name-list)))
        (let ((expanded (append alias (cdr prog-and-args))))
          (alias-validate 'sh-aliases-expand name alias)
          (aliases-expand expanded (cons name suppressed-name-list)))

        ;; name is not an alias, or is in suppressed-name-list:
        ;; stop recursion, just return prog-and-args
        prog-and-args))))


(define (alias-validate caller name alias)
  (assert-string-list? caller alias)
  (when (null? alias)
    (raise-errorf caller "an alias cannot be empty.\n\tReason: it would be functionally equivalent to \"unsafe\" builtin.\n\tFound: ~s"
      (list "alias" name)))
  (when (string-list-starts-with-unsafe? alias)
    (raise-errorf caller "an alias cannot expand to \"unsafe\".\n\tReason: it would allow hiding the \"unsafe\" builtin.\n\tFound: ~s"
      (cons "alias" (cons name alias)))))


(define (string-list-starts-with-unsafe? l)
  (and (not (null? l))
       (string=? "unsafe" (car l))))


;; add an alias to (sh-aliases) table.
;; name must be a string; alias must be a list of strings.
;; command line (cons name args) will be expanded to (append alias args)
;;
;; do NOT modify alias after calling this function.
(define (sh-alias-set! name alias)
  (alias-validate 'sh-alias-set! name alias)
  (when (hashtable-ref (sh-builtins) name #f)
    (write-builtin-warning
      (string-append "\"" name
        "\" is a builtin. defining an alias with the same name is allowed, but probably confusing")))
  (hashtable-set! (sh-aliases) name alias))


;; remove an alias from (sh-aliases) table.
(define (sh-alias-delete! name)
  (hashtable-delete! (sh-aliases) name))


;; the "alias" builtin: show all aliases, or show a single alias, or set an alias.
;;
;; As all builtins do, must return job status.
(define (builtin-alias job prog-and-args options)
  ; (debugf "builtin-alias ~s" prog-and-args)
  (assert-string-list? 'builtin-alias prog-and-args)
  (cond
   ((or (null? prog-and-args) (null? (cdr prog-and-args)))
     (show-aliases))
   ((null? (cddr prog-and-args))
     (show-alias (cadr prog-and-args)))
   (#t
     (sh-alias-set! (cadr prog-and-args) (cddr prog-and-args))
     (void))))


;; the "unalias" builtin: unset zero or more aliases.
;;
;; As all builtins do, must return job status.
(define (builtin-unalias job prog-and-args options)
  (assert-string-list? 'builtin-unalias prog-and-args)
  (do ((tail (cdr prog-and-args) (cdr tail)))
      ((null? tail) (void))
    (sh-alias-delete! (car tail))))


;; function returning the global hashtable name -> alias
;; Each alias is a function args -> prog-and-args
;; i.e. it must accept a list of strings and return a list of strings
(define sh-aliases
  (let ((ht (make-hashtable string-hash string=?)))
    ; initial aliases
    (hashtable-set! ht "ls" '("ls" "--color=auto"))
    (hashtable-set! ht "l"  '("ls" "-al"))
    (hashtable-set! ht "v"  '("ls" "-l"))
    (lambda () ht)))



(define (show-aliases)
  (let ((wbuf    (make-bytespan 0))
        (aliases (span))
        (fd      (sh-fd-stdout)))
    (hashtable-iterate (sh-aliases)
      (lambda (cell)
        (span-insert-back! aliases cell)))
    (span-sort! (lambda (cell1 cell2) (string<? (car cell1) (car cell2))) aliases)
    (span-iterate aliases
      (lambda (i cell)
        (show-alias* (car cell) (cdr cell) wbuf)
        (when (fx>=? (bytespan-length wbuf) 4096)
          (fd-write/bspan! fd wbuf))))
    (fd-write/bspan! fd wbuf)
    (void))) ; return (void), means builtin exited successfully


(define (show-alias name)
  (let ((alias (sh-alias-ref name)))
    (if alias
      (let ((wbuf  (make-bytespan 0)))
        (show-alias* name alias wbuf)
        (fd-write/bspan! (sh-fd-stdout) wbuf)
        (void)                                           ; success, return (void)
      (write-builtin-error "alias" name "not found"))))) ; error, return '(exited . 1)


(define (show-alias* name alias wbuf)
  (bytespan-insert-back/string! wbuf "alias ")
  (bytespan-insert-back/string! wbuf name)
  (cond
    ((procedure? alias)
      (bytespan-insert-back/string! wbuf " #<procedure>"))
    ((list? alias)
      (list-iterate alias
        (lambda (elem)
          (if (string? elem)
            (begin
              (bytespan-insert-back/u8! wbuf 32 39) ; #\space #\'
              (bytespan-insert-back/string! wbuf elem)
              (bytespan-insert-back/u8! wbuf 39))   ; #\'
            (bytespan-insert-back/string! wbuf  elem " #<bad-value>")))))
    (#t
      (bytespan-insert-back/string! wbuf " #<bad-value>")))
  (bytespan-insert-back/u8! wbuf 10)) ; #\newline
