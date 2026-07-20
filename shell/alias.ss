;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file job.ss


(define (alias-apply alias args)
  (if (procedure? alias)
    (alias args)
    (append alias args)))


(define (alias-expand prog-and-args suppressed-name-list)
  (assert-string-list? 'sh-alias-expand prog-and-args)
  (if (null? prog-and-args)
    prog-and-args
    (let* ((name  (car prog-and-args))
           (alias (sh-alias name)))
      ;; try to recursively expand output of alias expansion,
      ;; but suppress expansion of already-expanded names
      (if (and alias (not (member name suppressed-name-list)))
        (let ((expanded (alias-apply alias (cdr prog-and-args))))
          (alias-expand expanded (cons name suppressed-name-list)))

        ;; name is not an alias, or is in suppressed-name-list:
        ;; stop recursion, just return prog-and-args
        prog-and-args))))


;; return known aliases, or a single known alias, or insert/delete an alias
(define sh-alias
  (let ((ht (make-hashtable string-hash string=?)))
    ; initial aliases
    (hashtable-set! ht "ls" '("ls" "--color=auto"))
    (hashtable-set! ht "l"  '("ls" "-al"))
    (hashtable-set! ht "v"  '("ls" "-l"))
    (case-lambda
      (()
        ;; return the global hashtable name -> alias
        ;; Each alias is either a list of strings or a procedure args -> prog-and-args
        ;; i.e. it must accept a list of strings and return a list of strings
        ht)
      ((name)
        ;; find and return the definition of specified alias name,
        ;; or #f if not found
        (hashtable-ref ht name #f))
      ((name alias)
        (if alias
          ;; add an alias to (sh-alias) table.
          ;; name must be a string;
          ;; alias must be a list of strings or a procedure args -> prog-and-args
          ;; i.e. it must accept a list of strings and return a list of strings
          ;;
          ;; do NOT modify alias argument after calling this function.
          (begin
            (if (procedure? alias)
              (assert* 'sh-alias (logbit? 1 (procedure-arity-mask alias)))
              (assert-string-list? 'sh-alias alias))
            (when (hashtable-ref (sh-builtins) name #f)
              (write-builtin-warning
                (string-append "\"" name
                  "\" is a builtin. defining an alias with the same name is allowed, but probably confusing")))
            (hashtable-set! ht name alias))
          ;; remove an alias from (sh-alias) table.
          (hashtable-delete! ht name))))))


;; given a command line prog-and-args i.e. a list of strings,
;; extract the first string and expand the corresponding alias.
;; Return the list of strings produced by recursive alias expansion.
;; Return the unmodified prog-and-args if no corresponding alias is found.
;;
;; Note: aliases are expanded recursively, i.e. if the expansion produced by an alias
;; starts with an alias name, it is expanded again.
;; To avoid infinite recursion, each alias name is only expanded at most once.
;;
;; Note: for sanity, (sh-alias-expand) ignores aliases for "builtin"
(define (sh-alias-expand prog-and-args)
  (assert-string-list? 'sh-alias-expand prog-and-args)
  (let ((expansion (alias-expand prog-and-args '("builtin")))) ; suppress alias expansion for "builtin"
    (assert-string-list? 'sh-alias-expand expansion)
    expansion))



;; the "alias" builtin: show all aliases, or show a single alias, or set an alias.
;;
;; As all builtins do, must return job status.
(define (builtin-alias job prog-and-args options)
  ; (debugf "builtin-alias ~s" prog-and-args)
  (cond
   ((or (null? prog-and-args) (null? (cdr prog-and-args)))
     (show-aliases))
   ((null? (cddr prog-and-args))
     (show-alias (cadr prog-and-args)))
   (else
     (sh-alias (cadr prog-and-args) (cddr prog-and-args))
     (void))))


;; the "unalias" builtin: unset zero or more aliases.
;;
;; As all builtins do, must return job status.
(define (builtin-unalias job prog-and-args options)
  (do ((tail (cdr prog-and-args) (cdr tail)))
      ((null? tail) (void))
    (sh-alias (car tail) #f)))


(define (show-aliases)
  (let ((wbuf    (make-bytespan 0))
        (aliases (span))
        (fd      (sh-fd 1)))
    (for-hash-cells ((cell (sh-alias)))
      (span-insert-right! aliases cell))
    (span-sort! (lambda (cell1 cell2) (string<? (car cell1) (car cell2))) aliases)
    (for-span cell aliases
      (show-alias* (car cell) (cdr cell) wbuf)
      (when (fx>=? (bytespan-length wbuf) 4096)
        (fd-write/bytespan! fd wbuf)))
    (fd-write/bytespan! fd wbuf)
    (void))) ; return (void), means builtin finished, successfully


(define (show-alias name)
  (let ((alias (sh-alias name)))
    (if alias
      (let ((wbuf  (make-bytespan 0)))
        (show-alias* name alias wbuf)
        (fd-write/bytespan! (sh-fd 1) wbuf)
        (void))                                         ; success, return (void)
      (write-builtin-error "alias" name "not found")))) ; error, return (failed 1)


(define (show-alias* name alias wbuf)
  (bytespan-insert-right/string! wbuf "alias ")
  (bytespan-insert-right/string! wbuf name)
  (cond
    ((procedure? alias)
      (bytespan-insert-right/string! wbuf " #<procedure>"))
    ((or (pair? alias) (null? alias))
      (for-list ((elem alias))
        (if (string? elem)
          (begin
            (bytespan-insert-right/u8! wbuf 32 39) ; #\space #\'
            (bytespan-insert-right/string! wbuf elem)
            (bytespan-insert-right/u8! wbuf 39))   ; #\'
          (bytespan-insert-right/string! wbuf  elem " #<bad-value>"))))
    (else
      (bytespan-insert-right/string! wbuf " #<bad-value>")))
  (bytespan-insert-right/u8! wbuf 10)) ; #\newline
