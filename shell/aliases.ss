;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only from file builtins.ss


;; find and return the alias corresponding to specified name,
;; or #f if not found
(define (sh-alias name)
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
;; Note: for sanity, (sh-alias-expand) ignores aliases for "builtin"
(define (sh-alias-expand prog-and-args)
  (alias-expand prog-and-args '("builtin"))) ; suppress alias expansion for "builtin"


(define (alias-expand prog-and-args suppressed-name-list)
  (assert-string-list? 'sh-alias-expand prog-and-args)
  (if (null? prog-and-args)
    prog-and-args
    (let* ((name  (car prog-and-args))
           (alias (sh-alias name)))
      (if (and alias (not (member name suppressed-name-list)))
        ;; recursively expand output of alias expansion,
        ;; but suppress expansion of already-expanded name
        (let ((expanded (if (procedure? alias)
                          (alias (cdr prog-and-args))
                          (append alias (cdr prog-and-args)))))
          (alias-expand expanded (cons name suppressed-name-list)))
        prog-and-args))))


;; add an alias to (sh-aliases) table.
;; name must be a string; expansion must be a list of strings.
;; command line (cons name args) will be expanded to (append expansion args)
;;
;; do NOT modify expansion after calling this function.
(define (sh-alias-set! name expansion)
  (assert-string-list? 'sh-alias-set! expansion)
  (hashtable-set! (sh-aliases) name expansion))


;; remove an alias from (sh-aliases) table.
(define (sh-alias-delete! name)
  (hashtable-delete! (sh-aliases) name))


;; the "alias" builtin
(define (builtin-alias job prog-and-args options)
  ; (debugf "builtin-alias ~s" prog-and-args)
  (assert-string-list? 'builtin-alias prog-and-args)
  (cond
   ((or (null? prog-and-args) (null? (cdr prog-and-args)))
     (show-aliases))
   ((null? (cddr prog-and-args))
     (show-alias (cadr prog-and-args)))
   (#t
     (sh-alias-set! (cadr prog-and-args) (cddr prog-and-args)))))


;; the "unalias" builtin
(define (builtin-unalias job prog-and-args options)
  (assert-string-list? 'builtin-unalias prog-and-args)
  (do ((tail (cdr prog-and-args) (cdr tail)))
      ((null? tail))
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
  (let ((wbuf (make-bytespan 0))
        (aliases (span)))
    (hashtable-iterate (sh-aliases)
      (lambda (cell)
        (span-insert-back! aliases cell)))
    (span-sort! (lambda (cell1 cell2) (string<? (car cell1) (car cell2))) aliases)
    (span-iterate aliases
      (lambda (i cell)
        (show-alias* (car cell) (cdr cell) wbuf)
        (when (fx>=? (bytespan-length wbuf) 4096)
          (fd-stdout-write/bspan! wbuf))))
    (fd-stdout-write/bspan! wbuf)
    (void))) ; return (void), means builtin exited succesfully


(define (show-alias name)
  (let ((wbuf  (make-bytespan 0))
        (alias (sh-alias name)))
    (if alias
      (show-alias* name alias wbuf)
      (begin
        (bytespan-insert-back/string! wbuf "schemesh: alias: ")
        (bytespan-insert-back/string! wbuf name)
        (bytespan-insert-back/string! wbuf ": not found\n")))
    (fd-stdout-write/bspan! wbuf)
    (if alias (void) '(exited . 1))))


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
