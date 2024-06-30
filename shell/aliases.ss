;;; Copyright (C) 2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh shell aliases (0 1))
  (export sh-alias-delete! sh-alias-set! sh-alias-expand sh-aliases sh-builtin-alias sh-builtin-unalias)
  (import
    (rnrs)
    (only (chezscheme) void)
    (only (schemesh bootstrap) raise-errorf)
    (only (schemesh containers misc) assert-string-list?))



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
  (%expand-alias prog-and-args '("builtin"))) ; suppress alias expansion for "builtin"


(define (%expand-alias prog-and-args suppressed-name-list)
  (assert-string-list? 'sh-alias-expand prog-and-args)
  (if (null? prog-and-args)
    prog-and-args
    (let* ((name (car prog-and-args))
           (alias (hashtable-ref (sh-aliases) name #f)))
      (if (and alias (not (member name suppressed-name-list)))
        ;; recursively expand output of alias expansion,
        ;; but suppress expansion of already-expanded name
        (%expand-alias (alias (cdr prog-and-args)) (cons name suppressed-name-list))
        prog-and-args))))


;; add an alias to (sh-aliases) table.
;; name must be a string; expansion must be a list of strings.
;; command line (cons name args) will be expanded to (append expansion args)
(define (sh-alias-set! name expansion)
  (assert-string-list? 'sh-alias-set! expansion)
  (hashtable-set! (sh-aliases) name (lambda (args) (append expansion args))))


;; remove an alias from (sh-aliases) table.
(define (sh-alias-delete! name)
  (hashtable-delete! (sh-aliases) name))


;; the "alias" builtin
(define (sh-builtin-alias cmd prog-and-args options)
  ; (debugf "sh-builtin-alias ~s~%" prog-and-args)
  (assert-string-list? 'sh-builtin-alias prog-and-args)
  (if (or (null? prog-and-args) (null? (cdr prog-and-args)))
    (void) ;; TODO: a lone "alias" should list aliases
    (sh-alias-set! (cadr prog-and-args) (cddr prog-and-args))))


;; the "unalias" builtin
(define (sh-builtin-unalias cmd prog-and-args options)
  (assert-string-list? 'sh-builtin-unalias prog-and-args)
  (do ((tail (cdr prog-and-args) (cdr list)))
      ((null? tail))
    (sh-alias-delete! (car tail))))


;; function returning the global hashtable name -> alias
;; Each alias is a function args -> prog-and-args
;; i.e. it must accept a list of strings and return a list of strings
(define sh-aliases
  (let ((t (make-hashtable string-hash string=?)))
    ; initial aliases
    (hashtable-set! t ":"  (lambda (args) (cons "true" args)))
    (hashtable-set! t "l"  (lambda (args) (cons "ls" (cons "-l" args))))
    (hashtable-set! t "ls" (lambda (args) (cons "ls" (cons "--color=auto" args))))
    (lambda () t)))


) ; close library
