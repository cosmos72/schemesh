;;; Copyright (C) 2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh shell aliases (0 1))
  (export sh-alias-set! sh-aliases sh-expand-alias)
  (import
    (rnrs)
    (only (schemesh bootstrap) raise-errorf))


;; given a command arg-list i.e. a list of strings,
;; extract the first string and expand the corresponding alias.
;; Return the list of strings produced by alias expansion.
;; Return the unmodified arg-list if no corresponding alias is found.
;;
;; Note: aliases are expanded recursively, i.e. if the expansion produced by an alias
;; starts with an alias name, it is expanded again.
;; To avoid infinite recursion, each alias name is only expanded at most once.
(define (sh-expand-alias arg-list)
  (%expand-alias arg-list '()))


(define (%expand-alias arg-list suppressed-name-list)
  (if (null? arg-list)
    arg-list
    (let* ((name (car arg-list))
           (alias (hashtable-ref (sh-aliases) name #f)))
      (if (and alias (not (member name suppressed-name-list)))
        ;; recursively expand output of alias expansion,
        ;; but suppress expansion of already-expanded name
        (%expand-alias (alias (cdr arg-list)) (cons name suppressed-name-list))
        arg-list))))


;; function returning the global hashtable name -> alias
;; Each alias is a function arg-list -> arg-list
;; i.e. it must accept a list of strings and return a list of strings
(define sh-aliases
  (let ((t (make-hashtable string-hash string=?)))
    ; some initial aliases
    (hashtable-set! t "l" (lambda (args) (cons* "ls" "-l" args)))
    (hashtable-set! t "ls" (lambda (args) (cons* "ls" "--color=auto" args)))
    (lambda () t)))


;; add an alias to (sh-aliases) table.
;; command line (name . args) will be expanded to (cons* expansion args)
(define (sh-alias-set! name . expansion)
  (hashtable-set! (sh-aliases) name (lambda (args) (cons* expansion args))))


) ; close library
