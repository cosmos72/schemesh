;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh lineedit autocomplete (0 1))
  (export
    lineedit-r6rs-autocomplete
    lineedit-scheme-autocomplete
    lineedit-shell-autocomplete)
  (import
    (rnrs)
    (only (chezscheme) environment-symbols fx1+ interaction-environment sort! void)
    (only (schemesh bootstrap) debugf)
    (only (schemesh containers misc) list-iterate)
    (schemesh containers charspan)
    (schemesh containers span)
    (only (schemesh containers utf8b) utf8b->string)
    (only (schemesh posix misc) directory-u8-list))

;; fill span-of-charspans completions with top-level scheme symbols whose name starts with charspan stem
(define (lineedit-r6rs-autocomplete stem completions)
  (lineedit-scheme-autocomplete stem completions))

;; fill span-of-charspans completions with top-level scheme symbols whose name starts with charspan stem
(define (lineedit-scheme-autocomplete stem completions)
  (let ((n (charspan-length stem))
        (l '()))
    (list-iterate (environment-symbols (interaction-environment))
      (lambda (sym)
        (let* ((name (symbol->string sym))
               (len  (string-length name)))
          (when (and (fx>=? len n) (charspan-range/string=? stem 0 name 0 (fxmin len n)))
            (set! l (cons name l))))))
    (list-iterate (sort! string<? l)
      (lambda (name)
        (span-insert-back! completions (string->charspan* name))))))


;; fill span-of-charspans completions with file names starting with charspan stem
(define (lineedit-shell-autocomplete stem completions)
  (debugf "lineedit-shell-autocomplete stem = ~s~%" stem)
  (let* ((n (charspan-length stem))
         (slash-pos (and (not (fxzero? n)) (charspan-find/ch stem 0 n #\/))))
    (cond
      (slash-pos ; list contents of a directory
        (let ((dir    (charspan-range->string stem 0 (fx1+ slash-pos)))
              (filter (charspan-range->string stem (fx1+ slash-pos) n)))
          (%lineedit-shell-directory-list dir filter slash-pos completions)))
      (#t ; list contents of current directory
          ; FIXME: if stem is the first word in a shell command,
          ;        we should list programs in $PATH instead.
        (%lineedit-shell-directory-list "." (charspan->string stem) #f completions)))))


(define (%lineedit-shell-directory-list dir filter slash? completions)
  (debugf "lineedit-shell-directory-list dir = ~s, filter = ~s~%" dir filter)
  (let* ((dir-len    (string-length dir))
         (dir?       (and slash? (not (fxzero? dir-len))))
         (filter?    (not (fxzero? (string-length filter))))
         (filter-starts-with-dot? (and filter? (char=? #\. (string-ref filter 0)))))
    (list-iterate (directory-u8-list dir filter)
      (lambda (elem)
        (let ((name (string->charspan* (utf8b->string (cdr elem)))))
          (when (or filter-starts-with-dot? (not (char=? #\. (charspan-ref name 0))))
            (when dir?
              (charspan-insert-front/string! name dir 0 dir-len))
            (when (eq? 'dir (car elem))
              (charspan-insert-back! name #\/))
            (span-insert-back! completions name))))))
  (debugf "lineedit-shell-directory-list completions = ~s~%" completions)
  )

) ; close library
