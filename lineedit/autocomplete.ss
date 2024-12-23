;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh lineedit autocomplete (0 1))
  (export
    lineedit-autocomplete/r6rs
    lineedit-autocomplete/scheme
    lineedit-autocomplete/shell)
  (import
    (rnrs)
    (only (chezscheme) environment-symbols fx1+ interaction-environment sort! top-level-value void)
    (only (schemesh bootstrap) debugf values->list)
    (only (schemesh containers misc) list-iterate list-remove-consecutive-duplicates! string-split)
    (schemesh containers charspan)
    (schemesh containers span)
    (only (schemesh containers utf8b) utf8b->string)
    (only (schemesh posix misc) directory-u8-list)
    (only (schemesh lineedit vscreen) vscreen-char-before-xy vscreen-cursor-ix vscreen-cursor-iy)
    (schemesh lineedit paren)
    (only (schemesh lineedit linectx) linectx-completion-stem linectx-vscreen))


;; fill span-of-charspans completions with top-level scheme symbols whose name starts with charspan stem
(define (lineedit-autocomplete/r6rs lctx paren completions)
  (lineedit-autocomplete/scheme lctx paren completions))


;; fill span-of-charspans completions with top-level scheme symbols whose name starts with charspan stem
(define (lineedit-autocomplete/scheme lctx paren completions)
  (%lineedit-compute-stem lctx paren %char-is-scheme-identifier?)
  (let* ((stem     (linectx-completion-stem lctx))
         (stem-len (charspan-length stem))
         (l        '()))
    ; (debugf "lineedit-autocomplete/shell stem=~s~%" stem)
    (list-iterate (environment-symbols (interaction-environment))
      (lambda (sym)
        (let* ((name (symbol->string sym))
               (len  (string-length name)))
          (when (and (fx>=? len stem-len) (charspan-range/string=? stem 0 name 0 stem-len))
            (set! l (cons name l))))))
    ; (debugf "lineedit-autocomplete/scheme list=~s~%" l)
    (list-iterate (sort! string<? l)
      (lambda (name)
        (let ((csp (string->charspan* name)))
          (charspan-erase-front! csp stem-len)
          (span-insert-back! completions csp))))))


;; fill span-of-charspans completions with file names starting with charspan stem
(define (lineedit-autocomplete/shell lctx paren completions)
  (let* ((stem-is-first-word? (%lineedit-compute-stem lctx paren %char-is-shell-identifier?))
         (stem      (linectx-completion-stem lctx))
         (stem-len  (charspan-length stem))
         (slash-pos (and (not (fxzero? stem-len)) (charspan-rfind/ch stem 0 stem-len #\/))))
    ; (debugf "lineedit-autocomplete/shell stem=~s, stem-is-first-word?=~s~%" stem stem-is-first-word?)
    (cond
      (slash-pos ; list contents of a directory
        (let ((dir    (charspan-range->string stem 0 (fx1+ slash-pos)))
              (filter (charspan-range->string stem (fx1+ slash-pos) stem-len)))
          (%lineedit-shell-list/directory dir filter slash-pos completions)))
      (stem-is-first-word? ; list programs in $PATH
        (%lineedit-shell-list/programs-in-$path lctx (charspan->string stem) completions))
      (#t ; list contents of current directory
        (%lineedit-shell-list/directory "." (charspan->string stem) #f completions)))))


;; TEMPORARY and APPROXIMATED:
;; fill charspan (linectx-completion-stem) with the word to autocomplete.
;; the correct solution requires parsing parens and finding the longest syntax-aware identifier
;; returns #t if stem starts at beginning of paren, otherwise return #f
(define (%lineedit-compute-stem lctx paren char-pred)
  (let* ((stem   (linectx-completion-stem lctx))
         (screen (linectx-vscreen lctx))
         (xmin  (if paren
                   (fx+ (paren-start-x paren)
                        (if (char? (paren-start-token paren)) 2 1))
                   0))
         (ymin  (if paren (paren-start-y paren) 0))
         (%vscreen-char-before-xy
           (lambda (screen x y)
             (if (or (fx>? y ymin)
                     (and (fx=? y ymin) (fx>=? x xmin)))
               (vscreen-char-before-xy screen x y)
               (values #f #f #f)))))
    ; (debugf "%lineedit-compute-stem paren=~s, xmin=~s, ymin=~s~%" (values->list (paren->values paren)) xmin ymin)
    (charspan-clear! stem)
    (let %fill-stem ((x (vscreen-cursor-ix screen))
                     (y (vscreen-cursor-iy screen)))
      (let-values (((x1 y1 ch) (%vscreen-char-before-xy screen x y)))
        ; (debugf "%vscreen-char-before-xy x=~s, y=~s -> x1=~s, y1=~s, ch=~s~%" x y x1 y1 ch)
        (cond
          ((not (and x1 y1 (char? ch))) ; reached start of paren or vscreen
             #t)
          ((char-pred ch) ; found an identifier char, insert it and iterate
             (charspan-insert-front! stem ch)
             (%fill-stem x1 y1))
          (#t ; found a non-identifier char, could be a blank
            (let %vscreen-contains-only-blanks-before-xy? ((screen screen) (x x) (y y))
              (let-values (((x1 y1 ch) (%vscreen-char-before-xy screen x y)))
                (cond
                  ((not (and x1 y1 (char? ch)))  ; reached start of paren or vscreen, found only blanks
                    #t)
                  ((char>? ch #\space)  ; found another word before stem
                    #f)
                  (#t ; iterate
                    (%vscreen-contains-only-blanks-before-xy? screen x1 y1)))))))))))


(define (%char-is-scheme-identifier? ch)
  (and
    (char<=? #\! ch #\~)
    (or (char<=? #\a ch #\z)
        (char<=? #\< ch #\Z)  ; i.e. one of < = > ? @ A ... Z
        (char<=? #\* ch #\:)  ; i.e. one of * + , - . / 0 ... 9 :
        (char<=? #\$ ch #\%)  ; i.e. one of $ % %
        (memv ch '(#\! #\\ #\^ #\_ #\| #\~ ch)))))


(define (%char-is-shell-identifier? ch)
  (and
    (char<=? #\% ch #\~)
    (or (char<=? #\a ch #\z)
        (char<=? #\@ ch #\Z)  ; i.e. one of @ A ... Z
        (char<=? #\+ ch #\:)  ; i.e. one of + , - . / 0 ... 9 :
        (memv ch '(#\% #\= #\\ #\^ #\\ #\_ #\~)))))


(define (%lineedit-shell-list/directory dir filter slash? completions)
  ; (debugf "lineedit-shell-list/directory dir = ~s, filter = ~s~%" dir filter)
  (let* ((dir-len    (string-length dir))
         (dir?       (and slash? (not (fxzero? dir-len))))
         (filter-len (string-length filter))
         (filter?    (not (fxzero? filter-len)))
         (filter-starts-with-dot? (and filter? (char=? #\. (string-ref filter 0)))))
    (list-iterate (directory-u8-list dir filter)
      (lambda (elem)
        (let ((name (string->charspan* (utf8b->string (cdr elem)))))
          (when (or filter-starts-with-dot? (not (char=? #\. (charspan-ref name 0))))
            (charspan-erase-front! name filter-len)
            (when (eq? 'dir (car elem))
              (charspan-insert-back! name #\/))
            (span-insert-back! completions name))))))
  ; (debugf "lineedit-shell-list/directory completions = ~s~%" completions)
  )

(define (%lineedit-shell-list/programs-in-$path lctx stem completions)
  ; (debugf "lineedit-shell-list/programs-in-$path stem = ~s~%" stem)
  (let* (($path      ((top-level-value 'sh-env) #t "PATH"))
         (dirs       (string-split $path 0 (string-length $path) #\:))
         (filter     stem)
         (filter-len (string-length filter))
         (l      '()))
    (list-iterate dirs
      (lambda (dir)
        (list-iterate (directory-u8-list dir filter)
          (lambda (elem)
            (let ((type (car elem)))
              (when (or (eq? 'file type) (eq? 'symlink type))
                (let ((name (utf8b->string (cdr elem))))
                  (set! l (cons name l)))))))))
    (set! l (sort! string<? l))
    (list-remove-consecutive-duplicates! l string=?)
    (list-iterate l
      (lambda (name)
        (let ((cname (string->charspan* name)))
          (charspan-erase-front! cname filter-len)
          (span-insert-back! completions cname)))))
  ; (debugf "lineedit-shell-list/programs-in-$path completions = ~s~%" completions)
  )



) ; close library
