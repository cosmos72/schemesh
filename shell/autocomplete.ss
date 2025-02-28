;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh shell autocomplete (0 7 6))
  (export
      sh-autocomplete-func sh-autocomplete-r6rs sh-autocomplete-scheme sh-autocomplete-shell)
  (import
    (rnrs)
    (only (chezscheme) environment-symbols fx1+ fx1- sort!)
    (only (schemesh bootstrap)            values->list)
    (only (schemesh containers list)      list-iterate list-remove-consecutive-duplicates!)
    (only (schemesh containers string)    string-range=? string-split string-prefix?)
    (only (schemesh containers hashtable) hashtable-iterate)
    (schemesh containers charspan)
    (schemesh containers span)
    (schemesh containers sort)
    (only (schemesh containers utf8b) utf8b->string)
    (only (schemesh posix dir)        directory-list-type directory-sort!)
    (only (schemesh lineedit vscreen) vscreen-char-before-xy vscreen-cursor-ix vscreen-cursor-iy)
    (schemesh lineedit paren)
    (only (schemesh lineedit linectx) linectx-completion-stem linectx-vscreen)
    (only (schemesh shell parameters) sh-current-environment)
    (only (schemesh shell builtins)   sh-builtins)
    (only (schemesh shell job)        sh-env-iterate/direct sh-aliases sh-env-ref))

;; each sh-autocomplete-... procedure accepts a prefix charspan and a span of charspans,
;; and fills the span with possible completions of prefix:
;;  scheme parsers will list possible completions among symbols in current environment,
;;  shell parsers will list possible completions among files in current directory, etc.


;; fill span-of-charspans completions with top-level scheme symbols whose name starts with charspan stem
(define (sh-autocomplete-r6rs lctx paren completions)
  (sh-autocomplete-scheme lctx paren completions))


;; fill span-of-charspans completions with top-level scheme symbols whose name starts with charspan stem
(define (sh-autocomplete-scheme lctx paren completions)
  (%compute-stem lctx paren %char-is-scheme-identifier? %always-false)
  (let* ((stem     (linectx-completion-stem lctx))
         (stem-len (charspan-length stem)))
    ; (debugf "sh-autocomplete-scheme paren=~s stem=~s" paren stem)
    (if (eqv? #\" (paren-start-token paren))
       ; list contents of some directory
       (let ((slash-pos (charspan-rfind/char stem #\/)))
         (if slash-pos
           ; list contents of a directory
           (let ((dir    (charspan->string stem 0 (fx1+ slash-pos)))
                 (prefix (charspan->string stem (fx1+ slash-pos) stem-len)))
             (%list-directory dir prefix slash-pos completions))
           ; list contents of current directory
           (%list-directory "." (charspan->string stem) #f completions)))
      ; list top-level scheme symbols
      (begin
        (list-iterate (environment-symbols (sh-current-environment))
          (lambda (sym)
            (let* ((name (symbol->string sym))
                   (len  (string-length name)))
              (when (and (fx>=? len stem-len) (charspan-range/string=? stem 0 name 0 stem-len))
                (let ((csp (string->charspan* name)))
                  (charspan-erase-front! csp stem-len)
                  (span-insert-back! completions csp))))))
        (span-sort! charspan<? completions)))))


;; fill span-of-charspans completions with file names starting with charspan stem
(define (sh-autocomplete-shell lctx paren completions)
  (let-values (((stem-is-first-word? x y) (%compute-stem lctx paren %char-is-shell-identifier? %char-is-dollar?)))
    (let* ((stem      (linectx-completion-stem lctx))
           (stem-len  (charspan-length stem))
           (dollar?   (and (not (fxzero? stem-len)) (char=? #\$ (charspan-ref stem 0))))
           (slash-pos (and (not (fxzero? stem-len)) (not dollar?) (charspan-rfind/char stem #\/))))
      ; (debugf "sh-autocomplete-shell stem=~s, stem-is-first-word?=~s" stem stem-is-first-word?)
      (cond
        (dollar?
          (%list-shell-env lctx (charspan->string stem) completions))
        (slash-pos ; list contents of a directory
          (let ((dir    (charspan->string stem 0 (fx1+ slash-pos)))
                (prefix (charspan->string stem (fx1+ slash-pos) stem-len)))
            (%list-directory dir prefix slash-pos completions)))
        ((or stem-is-first-word? (%stem-is-after-shell-separator? (linectx-vscreen lctx) x y))
          ; list builtins, aliases and programs in $PATH
          (%list-shell-commands lctx (charspan->string stem) completions))
        (else ; list contents of current directory
          (%list-directory "." (charspan->string stem) #f completions))))))


;; TEMPORARY and APPROXIMATED:
;; fill charspan (linectx-completion-stem) with the word to autocomplete.
;; the correct solution requires parsing parens and finding the longest syntax-aware identifier
;; returns three values:
;;   #t if stem starts at beginning of paren, otherwise return #f
;;   vscreen x and y of stopping character before stem
(define (%compute-stem lctx paren char-is-ident-pred char-is-start-pred)
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
    ; (debugf "%compute-stem paren=~s, xmin=~s, ymin=~s" (paren->list paren) xmin ymin)
    (charspan-clear! stem)
    (let %fill-stem ((x (vscreen-cursor-ix screen))
                     (y (vscreen-cursor-iy screen)))
      (let-values (((x1 y1 ch) (%vscreen-char-before-xy screen x y)))
        ; (debugf "%vscreen-char-before-xy x=~s, y=~s -> x1=~s, y1=~s, ch=~s" x y x1 y1 ch)
        (cond
          ((not (and x1 y1 (char? ch))) ; reached start of paren or vscreen
             (values #t x y))
          ((char-is-ident-pred ch) ; found an identifier char, insert it and iterate
             (charspan-insert-front! stem ch)
             (%fill-stem x1 y1))
          ((char-is-start-pred ch) ; found $ or some other char that starts the identifier, insert it and exit
             (charspan-insert-front! stem ch)
             (values #f x y))
          (else ; found a non-identifier char, could be a blank
            (let %vscreen-contains-only-blanks-before-xy? ((screen screen) (x x) (y y))
              (let-values (((x1 y1 ch) (%vscreen-char-before-xy screen x y)))
                (cond
                  ((not (and x1 y1 (char? ch))) ; reached start of paren or vscreen, found only blanks
                    (values #t x y))
                  ((char>? ch #\space) ; found another word before stem
                    (values #f x y))
                  (else ; iterate
                    (%vscreen-contains-only-blanks-before-xy? screen x1 y1)))))))))))


(define (%always-false ch)
  #f)

(define (%char-is-dollar? ch)
  (char=? #\$ ch))

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


(define (%stem-is-after-shell-separator? screen x y)
  (let-values (((x1 y1 ch) (vscreen-char-before-xy screen x y)))
    ; (debugf "%stem-is-after-shell-separator? x=~s, y=~s -> x1=~s y1=~s ch=~s" x y x1 y1 ch)
    (cond
      ((not (and x1 y1 ch))  #f)
      ((memv ch '(#\; #\newline #\! #\& #\|)) #t)
      ((char<=? ch #\space)
        (%stem-is-after-shell-separator? screen x1 y1))
      (else #f))))


(define (%list-directory dir prefix slash? completions)
  ; (debugf "lineedit-shell-list/directory dir = ~s, prefix = ~s" dir prefix)
  (let* ((dir?       (and slash? (not (fxzero? (string-length dir)))))
         (prefix-len (string-length prefix))
         (prefix?    (not (fxzero? prefix-len)))
         (prefix-starts-with-dot? (and prefix? (char=? #\. (string-ref prefix 0)))))
    (list-iterate (directory-sort! (directory-list-type dir 'prefix prefix 'bytes 'catch))
      (lambda (elem)
        (let ((name (string->charspan* (utf8b->string (car elem)))))
          (when (or prefix-starts-with-dot? (not (char=? #\. (charspan-ref name 0))))
            (charspan-erase-front! name prefix-len)
            (when (eq? 'dir (cdr elem))
              (charspan-insert-back! name #\/))
            (span-insert-back! completions name))))))
  ; (debugf "lineedit-shell-list/directory completions = ~s" completions)
  )


;; list environment variables that start with prefix, and append them to completions
;; NOTE: prefix always starts with #\$
(define (%list-shell-env lctx prefix completions)
  ; (debugf "%list-shell-env prefix = ~s" prefix)
  (let* ((prefix-len (fx1- (string-length prefix)))
         (l      '()))
    ; (debugf "%list-shell-env htable = ~s" htable)
    (sh-env-iterate/direct #t
      (lambda (name val visibility)
        (when (and (fx>=? (string-length name) prefix-len)
                   (string-range=? name 0 prefix 1 prefix-len))
          (let ((cname (string->charspan* name)))
            (charspan-erase-front! cname prefix-len)
            (span-insert-back! completions cname))))))
  ; (debugf "%list-shell-env completions = ~s" completions)
  (span-sort! charspan<? completions)
  ; (debugf "%list-shell-env completions sorted = ~s" completions)
  )



;; list builtins, aliases and programs in $PATH that start with prefix,
;; and append them to completions
(define (%list-shell-commands lctx prefix completions)
  ; (debugf "%list-shell-commands prefix = ~s" prefix)
  (let ((l (%list-shell-programs prefix
             (%list-shell-builtins prefix
               (%list-shell-aliases prefix '()))))
        (prefix-len (string-length prefix)))
    (set! l (sort! string<? l))
    ; (debugf "%list-shell-commands prefix=~s, list-with-duplicates   =~s" prefix l)
    (list-remove-consecutive-duplicates! l string=?)
    ; (debugf "%list-shell-commands prefix=~s, list-without-duplicates=~s" prefix l)
    (list-iterate l
      (lambda (name)
        (let ((cname (string->charspan* name)))
          (charspan-erase-front! cname prefix-len)
          (span-insert-back! completions cname)))))
  ; (debugf "%list-shell-commands prefix=~s, completions=~s" prefix completions)
  )

;; find shell aliases starting with prefix, cons them onto list l, and return l
(define (%list-shell-aliases prefix l)
  (%list-htable-keys (sh-aliases) prefix l))


;; find shell builtins starting with prefix, cons them onto list l, and return l
(define (%list-shell-builtins prefix l)
  (%list-htable-keys (sh-builtins) prefix l))


;; find hashtable keys starting with prefix, cons them onto list l, and return l
(define (%list-htable-keys htable prefix l)
  (let ((prefix-len (string-length prefix)))
    (hashtable-iterate htable
      (lambda (cell)
        (let ((name (car cell)))
          (when (string-prefix? name prefix)
            (set! l (cons name l)))))))
  l)

;; find programs in $PATH that start with prefix, cons them onto list l, and return l
(define (%list-shell-programs prefix l)
  (let* (($path      (sh-env-ref #t "PATH"))
         (dirs       (string-split $path #\:))
         (prefix-len (string-length prefix)))
    (list-iterate dirs
      (lambda (dir)
        (list-iterate (directory-list-type dir 'prefix prefix 'catch) ; no need to sort directory list
          (lambda (elem)
            (when (eq? 'file (cdr elem))
              (set! l (cons (car elem) l))))))))
  l)

;; return the correct autocompletion function for specified parser name,
;; or #f if not found.
(define (sh-autocomplete-func parser-name)
  (case parser-name
    ((r6rs)   sh-autocomplete-r6rs)
    ((scheme) sh-autocomplete-scheme)
    ((shell)  sh-autocomplete-shell)
    (else     #f)))

) ; close library
