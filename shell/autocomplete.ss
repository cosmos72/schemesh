;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh shell autocomplete (0 9 3))
  (export
      sh-autocomplete-func sh-autocomplete-r6rs sh-autocomplete-scheme sh-autocomplete-shell)
  (import
    (rnrs)
    (only (chezscheme)                    environment-symbols fx1+ fx1- sort!)
    (only (scheme2k containers list)      for-list list-remove-consecutive-duplicates!)
    (only (scheme2k containers string)    substring=? string-empty? string-prefix? string-split)
    (only (scheme2k containers hashtable) for-hash-keys)
    (scheme2k containers charspan)
    (scheme2k containers span)
    (scheme2k containers sort)
    (only (scheme2k containers utf8b)     codepoint-utf8b? integer->char* utf8b->string)
    (only (scheme2k posix fs)             directory-list-type directory-sort!)
    (only (scheme2k vscreen)              vscreen-char-before-xy vscreen-cursor-ix vscreen-cursor-iy)
    (scheme2k lineedit paren)
    (only (scheme2k lineedit lineedit)    linectx-completion-stem linectx-vscreen)
    (only (schemesh shell parameters)     sh-current-environment)
    (only (schemesh shell job)            sh-aliases sh-builtins sh-env-iterate/direct sh-env-ref sh-userhome))

;; each sh-autocomplete-... procedure accepts a prefix charspan and a span of charspans,
;; and fills the span with possible completions of prefix:
;;  scheme parsers will list possible completions among symbols in current environment,
;;  shell parsers will list possible completions among files in current directory, etc.


;; fill span-of-charspans completions with top-level scheme symbols whose name starts with charspan stem
(define (sh-autocomplete-r6rs lctx paren completions)
  (sh-autocomplete-scheme lctx paren completions))


;; fill span-of-charspans completions with top-level scheme symbols whose name starts with charspan stem
(define (sh-autocomplete-scheme lctx paren completions)
  (%compute-scheme-stem lctx paren)
  (let* ((stem     (linectx-completion-stem lctx))
         (stem-len (charspan-length stem)))
    ; (debugf "sh-autocomplete-scheme paren=~s stem=~s" paren stem)
    (if (eqv? #\" (paren-start-token paren))
       ; list contents of some directory
       (let ((slash-pos (charspan-index-right/char stem #\/)))
         (if slash-pos
           ; list contents of a directory
           (let ((dir    (%unescape-scheme-dquoted stem 0 (fx1+ slash-pos)))
                 (prefix (%unescape-scheme-dquoted stem (fx1+ slash-pos) stem-len)))
             (%list-directory dir prefix slash-pos %escape-scheme-dquoted completions))
           ; list contents of current directory
           (%list-directory "." (%unescape-scheme-dquoted stem 0 stem-len) #f %escape-scheme-dquoted completions)))
      ; list top-level scheme symbols
      (begin
        (for-list ((sym (environment-symbols (sh-current-environment))))
          (let* ((name (symbol->string sym))
                 (len  (string-length name)))
            (when (and (fx>=? len stem-len) (charspan/string=? stem 0 name 0 stem-len))
              (let ((csp (string->charspan* name)))
                (charspan-delete-left! csp stem-len)
                (span-insert-right! completions csp)))))
        (span-sort! charspan<? completions)))))


;; fill span-of-charspans completions with file names starting with charspan stem
(define (sh-autocomplete-shell lctx paren completions)
  (let-values (((stem-is-first-word? x y) (%compute-shell-stem lctx paren)))
    (let* ((stem      (linectx-completion-stem lctx))
           (stem?     (not (charspan-empty? stem)))
           (stem-ch0  (and stem? (charspan-ref stem 0)))
           (unescape-func (case stem-ch0
                            ((#\') %unescape-shell-squoted)
                            ((#\") %unescape-shell-dquoted)
                            (else  %unescape-shell-unquoted)))
           (escape-func (case stem-ch0
                          ((#\') %escape-shell-squoted)
                          ((#\") %escape-shell-dquoted)
                          (else  %escape-shell-unquoted)))
           (dollar?   (eqv? #\$ stem-ch0))
           (offset    (if (memv stem-ch0 '(#\' #\")) 1 0))
           ;; when expanding initial tilde, do NOT update dollar? unescape-func escape-func
           (stem      (if (eqv? #\~ stem-ch0)
                        (%expand-initial-tilde stem)
                        stem))
           (stem-len  (charspan-length stem))
           (slash-pos (and stem? (not dollar?) (charspan-index-right/char stem #\/))))

      ;; (debugf "sh-autocomplete-shell stem=~s, stem-is-first-word?=~s" stem stem-is-first-word?)
      (cond
        (dollar?
          (%list-shell-env lctx (charspan->string stem) completions))
        (slash-pos ; list contents of a directory
          (let ((dir    (unescape-func stem offset (fx1+ slash-pos)))
                (prefix (unescape-func stem (fx1+ slash-pos) stem-len)))
            (%list-directory dir prefix slash-pos escape-func completions)))
        ((or stem-is-first-word? (%stem-is-after-shell-separator? (linectx-vscreen lctx) x y))
          ; list builtins, aliases and programs in $PATH
          (%list-shell-commands lctx (charspan->string stem) completions))
        (else ; list contents of current directory
          (let ((prefix (unescape-func stem offset stem-len)))
            (%list-directory "." prefix #f escape-func completions)))))))


(define (%expand-initial-tilde csp)
  (let* ((len      (charspan-length csp))
         (slash    (charspan-index/char csp 1 len #\/))
         ;; TODO: username may be the initial prefix of one or more existing usernames: autocomplete them
         (username (charspan->string csp 1 (if slash slash len)))
         (userhome (if (string-empty? username) (sh-userhome) (sh-userhome username))))
    (cond
      ((not userhome)
        csp)
      ((not slash) ;; expand ~ or ~user
        (string->charspan userhome))
      (else ;; expand ~/some-path or ~user/some-path
        (let ((ret (string->charspan userhome)))
          (charspan-insert-right/charspan! ret csp slash len)
          ret)))))


;; TEMPORARY and APPROXIMATED:
;; fill charspan (linectx-completion-stem) with the word to autocomplete.
;; the correct solution requires parsing parens and finding the longest syntax-aware identifier
;; returns three values:
;;   #t if stem is at beginning of a shell command, otherwise return #f
;;   vscreen x and y of stopping character before stem
(define (%compute-shell-stem lctx paren)
  (let* ((screen (linectx-vscreen lctx))
         (stem   (linectx-completion-stem lctx))
         (token  (and paren (paren-start-token paren)))
         (quote? (memv token '(#\" #\')))
         (xmin   (if paren
                   (fx+ (paren-start-x paren)
                        (if (char? token) 1 0))
                   0))
         (ymin   (if paren (paren-start-y paren) 0))
         (%vscreen-char-before-xy
           (lambda (screen x y)
             (if (or (fx>? y ymin)
                     (and (fx=? y ymin) (fx>=? x xmin)))
               (vscreen-char-before-xy screen x y)
               (values #f #f #f)))))
    ; (debugf "%compute-shell-stem paren=~s, xmin=~s, ymin=~s" (paren->list paren) xmin ymin)
    (charspan-clear! stem)
    (let %fill-stem ((x (vscreen-cursor-ix screen))
                     (y (vscreen-cursor-iy screen)))
      (let-values (((x1 y1 ch) (%vscreen-char-before-xy screen x y)))
        ; (debugf "%vscreen-char-before-xy x=~s, y=~s -> x1=~s, y1=~s, ch=~s" x y x1 y1 ch)
        (cond
          ((not (and x1 y1 (char? ch))) ; reached start of paren or vscreen
             (values #t x y))
          ((and (eqv? ch token) (fx=? y ymin) (fx=? x xmin)) ; reached start of paren
             (when quote?
               (charspan-insert-left! stem ch)) ; insert quote char into stem
             (values (not quote?) x1 y1))
          ((eqv? token #\') ; only #\' is special in shell single quotes, and it is handled above
             (charspan-insert-left! stem ch)
             (%fill-stem x1 y1))
          ((char=? #\$ ch) ; #\$ is special also inside shell double quotes
             ;; TODO: check for backslash before #\$
             (charspan-insert-left! stem ch) ; insert #\$ char into stem
             (values #f x y))
          ((eqv? token #\")
            ;; inside shell double quotes, special chars are: #\$ #\` #\\ and of course #\"
            ;;   #\$ and #\" are handled above
            ;;   #\\ is currently ignored (APPROXIMATE)
            ;;   non-backslashed #\` cannot happen, paren token would be #\`
            (charspan-insert-left! stem ch)
            (%fill-stem x1 y1))
          ((%char-is-shell-identifier? ch)
            (charspan-insert-left! stem ch)
            (%fill-stem x1 y1))
          (else ; found a non-identifier char, could be a blank
            ;; TODO: check for backslash before non-identifier
            (let %vscreen-contains-only-blanks-before-xy? ((screen screen) (x x) (y y))
              (let-values (((x1 y1 ch) (%vscreen-char-before-xy screen x y)))
                (cond
                  ((not (and x1 y1 (char? ch))) ; reached start of paren or vscreen, found only blanks
                    (values #t x y))
                  ((char>? ch #\space) ; found another word before stem
                    (values #f x y))
                  (else ; iterate
                    (%vscreen-contains-only-blanks-before-xy? screen x1 y1)))))))))))


;; TEMPORARY and APPROXIMATED:
;; fill charspan (linectx-completion-stem) with the word to autocomplete.
;; the correct solution requires parsing parens and finding the longest syntax-aware identifier
;; returns three values:
;;   #t if stem starts at beginning of paren, otherwise return #f
;;   vscreen x and y of stopping character before stem
(define (%compute-scheme-stem lctx paren)
  (let* ((screen  (linectx-vscreen lctx))
         (stem    (linectx-completion-stem lctx))
         (token   (and paren (paren-start-token paren)))
         (dquote? (eqv? token #\"))
         (xmin    (if paren
                     (fx+ (paren-start-x paren)
                          (if (char? token) 2 1))
                     0))
         (ymin    (if paren (paren-start-y paren) 0))
         (%vscreen-char-before-xy
           (lambda (screen x y)
             (if (or (fx>? y ymin)
                     (and (fx=? y ymin) (fx>=? x xmin)))
               (vscreen-char-before-xy screen x y)
               (values #f #f #f)))))
    ; (debugf "%compute-scheme-stem paren=~s, xmin=~s, ymin=~s" (paren->list paren) xmin ymin)
    (charspan-clear! stem)
    (let %fill-stem ((x (vscreen-cursor-ix screen))
                     (y (vscreen-cursor-iy screen)))
      (let-values (((x1 y1 ch) (%vscreen-char-before-xy screen x y)))
        ; (debugf "%vscreen-char-before-xy x=~s, y=~s -> x1=~s, y1=~s, ch=~s" x y x1 y1 ch)
        (cond
          ((not (and x1 y1 (char? ch))) ; reached start of paren or vscreen
             (values #t x y))
          ((or dquote? (%char-is-scheme-identifier? ch)) ; found an identifier char, insert it and iterate
             (charspan-insert-left! stem ch)
             (%fill-stem x1 y1))
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


(define (%char-is-scheme-identifier? ch)
  (and
    (char>=? ch #\!)
    (or (char<=? #\a ch #\z)
        (char<=? #\< ch #\Z)  ; i.e. one of < = > ? @ A ... Z
        (char<=? #\- ch #\:)  ; i.e. one of - . / 0 ... 9 :
        (memv ch '(#\! #\$ #\% #\& #\* #\+ #\\ #\^ #\_ #\| #\~ ch)))))


(define (%char-is-scheme-dquote-special? ch)
  (or (char=? #\" ch) (char=? #\\ ch)))


(define (%char-is-shell-identifier? ch)
  (and
    (char>=? ch #\%)
    (or (char<=? #\a ch #\z)
        (char<=? #\@ ch #\Z)  ; i.e. one of @ A ... Z
        (char<=? #\+ ch #\:)  ; i.e. one of + , - . / 0 ... 9 :
        (memv ch '(#\% #\= #\\ #\^ #\_ #\~)))))


(define (%char-is-shell-squote-special? ch)
  (char=? #\' ch))

(define (%char-is-shell-dquote-special? ch)
  (if (memv ch '(#\" #\` #\$ #\\)) #t #f))

(define (%char-is-shell-unquoted-special? ch)
  (or (char=? #\\ ch) (not (%char-is-shell-identifier? ch))))


(define (%stem-is-after-shell-separator? screen x y)
  (let-values (((x1 y1 ch) (vscreen-char-before-xy screen x y)))
    ; (debugf "%stem-is-after-shell-separator? x=~s, y=~s -> x1=~s y1=~s ch=~s" x y x1 y1 ch)
    (cond
      ((not (and x1 y1 ch))  #f)
      ((memv ch '(#\; #\newline #\! #\& #\|)) #t)
      ((char<=? ch #\space)
        (%stem-is-after-shell-separator? screen x1 y1))
      (else #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; extract range [start, end) from charspan and convert it to string,
;; unescaping sequences according to shell double single quote rules - i.e. no character is special
(define (%unescape-scheme-dquoted csp start end)
  (let ((ret (make-charspan 0)))
    (charspan-reserve-right! ret (fx- end start))
    (let %loop ((i start))
      (let ((ch (and (fx<? i end) (charspan-ref csp i)))
            (i+1 (fx1+ i)))
        (case ch
          ((#f)
            (charspan->string*! ret))
          ((#\\)
            (if (fx<? i+1 end)
              (%loop (%unescape-scheme-chars-after-backslash csp i+1 ret))
              (%loop i+1))) ;; ignore lone #\\ at end of string
          (else
            (charspan-insert-right! ret ch)
            (%loop i+1)))))))


;; scan characters starting from position i in charspan src
;; and parse scheme string escape sequence assuming #\\ was already parsed.
;;
;; append unescaped sequence to charspan dst.
;;
;; return position of next character to parse
(define (%unescape-scheme-chars-after-backslash src i dst)
  (let ((ch  (charspan-ref src i))
        (i+1 (fx1+ i)))
    (if (char=? #\x ch)
      (%unescape-scheme-chars-hex-sequence src i+1 (charspan-length src) dst)
      (begin
        (charspan-insert-right! dst
          (case ch
            ((#\" #\\) ch)
            ((#\a) #\alarm)
            ((#\b) #\backspace)
            ((#\f) #\page)
            ((#\n) #\newline)
            ((#\r) #\return)
            ((#\t) #\tab)
            ((#\v) #\vtab)
            (else  ch)))
        i+1))))


;; scan characters starting from position i in charspan src
;; and parse scheme string hexadecimal escape sequence assuming #\\ and #\x were already parsed.
;;
;; append unescaped sequence to charspan dst.
;;
;; return position of next character to parse
(define (%unescape-scheme-chars-hex-sequence src start end dst)
  (let %next ((i start) (codepoint 0))
    (if (fx<? i end)
      (let* ((ch (charspan-ref src i))
             (n  (%hex-digit->fixnum ch)))
        (cond
          (n
            (%next (fx1+ i) (fxior n (fxarithmetic-shift-left codepoint 4))))
          ((eqv? ch #\;)
            (when (codepoint-utf8b? codepoint)
              (charspan-insert-right! dst (integer->char* codepoint)))
            (fx1+ i))
          (else
            i)))
      i)))


;; convert a character containing a hexadecimal digit
;; to the numerical value of such digit:
;;   #\0 -> 0
;;   #\1 -> 1
;;   ...
;;   #\9 -> 9
;;   #\A or #\a -> 10
;;   #\B or #\b -> 11
;;   ...
;;   #\F or #\f -> 15
;;
;; returns #f in all other cases
(define (%hex-digit->fixnum ch)
  (let ((x (char->integer ch)))
    (cond
      ((char<=? #\0 ch #\9)
        (fx- x 48))
      ((char<=? #\A ch #\F)
        (fx- x 55))
      ((char<=? #\a ch #\f)
        (fx- x 87))
      (else
        #f))))


;; extract range [start, end) from charspan and convert it to string,
;; unescaping sequences according to shell single quote rules - i.e. no character is special
(define %unescape-shell-squoted charspan->string)

;; extract range [start, end) from charspan and convert it to string,
;; unescaping sequences according to shell double quote rules:
;; each backslash must be removed after copying the next character (which may be a backslash)
(define (%unescape-shell-dquoted csp start end)
  (let ((ret (make-charspan 0)))
    (charspan-reserve-right! ret (fx- end start))
    (let %loop ((i start))
      (let ((ch (and (fx<? i end) (charspan-ref csp i)))
            (i+1 (fx1+ i)))
        (case ch
          ((#f)
            (charspan->string*! ret))
          ((#\\)
            (when (fx<? i+1 end)
              (charspan-insert-right! ret (charspan-ref csp i+1)))
            (%loop (fx1+ i+1)))
          (else
            (charspan-insert-right! ret ch)
            (%loop i+1)))))))


;; extract range [start, end) from charspan and convert it to string,
;; unescaping sequences according to shell unquoted rules
(define %unescape-shell-unquoted %unescape-shell-dquoted)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; if charspan csp contains characters that are special according to char-special-pred,
;; prefix each one with escape-str. Also append append-str to charspan.
;; Return csp modified in-place, or a new charspan.
(define (%escape csp char-special-pred escape-str append-str)
  (let* ((len (charspan-length csp))
         (ret (if (charspan-index csp 0 len char-special-pred)
                (let ((ret (make-charspan 0)))
                  (charspan-reserve-right! ret (fx+ len 8))
                  (do ((i 0 (fx1+ i)))
                      ((fx>=? i len) ret)
                    (let ((ch (charspan-ref csp i)))
                      (when (char-special-pred ch)
                        (charspan-insert-right/string! ret escape-str))
                      (charspan-insert-right! ret ch))))
                csp)))
    (charspan-insert-right/string! ret append-str)
    ret))


(define (%escape-scheme-dquoted csp)
  (%escape csp %char-is-scheme-dquote-special? "\\" "\""))

(define (%escape-shell-squoted csp)
  (%escape csp %char-is-shell-squote-special? "'\\'" "'"))

(define (%escape-shell-dquoted csp)
  (%escape csp %char-is-shell-dquote-special? "\\" "\""))

(define (%escape-shell-unquoted csp)
  (%escape csp %char-is-shell-unquoted-special? "\\" ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (%list-directory dir prefix slash? quote-func completions)
  ; (debugf "lineedit-shell-list/directory dir = ~s, prefix = ~s, quote-func = ~s" dir prefix quote-func)
  (let* ((dir?       (and slash? (not (string-empty? dir))))
         (prefix-len (string-length prefix))
         (prefix?    (not (fxzero? prefix-len)))
         (prefix-starts-with-dot? (and prefix? (char=? #\. (string-ref prefix 0)))))
    (for-list ((elem (directory-sort! (directory-list-type dir (list 'prefix prefix 'bytes 'catch)))))
      (let ((name (string->charspan* (utf8b->string (car elem)))))
        (when (or prefix-starts-with-dot? (not (char=? #\. (charspan-ref name 0))))
          (charspan-delete-left! name prefix-len)
          (when (eq? 'dir (cdr elem))
            (charspan-insert-right! name #\/))
          (span-insert-right! completions (quote-func name))))))
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
                   (substring=? name 0 prefix 1 prefix-len))
          (let ((cname (string->charspan* name)))
            (charspan-delete-left! cname prefix-len)
            (span-insert-right! completions cname))))))
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
    (for-list ((name l))
      (let ((cname (string->charspan* name)))
        (charspan-delete-left! cname prefix-len)
        (span-insert-right! completions cname))))
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
    (for-hash-keys ((name htable))
      (when (string-prefix? name prefix)
        (set! l (cons name l)))))
  l)

;; find programs in $PATH that start with prefix, cons them onto list l, and return l
(define (%list-shell-programs prefix l)
  (let* (($path      (sh-env-ref #t "PATH"))
         (dirs       (string-split $path #\:))
         (prefix-len (string-length prefix)))
    (for-list ((dir dirs))
      (for-list ((elem (directory-list-type dir (list 'prefix prefix 'catch)))) ; no need to sort directory list
        (when (eq? 'file (cdr elem))
          (set! l (cons (car elem) l))))))
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
