;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh parser shell (0 7 3))
  (export
    read-shell-char lex-shell parse-shell-word parse-shell-form1
    parse-shell-forms parser-shell)
  (import
    (rnrs)
    (only (chezscheme) append! fx1+ fx1- inspect reverse! unread-char void)
    (only (schemesh bootstrap) assert* until while)
    (only (schemesh containers misc) list-iterate string-contains-only-decimal-digits?)
    (schemesh containers charspan)
    (schemesh lineedit paren)
    (schemesh lineedit parser))

(define (paren-type->string type)
  (case type
    ((lparen) "(") ((lbrack) "[") ((lbrace) "{")
    ((rparen) ")") ((rbrack) "]") ((rbrace) "}")
    ((backquote) "`") ((dollar+lparen) "$(")
    ((eof) "#!eof")
    (else "???")))


;; Categorize a single character according to shell syntax.
;; Return the character's type.
(define (char->type ch)
  (if (eof-object? ch)
    'eof
    (case ch
      ((#\newline #\;) 'separator)
      ; TODO: is this list complete?
      ; Note: (lex-shell-impl) will change type of #\& to 'separator
      ;   unless it's followed by another #\&
      ((#\! #\& #\# #\< #\> #\|) 'op)
      ((#\" ) 'dquote)
      ((#\' ) 'squote)
      ((#\$ ) 'dollar)
      ((#\\ ) 'backslash)
      ((#\` ) 'backquote)
      ((#\( ) 'lparen)
      ((#\) ) 'rparen)
      ((#\[ ) 'lbrack)
      ((#\] ) 'rbrack)
      ((#\{ ) 'lbrace)
      ((#\} ) 'rbrace)
      (else   (if (char<=? ch #\space) 'space 'char)))))

;; Convert a character whose type is 'op or 'separator to corresponding symbol
(define (op->symbol ctx ch)
  (case ch
    ((#\newline #\;) '\x3B;)
    ((#\!) '!)
    ((#\&) '&)
    ((#\<) '<)
    ((#\>) '>)
    ((#\|) '\x7C;)
    (else (syntax-errorf ctx 'lex-shell
            "unexpected operator character ~s, cannot convert to symbol" ch))))


;; Peek a single character from textual input port 'in',
;; and categorize it according to shell syntax.
;; Return two values: character value (or eof) and its type.
(define (peek-shell-char ctx)
  (let ((ch (parsectx-peek-char ctx)))
    (values ch (char->type ch))))


;; Read a single character from textual input port 'in',
;; and categorize it according to shell syntax.
;; Return two values: character value (or eof) and its type.
(define (read-shell-char ctx)
  (let ((ch (parsectx-read-char ctx)))
    (values ch (char->type ch))))


;; Read a single character, suppressing any special meaning it may have
(define (read-char-after-backslash ctx csp-already-read)
  (let ((ch (parsectx-read-char ctx)))
    (cond
      ((eof-object? ch)
        (if csp-already-read
          (syntax-errorf ctx 'lex-shell
            "unexpected end-of-file after backslash ~a"
            (charspan->string csp-already-read))
          (syntax-errorf ctx 'lex-shell
            "unexpected end-of-file after backslash")))
      ((eqv? ch #\newline)
        ; backslash followed by newline -> ignore both
        #f)
      (#t ch))))


;; Read a subword starting with ${
(define (read-subword-dollar-braced ctx)
  (assert* 'read-subword-dollar-braced (eqv? #\{ (parsectx-read-char ctx)))
  (let ((csp (charspan))
        (again? #t))
    (while again?
      (let-values (((ch type) (read-shell-char ctx)))
        (case type
          ((eof)
            (syntax-errorf ctx 'parse-shell
              "unexpected end-of-file after ${"))
          ((rbrace)
            (set! again? #f))
          (else
            (charspan-insert-back! csp ch)))))
    (list 'shell-env (charspan->string csp))))


;; Read an unquoted subword starting with $
(define (read-subword-dollar-unquoted ctx)
  (let ((csp (charspan))
        (again? #t))
    (while again?
      (let ((ch (parsectx-read-char ctx)))
        (cond
          ((eof-object? ch)
            (set! again? #f))
          ((char=? #\\ ch)
            ; read next char, suppressing any special meaning it may have
            (let ((ch-i (read-char-after-backslash ctx csp)))
              (when ch-i (charspan-insert-back! csp ch-i))))
          ((or (char<=? #\0 ch #\9)
               (char<=? #\A ch #\Z)
               (char<=? #\a ch #\z)
               (char=?  #\_ ch))
            (charspan-insert-back! csp ch))
          (#t
            (set! again? #f)
            (parsectx-unread-char ctx ch)))))
    (list 'shell-env (charspan->string csp))))


;; Read a subword starting with $
(define (read-subword-dollar ctx)
  (assert* 'read-subword-dollar (eqv? #\$ (parsectx-read-char ctx)))
  (let-values (((ch type) (peek-shell-char ctx)))
    (case type
      ((eof)
        (syntax-errorf ctx 'parse-shell "unexpected end-of-file after $"))
      ((lparen)
        (parsectx-read-char ctx) ; consume (
        ; read a shell list surrounded by $(...)
        (let-values (((form _) (parse-shell-forms ctx 'dollar+lparen)))
          form))
      ((lbrace)
        (read-subword-dollar-braced ctx))
      (else
        (read-subword-dollar-unquoted ctx)))))


;; Read a single-quoted subword, stopping after the matching single quote.
;; Example: 'some text'
(define (read-subword-single-quoted ctx)
  (assert* 'read-subword-single-quoted (eqv? #\' (parsectx-read-char ctx)))
  (let ((csp (charspan))
        (again? #t))
    (while again?
      (let-values (((ch type) (read-shell-char ctx)))
        (case type
          ((eof squote)
            (set! again? #f)) ; newline, or end of string reached
          (else
            (charspan-insert-back! csp ch)))))
    (charspan->string csp)))


;; Read a subword AFTER double quotes, stopping BEFORE the matching double quote.
;; Example: "some text"
(define (read-subword-double-quoted ctx)
  (let ((csp (charspan))
        (again? #t))
    (while again?
      (let-values (((ch type) (read-shell-char ctx)))
        (case type
          ((eof)
            (set! again? #f))
          ((dquote dollar backquote)
            (parsectx-unread-char ctx ch)
            (set! again? #f))
          ((backslash)
            ; read next char, suppressing any special meaning it may have
            (let ((ch-i (read-char-after-backslash ctx csp)))
              (when ch-i (charspan-insert-back! csp ch-i))))
          (else
            ; single quote, newline, semicolon, operators and parentheses
            ; have no special meaning inside dquotes
            (charspan-insert-back! csp ch)))))
    (charspan->string csp)))


;; Read a single word not inside single or double quotes,
;; possibly composed by one or more subwords that should be concatenated.
;; return two values:
;;   1. a (possibly empty) list containing the parsed subwords
;;   2. either 'atom or 'rsplice. The latter means returned list should be reversed then spliced
;;      i.e. appended to command being parsed.
(define (read-subwords-noquote ctx equal-is-operator? wildcards? inside-backquote?)
  (let ((ret     '())
        (again?  #t)
        (splice? #f))
    ; (debugf ">   read-subwords-noquote equal-is-operator?=~s, wildcards?=~s, inside-backquote?=~s" equal-is-operator? wildcards? inside-backquote?)
    (while again?
      (let ((word (read-subword-noquote ctx equal-is-operator? wildcards?)))
        ; (debugf "... read-subwords-noquote subword=~s" word)
        (cond
          ((eq? word '=)
            (let-values (((words2 _) (parse-shell-word ctx #f #f #f inside-backquote?)))
              (set! ret (cons (if (null? words2) "" words2) (cons word ret))))
            (set! again? #f)
            (set! splice? #t))
          ((memq word '(% %!))
            (set! ret (cons (read-unescape-until-rbrack ctx) (cons word ret))))
          (word
            (set! ret (cons word ret)))
          (#t
            (set! again? #f)))))
    ; (debugf "<   read-subwords-noquote ret=~s splice?=~s" (reverse ret) splice?)
    (cond
      (splice?            (values ret 'rsplice))
      ((null? ret)        (values "" 'atom))
      ((%is-literal? ret) (values (car ret) 'atom))
      (#t                 (values (cons 'shell-wildcard (reverse! ret)) 'atom)))))


;; return #t if list l contains a single element that is not a wildcard
;; and thus can be simplified from (shell-wildcard x ...) to x
(define (%is-literal? l)
  (and (null? (cdr l))
       (not (memq (car l) '(~ * ?)))))


;; Read a single unquoted subword: either a string or a symbol '= '? '* '% '%!
;; returns the string or one of the symbols '= '? '* '% '%!
;; returns #f if the first character is a special character as ( ) [ ] { } ` " ' < > ; & etc.
(define (read-subword-noquote ctx equal-is-operator? wildcards?)
  (let ((word   (charspan))
        (again? #t))
    ; (debugf ">   read-subword-noquote equal-is-operator?=~s, wildcards?=~s" equal-is-operator? wildcards?)
    (while again?
      (let-values (((ch type) (read-shell-char ctx)))
        ; (debugf "... read-subword-noquote ch=~s type=~s ret=~s" ch type word)
        (cond
          ((eq? type 'backslash)
            ; read next char, suppressing any special meaning it may have
            (let ((ch2 (read-char-after-backslash ctx word)))
              (when ch2 (charspan-insert-back! word ch2))))
          ((and equal-is-operator? (eqv? ch #\=))
            (if (charspan-empty? word)
              (set! word '=)                 ; return '=
              (parsectx-unread-char ctx ch)) ; return word before =
            (set! again? #f))
          ((and wildcards? (memv ch '(#\? #\*)))
            (if (charspan-empty? word)
              ; return wildcard symbol '? or '*
              (set! word (if (char=? ch #\?) '? '*))
              (parsectx-unread-char ctx ch)) ; return word before ? or *
            (set! again? #f))
          ((eqv? ch #\~)
            (if (charspan-empty? word)
              ; return wildcard symbol '~
              (set! word '~)
              (parsectx-unread-char ctx ch)) ; return word before ~
            (set! again? #f))
          ((eq? type 'char)
            (charspan-insert-back! word ch))
          ((eq? type 'lbrack)
            (if (charspan-empty? word)
              ; return beginning of wildcard pattern '% or '%!
              (cond
                ((eqv? #\! (parsectx-peek-char ctx))
                  (parsectx-read-char ctx)
                  (set! word '%!))
                (#t
                  (set! word '%)))
              ; return word before [
              (parsectx-unread-char ctx ch))
            (set! again? #f))
          (#t
            ; treat anything else as delimiter.
            ; This means in our shell parser the characters ( ) [ ] { } retain their meaning
            ;; when found inside an unquoted string.
            ; Reason: we want to allow writing things like {ls -l | wc} without users having
            ; to worry whether semicolons are needed or not before the }.
            ;
            ; That's intentionally different from posix shell,
            ; where characters { } inside an unquoted string have no special meaning,
            ; and where characters ( ) inside an unquoted string are a syntax error.
            (parsectx-unread-char ctx ch)
            (set! again? #f)))))
    ; (debugf "<   read-subword-noquote ret=~s" word)
    (cond
      ((or (not word) (symbol? word))
        word)
      ((charspan-empty? word)
        #f)
      (#t
        (charspan->string word)))))


;; parse and return a string ending at the first unescaped ], which is consumed but not returned.
;;
;; each encountered backslash removes the special meaning of next character,
;; which is appended literally to the returned string (after removing the preceding backslash).
(define (read-unescape-until-rbrack ctx)
  (let ((word   (charspan))
        (again? #t))
    (while again?
      (let ((ch (parsectx-read-char ctx)))
        (cond
          ((or (not (char? ch)) (char=? ch #\]))
            (set! again? #f))
          ((char=? ch #\\)
            ; read next character and and append it literally
            (let ((ch2 (parsectx-read-char ctx)))
              (when (char? ch2)
                (charspan-insert-back! word ch2))))
          (#t
            (charspan-insert-back! word ch)))))
    (charspan->string word)))



;; Read a word, possibly containing single or double quotes, assignment operator =
;; wildcard operators ? * [...] and shell variable assignmens, as for example:
;;  FOO=BAR
;;  ls [ab]*.txt
;;  some$foo' text'"other text ${bar} "
;;
;; return two values: the parsed form, and either 'atom or 'splice
;; where 'splice means the parsed form should be spliced into the command being parsed.
(define (parse-shell-word ctx equal-is-operator? lbracket-is-subshell? wildcards? inside-backquote?)
  (let* ((ret '())
         (again? #t)
         (splice? #f)
         (dquote? #f)
         (%append (lambda (subword)
           (unless (and (string? subword) (fxzero? (string-length subword)))
             (set! ret (cons subword ret))))))
    ; (debugf "> parse-shell-word equal-is-operator?=~s, lbracket-is-subshell?=~s, inside-backquote?=~s" equal-is-operator? lbracket-is-subshell? inside-backquote?)
    (while again?
      (let-values (((ch type) (peek-shell-char ctx)))
        ; (debugf "... parse-shell-word ch=~s, type=~s, ret=~s" ch type ret)
        (case type
          ;; the following tokens retain their meaning also inside double quotes
          ((eof)
            (when dquote?
              (syntax-errorf ctx 'parse-shell
                "unexpected end-of-file inside double-quoted string ~s" (reverse! ret)))
            (set! again? #f))
          ((squote)
            (%append (read-subword-single-quoted ctx))
            (set! lbracket-is-subshell? #f))
          ((dquote)
            (set! dquote? (not dquote?))
            (set! lbracket-is-subshell? #f)
            (parsectx-read-char ctx))
          ((dollar)
            (%append (read-subword-dollar ctx))
            (set! lbracket-is-subshell? #f))
          ((backquote)
            (set! lbracket-is-subshell? #f)
            (if inside-backquote?
              (set! again? #f)
              (begin
                (parsectx-read-char ctx) ; consume initial `
                (let-values (((form _) (parse-shell-forms ctx type))) ; read a shell list surrounded by `...`
                  (unless (null? form)
                    (%append form))))))
          (else
            (cond
              (dquote?
                (set! lbracket-is-subshell? #f)
                (%append (read-subword-double-quoted ctx)))
              ((or (memq type '(backslash char))
                   (and (eq? type 'lbrack) (not lbracket-is-subshell?)))
                (set! lbracket-is-subshell? #f)
                (let-values (((words action)
                                (read-subwords-noquote ctx equal-is-operator? wildcards? inside-backquote?)))
                  (cond
                    ((eq? action 'rsplice)
                      (set! ret (append! words ret))
                      (set! splice? #t)
                      (set! again? #f))
                    (#t
                      (%append words)))))
              ; treat anything else as string delimiter. This means in our shell parser the
              ; characters ( ) [ ] { } retain their meaning when found inside an unquoted
              ; string.
              ; Reason: we want to allow writing things like {ls -l | wc} without users having
              ; to worry whether semicolons are needed or not before the }.
              ;
              ; That's intentionally different from posix shell,
              ; where { } inside a string are treated as regular characters,
              ; and where ( ) inside a string are a syntax error.
              (#t
                (set! again? #f)))))))

    ; (debugf "< parse-shell-word ret=~s" (reverse ret))
    (cond
      (splice?            (values (reverse! ret) 'splice))
      ((null? ret)        (values ""             'atom))
      ((%is-literal? ret) (values (car ret)      'atom))
      (#t                 (values (cons 'shell-wildcard (reverse! ret)) 'atom)))))


;; Read a single shell token from textual input port 'in'.
;; Return two values: token value and its type.
;; Does not skip initial whitespace, does not recognize parser directives #!...
;; and does not recognize numbers followed by redirection operators N< N<> N<& N> N>> N>&
;; use (lex-shell) for that.
;;
;; The definition of shell token is adapted from
;; https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_03
;;
(define (lex-shell-impl ctx equal-is-operator? lbracket-is-subshell? wildcards? inside-backquote?)
  (let-values (((ch type) (read-shell-char ctx)))
    (case type
      ((eof separator lparen rparen rbrack lbrace rbrace)
        (values ch type))
      ((op)
        (let ((ch2 (parsectx-peek-char ctx)))
          (case ch
            ((#\&) (cond ((eqv? ch2 #\&) (set! ch '&&))
                         (#t             (set! type 'separator))))
            ((#\|) (cond ((eqv? ch2 #\&) (set! ch '\x7C;&))
                         ((eqv? ch2 #\|) (set! ch '\x7C;\x7C;))))
            ((#\<) (cond ((eqv? ch2 #\>) (set! ch '<>))
                         ((eqv? ch2 #\&) (set! ch '<&))))
            ((#\>) (cond ((eqv? ch2 #\>) (set! ch '>>))
                         ((eqv? ch2 #\&) (set! ch '>&))))))
        (cond
          ((symbol? ch) ; consume peeked character
            (parsectx-read-char ctx)
            (values ch type))
          ((eqv? ch #\#) ; consume comment until end of line, then call again (lex-shell-impl)
            (parsectx-skip-line ctx)
            (lex-shell-impl ctx equal-is-operator? lbracket-is-subshell? wildcards? inside-backquote?))
          (#t  ; convert operator character to symbol
            (values (op->symbol ctx ch) type))))
      ((dollar lbrack)
        (cond
          ((and (eq? type 'dollar) (eqv? #\( (parsectx-peek-char ctx))) #| ) |# ; help vscode
            (values (parsectx-read-char ctx) 'dollar+lparen))
          ((and (eq? type 'lbrack) lbracket-is-subshell?)
            (values ch type))
          (#t
            (parsectx-unread-char ctx ch)
            (parse-shell-word ctx equal-is-operator? lbracket-is-subshell? wildcards? inside-backquote?))))
      ((backquote)
        (values ch type))
      ((char squote dquote backslash)
        (parsectx-unread-char ctx ch)
        (parse-shell-word ctx equal-is-operator? lbracket-is-subshell? wildcards? inside-backquote?))
      (else
        (syntax-errorf ctx 'lex-shell "unimplemented character type: ~a" type)))))


;; Read a single shell token from textual input port 'in'.
;; Return two values: token value and its type.
;; Skips initial whitespace, recognizes parser directives #!... and returns them th type 'parser,
;; and also recognizes numbers followed by redirection operators N< N<> N<& N> N>> N>&
;; and returns them as numbers - Joining them with subsequent redirection operator is left to (shell) macro.
(define (lex-shell ctx equal-is-operator? lbracket-is-subshell? wildcards? inside-backquote?)
  (parsectx-skip-whitespace ctx #f) ; don't skip newlines
  (let ((value (parsectx-try-read-directive ctx)))
    (if (symbol? value)
      (if (eq? 'eof value)
        ; yes, #!eof is an allowed directive:
        ; it injects (eof-object) in token stream, with type 'eof
        ; thus simulating an actual end-of-file in input port.
        ; Reason: historically used to disable the rest of a file, to help debugging
        (values (eof-object) 'eof)
        ; cannot switch to other parser here: just return it and let caller switch
        (values (get-parser ctx value 'parse-shell-forms) 'parser))

      ; read a single shell token
      (let-values (((value type) (lex-shell-impl ctx equal-is-operator? lbracket-is-subshell? wildcards? inside-backquote?)))
        ; (debugf "lex-shell value=~s type=~s" value type)
        (if (and (eq? 'atom type)
                 (string? value)
                 (string-contains-only-decimal-digits? value)
                 (memv (parsectx-peek-char ctx) '(#\< #\>)))
          (values (string->number value) 'integer) ;; integer followed by redirection operator
          (values value type))))))



;; Read a single, simple or compound shell command from textual input port 'in'
;; Stops at end-of-file.
;; Raises if multiple forms are parsed.
;;
;; Return parsed command, usually with (shell ...) prefix
;; unless the only parsed command is Scheme syntax (...)
(define (parse-shell-form1 ctx)
  (let-values (((forms _) (parse-shell-forms ctx 'eof)))
    ; forms is a 1-element list, unless #!... was found and parsed
    (cond
      ((null? forms)
        (void))
      ((and (pair? forms) (null? (cdr forms)))
        (car forms))
      (#t
        (syntax-errorf ctx 'parse-shell-form1 "expecting a single shell form, parsed ~s" forms)))))


;; Read simple or compound shell commands from textual input port 'in' until a token ) or ] or } matching
;; the specified begin-type token is found.
;; Automatically change parser when directive #!... is found.
;;
;; Return two values:
;; 1. a list containing parsed commands
;; 2. the new parser to use, or #f to continue using the same parser
;;
;; At top-level (i.e. begin-type is 'eof) usually returns ((shell...)) i.e. a list containing one form,
;; for uniformity with (parse-lisp-forms), unless a #!... directive is found,
;;
;; At nested levels (i.e. if begin-type is not 'eof) usually returns (shell...)
;; for uniformity with (parse-lisp-forms), unless a #!... directive is found,
;;
;; Raise syntax-errorf if a mismatched end token is found, as for example ']' when expecting '}'
(define (parse-shell-forms ctx begin-type)
  (let* ((ret '())
         (end-type (case begin-type
                     ((lbrace) 'rbrace)
                     ((lbrack) 'rbrack)
                     ((backquote) 'backquote)
                     ((eof)    'eof)
                     (else     'rparen)))
         (prefix (case begin-type
                     ((backquote dollar+lparen) 'shell-backquote)
                     ((lbrack)                  'shell-subshell)
                     (else                      'shell)))
         (can-change-parser?    #t)
         (equal-is-operator?    #t)
         (lbracket-is-subshell? #t)
         (inside-backquote?  (eq? 'backquote begin-type))
         (done? #f)
         (parser #f)
         (%merge (lambda (ret forms)
           ; (debugf "... parse-shell-forms > %merge ret=~s other-forms=~s" ret forms)
           (if (null? ret)
             forms
             (let ((simplified (%simplify-parse-shell-forms end-type prefix ret)))
               (let ((merged (if (null? forms) simplified (append! simplified forms))))
                 ; (debugf "... parse-shell-forms < %merge ret=~s" merged)
                 merged))))))
    ; (debugf ">   parse-shell-forms end-type=~s prefix=~s" end-type prefix)
    (until done?
      (let-values (((value type) (lex-shell ctx equal-is-operator? lbracket-is-subshell? #t inside-backquote?)))
        ; (debugf "... parse-shell-forms end-type=~s prefix=~s ret=~s value=~s type=~s" end-type prefix (if prefix (reverse ret) ret) value type)
        (set! lbracket-is-subshell? #f)
        (case type
          ((eof)
            (unless (eq? type end-type)
              (syntax-errorf ctx 'parse-shell-forms "unexpected end-of-file after ~a" (reverse! ret)))
            (set! done? #t))
          ((parser)
            ; switch to other parser until end of current list
            (unless can-change-parser?
              (syntax-errorf ctx 'parse-shell-forms
                "parser directive #!... can only appear before or after a shell command, not in the middle of it: ~a"
                (reverse! (cons (string-append "#!" (symbol->string (parser-name value))) ret))))
            (let ((other-parse-forms (parser-parse-forms value))) ; value is a parser
              (unless (eq? parse-shell-forms other-parse-forms)
                (let-values (((other-forms updated-parser) (other-parse-forms ctx begin-type)))
                  (set! parser (or updated-parser value))
                  (set! ret (%merge ret other-forms))
                  (set! done?  #t)
                  (set! prefix #f)))))
          ((separator)
            ; value can be #\& #\; or #\newline
            (set! ret (cons (if (eq? value '&) '& '\x3B;) ret))
            (set! equal-is-operator?    #t)
            (set! lbracket-is-subshell? #t))
          ((op string integer atom)
            (set! ret (cons value ret))
            (when (and (eq? type 'op)
                       (memq value '(&& \x7C;\x7C; \x7C; \x7C;&
                                      )))
              (set! equal-is-operator?    #t)
              (set! lbracket-is-subshell? #t)))
          ((splice)
            (set! ret (append! (reverse! value) ret)))
          ((backquote)
            (if (eq? type end-type)
              (set! done? #t)
              ; TODO: `...` may be followed by other words without a space
              (let-values (((form _) (parse-shell-forms ctx type)))
                (unless (null? form)
                  (set! ret (cons form ret))))))
          ((lparen)
            ; switch to Scheme parser for a single form.
            (let ((other-parse-forms (parser-parse-forms (get-parser ctx 'scheme 'parse-shell-forms))))
              (let-values (((other-forms _) (other-parse-forms ctx type)))
                (if (and (null? ret) (eq? 'eof end-type))
                  ; lparen is the first token at shell top level:
                  ; allow entering Scheme form
                  (let-values (((forms updated-parser) (%after-lisp-forms-at-top-level ctx other-forms)))
                    (set! ret forms)
                    (when updated-parser
                      (set! parser updated-parser))
                    (set! done? #t)
                    (set! prefix #f))
                  ; lparen was in the middle of shell syntax:
                  ; just insert parsed Scheme form into current shell command
                  (set! ret (cons other-forms ret))))))
          ((lbrace lbrack dollar+lparen)
            ; TODO: $(...) may be followed by other words without a space
            (let-values (((form _) (parse-shell-forms ctx type)))
              ; (debugf "... parse-shell-forms nested_form=~s ret=~s" form ret)
              (unless (null? form)
                (set! ret (cons form ret)))
              (unless (eq? 'dollar+lparen type)
                ; shell block { ... } and subshell [ ... ] end with an implicit separator ';
                (set! can-change-parser?    #t)
                (set! equal-is-operator?    #t)
                (set! lbracket-is-subshell? #t))
              ; (debugf "... parse-shell-forms nested_form    ret=~s" ret)
            ))
          ((rbrace rbrack rparen)
            (unless (eq? type end-type)
              (syntax-errorf ctx 'parse-shell-forms "unexpected token ~a, expecting ~a"
                (paren-type->string type) (paren-type->string end-type)))
            (set! done? #t))
          (else
            (syntax-errorf ctx 'parse-shell-forms "unexpected ~a ~s after ~a" type value (reverse! ret))))

        (set! can-change-parser? (eq? 'separator type))
        (set! equal-is-operator? (or (eq? 'separator type) (eq? 'splice type)))))

    ; (debugf "... parse-shell-forms end-type=~s prefix=~s ret=~s" end-type prefix (if prefix (reverse ret) ret))
    (let ((simplified (%simplify-parse-shell-forms end-type prefix ret)))
      ; (debugf "<   parse-shell-forms ret=~s" simplified)
      (values simplified parser))))



(define (%simplify-parse-shell-forms end-type prefix ret)
  (cond
    ((not prefix) ret) ; return a list containing possibly mixed shell and Scheme form(s)
    ((and (eq? 'eof end-type) (eq? 'shell prefix) (null? ret))
      ; simplify top-level (shell) -> nothing
      '())
    ((and (eq? 'eof end-type) (eq? 'shell prefix) (pair? ret)
          (null? (cdr ret)) (pair? (car ret)) (memq (caar ret) '(shell shell-subshell)))
      ; simplify top-level (shell (shell...)) -> (shell...)
      (list (car ret)))
    (#t
      (let ((form (cons prefix (reverse! ret))))
        (if (eq? 'eof end-type)
          (list form)
          form)))))


;; Convenience: lparen was the first token and begin-type is eof:
;; => omit the initial (shell ...)
;; This allows entering Scheme forms from shell syntax.
;; We still need to read until eof, and for readability the (...) must be followed by one of:
;; - eof
;; - semicolon or newline
;; - another (...)
;;
;; return two values:
;; 1. list of parsed forms
;; 2. updated parser to use, or #f if parser was not changed
(define (%after-lisp-forms-at-top-level ctx lisp-forms)
  (parsectx-skip-whitespace ctx #f) ; do not skip newlines
  (let-values (((ch type) (peek-shell-char ctx)))
    (case type
      ((eof lparen) #t)
      ((separator) (lex-shell ctx #f #f #f #f)) ; consume semicolon or newline
      (else
        (let-values (((value type) (lex-shell ctx #f #f #f #f)))
          (syntax-errorf ctx 'parse-shell-forms
            "unexpected token \"~a\" after initial (...) at shell top-level: expecting eof, newline, semicolon or another (...)"
            value)))))
  (let-values (((forms updated-parser) (parse-shell-forms ctx 'eof)))
    (values
      (cons lisp-forms forms)
      updated-parser)))




(define dollar+lparen 1)
(define dollar+lbrace 2)

;; Read until one of ( ) { } ' " ` $( is found, and return it.
;; ignore them if they are preceded by \
;; if $( is found, return $
;; Also recognize and return parser directives #!... and return them
(define (scan-shell-paren-or-directive ctx)
  (let ((ret #f)
        (prev-char #f))
    (until ret
      (let ((ch (parsectx-read-char ctx)))
        (case ch
          ((#\()    #| ) |# ; help vscode
             (set! ret (if (eqv? prev-char #\$) dollar+lparen ch)))

          ((#\{)
             (set! ret (if (eqv? prev-char #\$) dollar+lbrace ch)))

          #| ( |# ; help vscode
          ((#\) #\} #\[ #\] #\" #\' #\`) (set! ret ch))

          ((#\\)  (parsectx-read-char ctx)) ; consume one character after backslash

          ((#\#)
            (if (eqv? #\! (parsectx-read-char ctx))
              (set! ret (parsectx-read-directive ctx))
              ; #\# not followed by #\! is a comment line, skip it
              (parsectx-skip-line ctx)))

          (else
            (when (eof-object? ch)
              (set! ret ch))))
        (set! prev-char ch)))
    ret))

;; Read shell forms from textual input port (parsectx-in ctx),
;; collecting grouping tokens i.e. ( ) [ ] { } " ' ` and filling paren with them.
;;
;; If a parser directive #!... is found, switch to the corresponding parser
;; until the end of current group.
;;
;; Stops on end-of-file, or when a closing token matching the opening token
;; (paren-start-token paren) is found. Such closing token is consumed too.
;;
;; Return the updated parser to use.
(define (parse-shell-paren ctx start-ch)
  (assert* 'parse-shell-paren (parsectx? ctx))
  (when start-ch
    (assert* 'parse-shell-paren (char? start-ch)))
  (let* ((paren  (make-paren 'shell (or start-ch #t)))
         (end-ch (case start-ch ((#\() #\)) ((#\[) #\]) ((#\{) #\})
                                (else (or start-ch #t))))
         (ret    #f))

    ; (debugf ">   parse-shell-paren start-ch=~a end-ch=~a" start-ch end-ch)
    (let-values (((x y) (parsectx-previous-pos ctx (if start-ch 1 0))))
      (paren-start-xy-set! paren x y))
    (until ret
      (let ((token (scan-shell-paren-or-directive ctx)))
        (cond
          ((not token) ; not a grouping token
             #f)

          ((eqv? token end-ch) ; found matching close token
             (set! ret #t))

          ((symbol? token)
            ;; inside double quotes, parser directives are not special
            (unless (eqv? start-ch #\")
              ; recurse to other parser until end of current list
              (let* ((other-parser      (get-parser-or-false ctx token))
                     (other-parse-paren (and other-parser (parser-parse-paren other-parser)))
                     (other-paren       (and other-parse-paren (other-parse-paren ctx start-ch))))
                 ; (debugf "... parse-shell-paren other-paren=~s" other-paren)
                 (when other-paren
                   (paren-inner-append! paren other-paren)
                   (set! ret #t)))))

          ((or (fixnum? token) (memv token '(#\{ #\[ #\" #\`)))
             ;; inside double quotes, ${ is special but plain { or [ aren't.
             ;; " inside double quotes is handled above by (eqv? token end-ch)
             (unless (and (eqv? start-ch #\") (memv token '(#\{ #\[)))
               ;; recursion: call shell parser on nested list
               (let ((start-inner (cond ((eqv? token dollar+lparen) #\() #|)|#
                                        ((eqv? token dollar+lbrace) #\{)
                                        (#t                       token))))
                 (paren-inner-append! paren (parse-shell-paren ctx start-inner)))))

          ((eqv? token #\()                  #| ) |# ; help vscode
             (unless (eqv? start-ch #\")
               ;; paren not inside double quotes -> switch to scheme parser
               ;; recursion: call scheme parser on nested list
               (let* ((other-parser       (get-parser-or-false ctx 'scheme))
                      (other-parse-paren (and other-parser (parser-parse-paren other-parser)))
                      (other-paren       (if other-parse-paren
                                            (other-parse-paren ctx token)
                                            (parse-shell-paren ctx token))))
                 (when other-paren
                   (paren-inner-append! paren other-paren)))))

          ((eqv? token #\')
             ;; found single-quoted string, read it fully
             (unless (eqv? start-ch #\")
               (let ((inner (make-paren 'shell token)))
                 (let-values (((x y) (parsectx-previous-pos ctx 1)))
                   (paren-start-xy-set! inner x y))
                 (when (parsectx-skip-until-char ctx #\')
                   (paren-fill-end! ctx inner #\'))
                 (paren-inner-append! paren inner))))

          ((memv token '( #| { [ ( |#     ; help vscode
                         #\) #\] #\}))
            ;; found a close token. inside double quotes, it is not special
            (unless (eqv? start-ch #\")
              (let-values (((x y) (parsectx-previous-pos ctx 1)))
                (paren-inner-append! paren (make-paren/bad-close 'shell token x y)))))

          ((eof-object? token)
             (set! ret (if start-ch 'err #t)))

          ; ignore other tokens, including unexpected ones
          )))

    (paren-fill-end! ctx paren (and (eq? #t ret) end-ch))
    paren))


(define (paren-fill-end! ctx paren end-token)
  (let-values (((x y) (if (char? end-token)
                        (parsectx-previous-pos ctx 1)
                        (parsectx-current-pos ctx))))
    (paren-end-xy-set! paren x y)
    (paren-end-token-set! paren end-token)
    ; (debugf "shell paren-fill-end! paren=~s end-token=~s x=~s y=~s" paren end-token x y)
  ))


(define parser-shell
  (let ((ret (make-parser 'shell parse-shell-forms parse-shell-paren)))
    (lambda ()
      ret)))

) ; close library
