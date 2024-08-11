;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh parser shell (0 1))
  (export
    read-shell-char lex-shell parse-shell-word
    parse-shell parse-shell* parse-shell-list parser-shell)
  (import
    (rnrs)
    (only (chezscheme) append! fx1+ fx1- inspect reverse! unread-char void)
    (only (schemesh bootstrap) assert* debugf until while)
    (only (schemesh containers misc) string-contains-only-decimal-digits?)
    (schemesh containers charspan)
    (schemesh lineedit paren)
    (schemesh lineedit parser))

(define (paren-type->string type)
  (case type
    ((lparen) "(") ((lbrack) "[") ((lbrace) "{")
    ((rparen) ")") ((rbrack) "]") ((rbrace) "}")
    ((backquote) "`") ((dollar+lparen) "$(")
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
    ((#\newline #\;) '\x3b;)
    ((#\!) '!)
    ((#\&) '&)
    ((#\<) '<)
    ((#\>) '>)
    ((#\|) '\x7c;)
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
        (parse-shell-list ctx 'dollar+lparen '()))
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


;; Read an unquoted subword: a portion of a word, not inside single or double quotes
;; return two values: the word as a charspan and the position of first = in word,
;; or #f if = is not present.
(define (read-subword-noquote ctx)
  (let ((csp (charspan))
        (assign-pos #f)
        (again? #t))
    (while again?
      (let-values (((ch type) (read-shell-char ctx)))
        (case type
          ((backslash)
            ; read next char, suppressing any special meaning it may have
            (let ((ch-i (read-char-after-backslash ctx csp)))
              (when ch-i (charspan-insert-back! csp ch-i))))
          ((char)
            (when (and (not assign-pos) (char=? ch #\=))
              (set! assign-pos (charspan-length csp)))
            (charspan-insert-back! csp ch))
          (else
            ; treat anything else as string delimiter. This means in our shell parser the
            ; characters ( ) [ ] { } retain their meaning when found inside an unquoted
            ; string.
            ; Reason: we want to allow writing things like {ls -l | wc} without users having
            ; to worry whether semicolons are needed or not before the }.
            ;
            ; That's intentionally different from posix shell,
            ; where characters [ ] { } inside a string have no special meaning,
            ; and where characters ( ) inside a string are a syntax error.
            (set! again? #f)
            (parsectx-unread-char ctx ch)))))
    (values csp assign-pos)))



;; Read a word, possibly containing single or double quotes, assignment operator =,
;; and shell variables, as for example:
;;  FOO=BAR
;;  some$foo' text'"other text ${bar} "
;;
;; return two values: the parsed form, and either 'string or 'rlist
;; where 'rlist means the parsed form should be reversed then spliced into the command being parsed.
(define (parse-shell-word ctx equal-is-operator?)
  (let* ((ret '())
         (again? #t)
         (assign? #f)
         (dquote? #f)
         (%append (lambda (subword)
           (unless (and (string? subword) (fxzero? (string-length subword)))
             (set! ret (cons subword ret)))))
         (%simplify
           (lambda (l)
             (cond
               ((null? l)        "")
               ((null? (cdr l))  (car l))
               (#t               (cons 'shell-concat l))))))
    (while again?
      (let-values (((ch type) (peek-shell-char ctx)))
        (case type
          ((eof)
            (when dquote?
              (syntax-errorf ctx 'parse-shell
                "unexpected end-of-file inside double-quoted string ~s" (reverse! ret)))
            (set! again? #f))
          ((squote)
            (%append (if dquote? (read-subword-double-quoted ctx)
                                 (read-subword-single-quoted ctx))))
          ((dquote)
            (set! dquote? (not dquote?))
            (parsectx-read-char ctx))
          ((dollar)
            (%append (read-subword-dollar ctx)))
          (else
            (cond
              (dquote?
                (%append (read-subword-double-quoted ctx)))
              ((memq type '(backslash char))
                ;; FIXME: also consume `...` and $(...) because we must also support FOO=bar`cmd`etc
                (let-values (((csp assign-pos) (read-subword-noquote ctx)))
                  (cond
                    ((and equal-is-operator? (null? ret) (not assign?) assign-pos (not (fxzero? assign-pos)))
                      ; split "FOO=BAR" into "FOO" "BAR" and set flag assign?
                      ; (debugf "parse-shell-word splitting ~s~%" csp)
                      (%append (charspan->string/range csp 0 assign-pos))
                      (%append (charspan->string/range csp (fx1+ assign-pos) (charspan-length csp)))
                      (set! assign? #t))
                    (#t
                      (%append (charspan->string csp))))))
              ; treat anything else as string delimiter. This means in our shell parser the
              ; characters ( ) [ ] { } retain their meaning when found inside an unquoted
              ; string.
              ; Reason: we want to allow writing things like {ls -l | wc} without users having
              ; to worry whether semicolons are needed or not before the }.
              ;
              ; That's intentionally different from posix shell,
              ; where [ ] { } inside a string are treated as regular characters,
              ; and where ( ) inside a string are a syntax error.
              (#t
                (set! again? #f)))))))

    (set! ret (reverse! ret))
    (if assign?
       (values
         (list (%simplify (cdr ret)) '= (car ret))
         'rlist)
       (values
         (%simplify ret)
         'string))))


;; Read a single shell token from textual input port 'in'.
;; Return two values: token value and its type.
;; Does not skip initial whitespace, does not recognize parser directives #!...
;; and does not recognize numbers followed by redirection operators N< N<> N<& N> N>> N>&
;; use (lex-shell) for that.
;;
;; The definition of shell token is adapted from
;; https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_03
;;
(define (lex-shell-impl ctx equal-is-operator?)
  (let-values (((ch type) (read-shell-char ctx)))
    (case type
      ((eof separator lparen rparen lbrack rbrack lbrace rbrace)
        (values ch type))
      ((op)
        (let ((ch2 (parsectx-peek-char ctx)))
          (case ch
            ((#\&) (if (eqv? ch2 #\&)
                     (set! ch '&&)
                     (set! type 'separator)))
            ((#\|) (cond ((eqv? ch2 #\&) (set! ch '\x7c;&))
                         ((eqv? ch2 #\|) (set! ch '\x7c;\x7c;))))
            ((#\<) (cond ((eqv? ch2 #\>) (set! ch '<>))
                         ((eqv? ch2 #\&) (set! ch '<&))))
            ((#\>) (cond ((eqv? ch2 #\>) (set! ch '>>))
                         ((eqv? ch2 #\&) (set! ch '>&))))))
        (if (symbol? ch)
          (parsectx-read-char ctx); consume peeked character
          (set! ch (op->symbol ctx ch))) ; convert character to symbol
        (values ch type))
      ((dollar)
        (if (eqv? #\( (parsectx-peek-char ctx)) #| #\) |# ; make vscode happy
          (values (parsectx-read-char ctx) 'dollar+lparen)
          (begin
            (parsectx-unread-char ctx ch)
            (parse-shell-word ctx equal-is-operator?))))
      ((backquote)
        (values ch type))
      ((char squote dquote backslash)
        (parsectx-unread-char ctx ch)
        (parse-shell-word ctx equal-is-operator?))
      (else
        (syntax-errorf ctx 'lex-shell "unimplemented character type: ~a" type)))))


;; Read a single shell token from textual input port 'in'.
;; Return two values: token value and its type.
;; Skips initial whitespace, recognizes parser directives #!... and returns them th type 'parser,
;; and also recognizes numbers followed by redirection operators N< N<> N<& N> N>> N>&
;; and returns them as numbers - Joining them with subsequent redirection operator is left to (shell) macro.
(define (lex-shell ctx equal-is-operator?)
  (parsectx-skip-whitespace ctx #f) ; don't skip newlines
  (let ((value (try-read-parser-directive ctx)))
    (if (symbol? value)
      (if (eq? 'eof value)
        ; yes, #!eof is an allowed directive:
        ; it injects (eof-object) in token stream, with type 'eof
        ; thus simulating an actual end-of-file in input port.
        ; Reason: historically used to disable the rest of a file, to help debugging
        (values (eof-object) 'eof)
        ; cannot switch to other parser here: just return it and let caller switch
        (values (get-parser ctx value 'parse-shell) 'parser))

      ; read a single shell token
      (let-values (((value type) (lex-shell-impl ctx equal-is-operator?)))
        (if (and (eq? 'string type)
                 (string? value) ; type = 'string also allows value = `(+ ...)
                 (string-contains-only-decimal-digits? value)
                 (memv (parsectx-peek-char ctx) '(#\< #\>)))
          (values (string->number value) 'integer) ;; integer followed by redirection operator
          (values value type))))))



;; Repeatedly read from textual input port 'in' using (lex-shell)
;; and construct corresponding shell form.
;; Automatically change parser when directive #!... is found.
;;
;; Return parsed form, or new parser to use, or (eof-object) if end-of-file is reached
(define (parse-shell ctx)
  (parsectx-skip-whitespace ctx #f) ; don't skip newlines
  (let-values (((value type) (peek-shell-char ctx)))
    (case type
      ; cannot switch to other parser here, just return it and let caller switch
      ((eof parser) value)
      ((lparen)
        ; switch to Scheme parser until corresponding )
        ; also parse optional redirections after (...)
        (let ((other-parse (parser-parse (get-parser ctx 'scheme 'parse-shell))))
          (parse-shell-redirections ctx (other-parse ctx))))
      ((lbrack lbrace)
        (parsectx-read-char ctx) ; consume { or [
        ; read a shell list surrounded by {...} or [...]
        ; also parse optional redirections after {...} or [...]
        (let ((ret (parse-shell-list ctx type '())))
          (parse-shell-redirections ctx ret)))
      (else
        ; read a shell form
        (let-values (((value type) (lex-shell ctx 'equal-is-operator?)))
          (parse-shell-impl ctx value type #f))))))


;; Repeatedly read shell tokens from textual input port 'in' using (lex-shell)
;; and construct corresponding form.
;; Automatically change parser when directive #!... is found.
;;
;; Return parsed form, or new parser to use.
;;
;; Raise syntax-errorf if end-of-file is reached before completely reading a form.
(define (parse-shell* ctx)
  (let ((value (parse-shell ctx)))
    (when (eof-object? value)
      (syntax-errorf ctx 'parse-shell "unexpected end-of-file"))
    value))


;; Common backend of (parse-shell) (parse-shell*) and (parse-shell-list)
;; Does NOT support changing parser when directive #!... is found.
;;
;; Return parsed form.
(define (parse-shell-impl ctx value type is-inside-backquote?)
  (let* ((ret0 (list 'shell))
         (ret ret0)
         (again? #t)
         (reverse? #t)
         (equal-is-operator? #t)
         (%merge! (lambda (form)
           (cond
             ((eof-object? form) #f) ;; ignore (eof-object)
             ((null? ret)        (set! ret form))
             (else               (set! ret (cons form ret)))))))
    (while again?
      ; (debugf "parse-shell-impl: value = ~s, type = ~s, ret = ~s, equal-is-operator? = ~s~%" value type (if reverse? (reverse ret) ret) equal-is-operator?)
      (case type
        ((eof)
          (set! again? #f))
        ((parser)
          (unless (eq? ret ret0)
            (syntax-errorf ctx 'parse-shell
              "parser directive #!... can only appear before or after a shell command, not in the middle of it: ~a"
              (string-append "#!" (symbol->string (parser-name value)))))
          ; return parser directive #!...
          (set! ret value)
          (set! reverse? #f)
          (set! again? #f))
        ((separator)
          ; value can be '& #\newline or #\;
          (%merge! (if (eq? value '&) '& '\x3b;))
          (set! again? #f))
        ((op string integer)
          ; parsed non-assignment, it cannot be followed by further assignments
          (set! equal-is-operator? #f)
          (set! ret (cons value ret)))
        ((rlist)
          (when (and equal-is-operator? (not (memq '= value)))
            ; parsed non-assignment, it cannot be followed by further assignments
            (set! equal-is-operator? #f))
          (set! ret (append! value ret)))
        ((backquote dollar+lparen)
          (if (and is-inside-backquote? (eq? 'backquote type))
            ; we read one token too much - try to unread it
            (begin
              (parsectx-unread-char ctx value)
              (set! again? #f))
            ; parse nested shell list surrounded by `...` or $(...)
            (%merge! (parse-shell-list ctx type '()))))
        ((lparen)
          ; switch to Scheme parser for a single form.
          ; Convenience: if ( is the first token, omit the initial (shell ...)
          ; and set again? to #f. This allows entering Scheme forms from shell syntax
          (when (eq? ret ret0)
            (set! ret '())
            (set! again? #f)
            ; forms returned by (parser-parse-list) are already reversed
            (set! reverse? #f))
          (let ((other-parse-list (parser-parse-list (get-parser ctx 'scheme 'parse-shell))))
            (let ((other-ret (other-parse-list ctx type '())))
              (%merge! other-ret))))
        ((lbrack lbrace)
          (unless (or (eq? ret ret0)
                      (null? ret)
                      (memq (car ret) '(! && \x7c; \x7c;\x7c;
                                        )))
            ; characters [ { are not allowed in the middle of a shell command
            (syntax-errorf ctx 'parse-shell
              "grouping token ~a can only appear before or after a shell command, not the middle of it: ~a"
              value (reverse! (cons value ret))))
          ; parse nested shell list surrounded by {...} or [...]
          (%merge! (parse-shell-list ctx type '()))
          (set! again? #f))
        ((rparen rbrack rbrace)
          ; we read one token too much - try to unread it
          (set! again? #f)
          (parsectx-unread-char ctx value))
        (else
          (syntax-errorf ctx 'parse-shell "unexpected token type ~a after ~a"
            type (reverse! ret))))
      ; if needed, read another token and iterate
      (when again?
        (let-values (((value-i type-i) (lex-shell ctx equal-is-operator?)))
          (set! value value-i)
          (set! type type-i))))
    ; shell form is complete, return it
    (if reverse? (reverse! ret) ret)))


;; Read shell redirections from textual input port and append them to form
(define (parse-shell-redirections ctx form)
  (let* ((redirs '())
         (again? #t))
    (while again?
      (parsectx-skip-whitespace ctx #f) ; don't skip newlines
       (let ((ch (parsectx-peek-char ctx)))
          (if (and (char? ch)
                   (or (char=? #\< ch)
                       (char=? #\> ch)
                       (char<=? #\0 ch #\9)))
            (set! redirs (parse-shell-redirection ctx redirs))
            (set! again? #f))))
    (if (null? redirs)
      form
      (cons 'shell (cons form (reverse! redirs))))))


;; Read a two- or three- argument shell redirection from textual input port and prefix it to redirs
(define (parse-shell-redirection ctx redirs)
  (let-values (((value type) (lex-shell ctx #f))) ; equal-is-operator? = #f
    (if (eq? 'integer type)
      (parse-shell-redirection2 ctx (cons value redirs))
      (parse-shell-redirection1 ctx (cons value redirs)))))


;; Read a two-argument shell redirection from textual input port and prefix it to redirs
(define (parse-shell-redirection2 ctx redirs)
  (let ((ch (parsectx-peek-char ctx)))
    (if (and (char? ch)
             (or (char=? #\< ch)
                 (char=? #\> ch)))
      (let-values (((value type) (lex-shell ctx #f))) ; equal-is-operator? = #f
        (parse-shell-redirection1 ctx (cons value redirs)))
      redirs)))


;; Read the last argument of a shell redirection from textual input port and prefix it to redirs
(define (parse-shell-redirection1 ctx redirs)
  (let-values (((value type) (lex-shell ctx #f))) ; equal-is-operator? = #f
    (cons value redirs)))


;; Read shell forms from textual input port 'in' until a token } or ] or )
;; matching the specified begin-type token is found.
;; Automatically change parser when directive #!... is found.
;;
;; Return a list starting with 'shell 'shell-subshell or 'shell-backquote followed by such forms.
;; Raise syntax-errorf if mismatched end token is found, as for example ')' instead of '}'
;;
(define (parse-shell-list ctx begin-type already-parsed-reverse)
  (let* ((first-token (case begin-type
           ((backquote dollar+lparen)
             (unless (null? already-parsed-reverse)
               (syntax-errorf ctx 'parse-shell
                 "unimplemented backquote in the middle of non-shell commands, it currently can only be inside shell commands: ~a"
                 (reverse! (cons begin-type already-parsed-reverse))))
             'shell-backquote)
           ((lbrack) 'shell-subshell)
           ((lparen lbrace) 'shell) ; lparen may happen when parsing "(#!shell ...)"
           (else (syntax-errorf ctx 'parse-shell "unsupported list delimiter type ~a" begin-type))))
         (ret0 (if (null? already-parsed-reverse)
                 (list first-token)
                 already-parsed-reverse))
         (ret ret0)
         (again? #t)
         (reverse? #t)
         (end-type (case begin-type
                     ((lbrace) 'rbrace)
                     ((lbrack) 'rbrack)
                     ((backquote) 'backquote)
                     (else 'rparen)))
         (check-list-end (lambda (type)
           (unless (eq? type end-type)
             (syntax-errorf ctx 'parse-shell "unexpected token ~a, expecting ~a"
               (paren-type->string type) (paren-type->string end-type)))))
         (%merge! (lambda (form)
           ; By construction, a single-form (shell ...) can only contain & ; as last token
           ; and is not parsed from {...} or [...]
           (if (and (null? already-parsed-reverse)
                    (pair? form)
                    (eq? 'shell (car form)))
              ; flatten form into ret
              (set! ret (append! (reverse! (cdr form)) ret))
              ; add nested form to ret
              (set! ret (cons form ret))))))

    (while again?
      (let-values (((value type) (lex-shell ctx 'equal-is-operator?)))
        ; (debugf "parse-shell-list ret=~s value=~s type=~s~%" (reverse ret) value type)
        (case type
          ((eof)
            (syntax-errorf ctx 'parse-shell-list "unexpected end-of-file after ~a"
              (if reverse? (reverse! ret) ret)))
          ((parser)
            ; switch to other parser until the end of current list
            (let ((other-parse-list (parser-parse-list value)))
              (set! ret (other-parse-list ctx begin-type ret)))
            (set! reverse? #f)
            (set! again? #f))
          ((rparen rbrack rbrace)
            (check-list-end type)
            (set! again? #f))
          ((lbrace lbrack)
            ; parse nested shell list
            (set! ret (cons (parse-shell-list ctx type '()) ret)))
          ((separator)
            ; value can be '& #\newline or #\;
            (%merge! (if (eq? '& value) '& '\x3b;)))
          (else
            (if (and (eq? 'backquote begin-type) (eq? 'backquote type))
              ; end of backquote reached
              (begin
                (check-list-end type)
                (set! again? #f))
              ; parse a single shell form and accumulate it into ret
              (%merge! (parse-shell-impl ctx value type
                         (eq? 'backquote begin-type))))))))
    (if reverse? (reverse! ret) ret)))


;; Read until one of ( ) { } ' " ` $( is found, and return it.
;; ignore them if they are preceded by \
;; if $( is found, return $
;; Also recognize and return parser directives #!... and return them
(define (scan-shell-paren-or-directive ctx)
  (parsectx-skip-whitespace ctx 'also-skip-newlines)
  ;; cannot switch to other parser here: just return its name and let caller switch
  (or (try-read-parser-directive ctx)
      (scan-shell-paren ctx)))


(define dollar+lparen 1)
(define dollar+lbrace 2)

;; Read until one of ( ) [ ] { } ' " ` $( ${ is found, and return it.
;; ignore them if they are preceded by \
;; if $( is found, return value of global constant dollar+lparen
;; if ${ is found, return value of global constant dollar+lbrace
;; Does not recognize parser directives #!... use (scan-shell-paren-or-directive)
;; for that
(define (scan-shell-paren ctx)
  (let ((ret #f)
        (prev-char #f))
    (until ret
      (let ((ch (parsectx-read-char ctx)))
        (case ch
          ((#\()
             (set! ret (if (eqv? prev-char #\$) dollar+lparen ch)))
             #| make vscode happy: #\) |#

          ((#\{)
             (set! ret (if (eqv? prev-char #\$) dollar+lbrace ch)))

          #| make vscode happy: #\( |#
          ((#\) #\} #\[ #\] #\" #\' #\`) (set! ret ch))

          ((#\\)  (parsectx-read-char ctx)) ; consume one character after backslash

          ((#\#)  (parsectx-skip-line ctx))
          (else (when (eof-object? ch) (set! ret ch))))
        (set! prev-char ch)))
    ret))

;; Read shell forms from textual input port (parsectx-in ctx),
;; collecting grouping tokens i.e. ( ) [ ] { } " ' ` and filling paren with them.
;;
;; If a parser directive #!... is found, switch to the corresponding parser
;; until the end of current group.
;;
;; Stops on end-of-file, or when a closing token matching the opening token
;; (paren-token paren) is found. Such closing token is consumed too.
;;
;; Return the updated parser to use.
(define (parse-shell-paren ctx start-ch)
  (assert* 'parse-shell-paren (parsectx? ctx))
  (when start-ch
    (assert* 'parse-shell-paren (char? start-ch)))
  (let* ((paren  (make-paren 'shell start-ch))
         (end-ch (case start-ch ((#\() #\)) ((#\[) #\]) ((#\{) #\}) (else start-ch)))
         (ret    #f)
         (%paren-fill-end! (lambda (paren)
            (let-values (((x y) (parsectx-previous-pos ctx 1)))
             (paren-end-xy-set! paren x y))
           (paren-ok?-set! paren #t))))

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
            (unless (eqv? start-ch #\")
               ; recurse to other parser until end of current list
               (let* ((other-parser       (get-parser-or-false ctx token))
                      (other-parse-paren (and other-parser (parser-parse-paren other-parser)))
                      (other-paren       (and other-parse-paren (other-parse-paren ctx start-ch))))
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

          ((eqv? token #\()                  #| make vscode happy: #\) |#
             (unless (eqv? start-ch #\")
               ; paren not inside double quotes, switch to scheme parser
               ; recursion: call scheme parser on nested list
               (let* ((other-parser       (get-parser-or-false ctx 'scheme))
                      (other-parse-paren (and other-parser (parser-parse-paren other-parser)))
                      (other-paren       (if other-parse-paren
                                            (other-parse-paren ctx token)
                                            (parse-shell-paren ctx token))))
                 (when other-paren
                   (paren-inner-append! paren other-paren)))))

          ((eqv? token #\')       ; found single-quoted string
             (unless (eqv? start-ch #\")
               (let ((inner (make-paren 'shell token)))
                 (let-values (((x y) (parsectx-previous-pos ctx 1)))
                   (paren-start-xy-set! inner x y))
                 (when (parsectx-skip-until-char ctx #\')
                   (%paren-fill-end! inner)
                   (paren-inner-append! paren inner)))))

          ((eof-object? token)
             (set! ret 'err))

          ; ignore unexpected tokens
          )))

    (when (or (eq? #t ret) (not start-ch))
      (%paren-fill-end! paren))
    paren))


(define parser-shell
  (let ((ret (make-parser 'shell parse-shell parse-shell* parse-shell-list parse-shell-paren)))
    (lambda ()
      ret)))

) ; close library
