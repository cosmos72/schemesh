;;; Copyright (C) 2023 by Massimiliano Ghilardi
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
    (only (chezscheme) fx1+ fx1- inspect reverse! unread-char)
    (only (schemesh bootstrap) until while)
    (schemesh containers charspan)
    (schemesh lineedit parens)
    (schemesh lineedit parser))

(define (paren-type->string type)
  (case type
    ((lparen) "(") ((lbrack) "[") ((lbrace) "{")
    ((rparen) ")") ((rbrack) "]") ((rbrace) "}")
    ((backquote) "`") ((dollar+lparen) "$(")
    (else "???")))


; Categorize a single character according to shell syntax.
; Return the character's type.
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

; Convert a character whose type is 'op or 'separator to corresponding symbol
(define (op->symbol ctx ch)
  (case ch
    ((#\newline #\;) '\x3c;)
    ((#\!) '!)
    ((#\&) '&)
    ((#\<) '<)
    ((#\>) '>)
    ((#\|) '\x7c;)
    (else (syntax-errorf ctx 'lex-shell
            "unexpected operator character, cannot convert to symbol" ch))))


; Peek a single character from textual input port 'in',
; and categorize it according to shell syntax.
; Return two values: character value (or eof) and its type.
(define (peek-shell-char ctx)
  (let ((ch (parsectx-peek-char ctx)))
    (values ch (char->type ch))))


; Read a single character from textual input port 'in',
; and categorize it according to shell syntax.
; Return two values: character value (or eof) and its type.
(define (read-shell-char ctx)
  (let ((ch (parsectx-read-char ctx)))
    (values ch (char->type ch))))


; Read a single character, suppressing any special meaning it may have
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


; Read a subword starting with ${
(define (read-subword-dollar-braced ctx)
  (assert (eqv? #\{ (parsectx-read-char ctx)))
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
    (list 'shell-env-ref (charspan->string csp))))


; Read an unquoted subword starting with $
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
    (list 'shell-env-ref (charspan->string csp))))


; Read a subword starting with $
(define (read-subword-dollar ctx)
  (assert (eqv? #\$ (parsectx-read-char ctx)))
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


; Read a single-quoted subword, stopping after the matching single quote.
; Example: 'some text'
(define (read-subword-single-quoted ctx)
  (assert (eqv? #\' (parsectx-read-char ctx)))
  (let ((csp (charspan))
        (again? #t))
    (while again?
      (let ((ch (parsectx-read-char ctx)))
        (cond
          ((eof-object? ch)
            (syntax-errorf ctx 'lex-shell "unexpected end-of-file inside single-quoted string '~a'"
              (charspan->string csp)))
          ((eqv? ch #\')
            (set! again? #f)) ; end of string reached
          (#t
            (charspan-insert-back! csp ch)))))
    (charspan->string csp)))


; Read a subword AFTER double quotes, stopping BEFORE the matching double quote.
; Example: "some text"
(define (read-subword-in-double-quotes ctx)
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


; Read an unquoted subword: a portion of a word, not inside single or double quotes
(define (read-subword-noquote ctx)
  (let ((csp (charspan))
        (again? #t))
    (while again?
      (let-values (((ch type) (read-shell-char ctx)))
        (case type
          ((backslash)
          ; read next char, suppressing any special meaning it may have
            (let ((ch-i (read-char-after-backslash ctx csp)))
              (when ch-i (charspan-insert-back! csp ch-i))))
          ((char)
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
    (charspan->string csp)))


; Read a word, possibly containing single or double quotes and shell variables,
; as for example: some$foo' text'"other text ${bar} "
(define (parse-shell-word ctx)
  (let* ((ret '())
         (again? #t)
         (dquote? #f)
         (%append (lambda (subword)
           (unless (and (string? subword) (fxzero? (string-length subword)))
             (set! ret (cons subword ret))))))
    (while again?
      (let-values (((ch type) (peek-shell-char ctx)))
        (case type
          ((eof)
            (when dquote?
              (syntax-errorf ctx 'parse-shell
                "unexpected end-of-file inside double-quoted string ~s" (reverse! ret)))
            (set! again? #f))
          ((squote)
            (%append (if dquote? (read-subword-in-double-quotes ctx)
                                 (read-subword-single-quoted ctx))))
          ((dquote)
            (set! dquote? (not dquote?))
            (parsectx-read-char ctx))
          ((dollar)
            (%append (read-subword-dollar ctx)))
          (else
            (cond
              (dquote?
                (%append (read-subword-in-double-quotes ctx)))
              ((memq type '(backslash char))
                (%append (read-subword-noquote ctx)))
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
    (cond
      ((null? ret)       "")
      ((null? (cdr ret)) (car ret))
      (#t  (cons 'shell-concat (reverse! ret))))))


;; Read a single shell token from textual input port 'in'.
;; Return two values: token value and its type.
;; Does not skip initial whitespace, does not recognize parser directives #!...
;; and does not recognize numbers followed by redirection operators N< N<> N<& N> N>> N>&
;; use (lex-shell) for that.
;;
;; The definition of shell token is adapted from
;; https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_03
;;
(define (lex-shell-impl ctx)
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
            (values (parse-shell-word ctx) 'string))))
      ((backquote)
        (values ch type))
      ((char squote dquote backslash)
        (parsectx-unread-char ctx ch)
        (values (parse-shell-word ctx) 'string))
      (else
        (syntax-errorf ctx 'lex-shell "unimplemented character type: ~a" type)))))


;; Read a single shell token from textual input port 'in'.
;; Return two values: token value and its type.
;; Skips initial whitespace, recognizes parser directives #!... and returns them th type 'parser,
;; and also recognizes numbers followed by redirection operators N< N<> N<& N> N>> N>&
;; and returns them as numbers - Joining them with subsequent redirection operator is left to (shell) macro.
(define (lex-shell ctx)
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
      (let-values (((value type) (lex-shell-impl ctx)))
        (if (and (eq? 'string type)
                 (string? value) ; type = 'string also allows value = `(shell-concat ...)
                 (string-contains-only-decimal-digits? value)
                 (memv (parsectx-peek-char ctx) '(#\< #\>)))
          (values (string->number value) 'integer) ;; integer followed by redirection operator
          (values value type))))))

;; return #t if string is non-empty and only contains decimal digits
(define (string-contains-only-decimal-digits? str)
  (let ((n (string-length str)))
    (if (fxzero? n)
      #f
      (do ((i 0 (fx1+ i)))
          ((or (fx>=? i n) (not (decimal-digit? (string-ref str i))))
             (fx>=? i n))))))

;; return #t if character is a decimal digit 0..9
(define (decimal-digit? ch)
  (char<=? #\0 ch #\9))


; Repeatedly read from textual input port 'in' using (lex-shell)
; and construct corresponding shell form.
; Automatically change parser when directive #!... is found in a nested list.
;
; Return two values: parsed form, and #t.
; If end-of-file is reached, return (eof-object) and #f.
(define (parse-shell ctx)
  (let-values (((value type) (lex-shell ctx)))
    (values
      (case type
        ; cannot switch to other parser here: just return it and let caller switch
        ((eof parser) value)
        ((lbrace)
          ; read a shell list surrounded by {...}
          (parse-shell-list ctx type '()))
        (else
          (parse-shell-impl ctx value type #f)))
      (not (eq? 'eof type)))))


; Repeatedly read shell tokens from textual input port 'in' using (lex-shell)
; and construct corresponding form.
; Automatically change parser when directive #!... is found in a nested list.
;
; Return parsed form.
; Raise syntax-errorf if end-of-file is reached before completely reading a form.
(define (parse-shell* ctx)
  (let-values (((value ok) (parse-shell ctx)))
    (unless ok
      (syntax-errorf ctx 'parse-shell "unexpected end-of-file"))
    (when (parser? value)
      (syntax-errorf ctx 'parse-shell
        "parser directive #!... can only appear in shell command lists, not in single-command contexts: ~a"
        (string-append "#!" (symbol->string (parser-name value)))))
    value))

; Common backend of (parse-shell) (parse-shell*) and (parse-shell-list)
(define (parse-shell-impl ctx value type is-inside-backquote?)
  (let ((ret (list 'shell))
        (again? #t)
        (reverse? #t))
    (while again?
      ; (format #t "parse-shell-impl: value = ~s, type = ~s~%" value type)
      (case type
        ((eof)
          (set! again? #f))
        ((parser)
          (syntax-errorf ctx 'parse-shell
            "parser directive #!... can only appear before or after a shell command, not in the middle of it: ~a"
            (string-append "#!" (symbol->string (parser-name value)))))
        ((separator)
          (cond
            ((equal? '(shell) ret)
              ; return a lone '& or ';
              (set! ret (if (eq? '& value) value '\x3b;))
              (set! reverse? #f))
            ((eq? value '&)
              ; leave & for next call to parse-shell or parse-shell-list
              (parsectx-unread-char ctx #\&)))
          (set! again? #f))
        ((op string integer)
          (set! ret (cons value ret)))
        ((backquote dollar+lparen)
          (if (and is-inside-backquote? (eq? 'backquote type))
            ; we read one token too much - try to unread it
            (begin
              (set! again? #f)
              (parsectx-unread-char ctx value))
            ; parse nested shell list surrounded by `...` or $(...)
            (set! ret (cons (parse-shell-list ctx type '()) ret))))
        ((lparen lbrack)
          ; switch to Scheme parser for a single form.
          ; Convenience: if the first word is #\(, omit the initial (shell ...)
          ; and set again? to #f. This allows entering Scheme forms from shell syntax
          (when (equal? '(shell) ret)
            (set! ret '())
            (set! again? #f)
            ; forms returned by (parser-parse-list) are already reversed
            (set! reverse? #f))
          (let* ((other-parse-list (parser-parse-list
                   (get-parser ctx 'scheme 'parse-shell)))
                 (form (other-parse-list ctx type '())))
            (set! ret (if (null? ret) form (cons form ret)))))
        ((lbrace)
          (if (or (null? (cdr ret)) (memq (car ret) '(! && \x7c; \x7c;\x7c;
                                                     )))
            ; parse nested shell list surrounded by {...}
            (begin
              (set! again? #f)
              (set! ret (cons (parse-shell-list ctx type '()) ret)))
            ; character { is not allowed in the middle of a shell command
            (syntax-errorf ctx 'parse-shell
              "misplaced { in the middle of shell command, can only be at the beginning: ~a"
              (reverse! (cons value ret)))))
        ((rparen rbrack rbrace)
          ; we read one token too much - try to unread it
          (set! again? #f)
          (parsectx-unread-char ctx value))
        (else
          (syntax-errorf ctx 'parse-shell "unexpected token type ~a after ~a"
            type (reverse! ret))))
      ; if needed, read another token and iterate
      (when again?
        (let-values (((value-i type-i) (lex-shell ctx)))
          (set! value value-i)
          (set! type type-i))))
    ; shell form is complete, return it
    (if reverse? (reverse! ret) ret)))


; Read shell forms from textual input port 'in' until a token } or ] or )
; matching the specified begin-type token is found.
; Automatically change parser when directive #!... is found.
;
; Return a list containing 'shell-list followed by such forms.
; Raise syntax-errorf if mismatched end token is found, as for example ')' instead of '}'
;
(define (parse-shell-list ctx begin-type already-parsed-reverse)
  (let* ((first-token (case begin-type
           ((backquote dollar+lparen)
             (unless (null? already-parsed-reverse)
               (syntax-errorf ctx 'parse-shell
                 "unimplemented backquote in the middle of non-shell commands, it currently can only be inside shell commands: ~a"
                 (reverse! (cons begin-type already-parsed-reverse))))
             'shell-backquote)
           (else 'shell-list)))
         (ret (if (null? already-parsed-reverse)
                (cons first-token already-parsed-reverse)
                already-parsed-reverse))
         (again? #t)
         (reverse? #t)
         (end-type (case begin-type
                     ((lbrace) 'rbrace) ((lbrack) 'rbrack)
                     ((backquote) 'backquote) (else 'rparen)))
         (check-list-end (lambda (type)
           (unless (eq? type end-type)
             (syntax-errorf ctx 'parse-shell "unexpected token ~a, expecting ~a"
               paren-type->string type) (paren-type->string end-type)))))
    (while again?
      (let-values (((value type) (lex-shell ctx)))
        ; (format #t "parse-shell-list ret=~s value=~s type=~s~%" (reverse ret) value type)
        (case type
          ((eof)
            (if begin-type
              (syntax-errorf ctx 'parse-shell-list "unexpected end-of-file after ~a"
                (if reverse? (reverse! ret) ret)))
              (set! again? #f))
          ((parser)
            ; switch to other parser until the end of current list
            (let ((other-parse-list (parser-parse-list value)))
              (set! ret (other-parse-list ctx begin-type ret)))
            (set! reverse? #f)
            (set! again? #f))
          ((rparen rbrack rbrace)
            (check-list-end type)
            (set! again? #f))
          ((lbrace)
            ; parse nested shell list
            (let ((nested-list (parse-shell-list ctx type '())))
              (set! ret (cons nested-list ret))))
          ((separator)
            ; ignore separators, except & that must be honored
            (when (eq? '& value)
              (set! ret (cons value ret))))
          (else
            (if (and (eq? 'backquote begin-type) (eq? 'backquote type))
              ; end of backquote reached
              (begin
                (check-list-end type)
                (set! again? #f))
              ; parse a single shell form and accumulate it into ret
              (let ((value (parse-shell-impl ctx value type
                             (eq? 'backquote begin-type))))
                (set! ret (cons value ret))))))))
    (if reverse? (reverse! ret) ret)))


;; Read until one of ( ) { } ' " ` $( is found, and return it.
;; ignore them if they are preceded by \
;; if $( is found, return $
;; Also recognize and return parser directives #!... and return them
(define (scan-shell-parens-or-directive ctx)
  (parsectx-skip-whitespace ctx 'also-skip-newlines)
  ;; cannot switch to other parser here: just return its name and let caller switch
  (or (try-read-parser-directive ctx)
      (scan-shell-parens ctx)))


(define dollar+lparen 1)
(define dollar+lbrace 2)

;; Read until one of ( ) { } ' " ` $( ${ is found, and return it.
;; ignore them if they are preceded by \
;; if $( is found, return value of global constant dollar+lparen
;; if ${ is found, return value of global constant dollar+lbrace
;; Does not recognize parser directives #!... use (scan-shell-parens-or-directive)
;; for that
(define (scan-shell-parens ctx)
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
          ((#\) #\} #\" #\' #\`) (set! ret ch))

          ((#\\)  (parsectx-read-char ctx)) ; consume one character after backslash

          ((#\#)  (parsectx-skip-whitespace ctx #f))
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
;; (parens-token paren) is found. Such closing token is consumed too.
;;
;; Return the updated parser to use.
(define (parse-shell-parens ctx start-ch)
  (assert (parsectx? ctx))
  (when start-ch
    (assert (char? start-ch)))
  (let* ((paren  (make-parens 'shell start-ch))
         (end-ch (case start-ch ((#\() #\)) ((#\[) #\]) ((#\{) #\}) (else start-ch)))
         (pos    (parsectx-pos ctx))
         (ret    #f)
         (%paren-fill-end! (lambda (paren)
           (parens-end-x-set! paren (fx1- (car pos)))
           (parens-end-y-set! paren (cdr pos))
           (parens-ok?-set! paren #t))))
    (parens-start-x-set! paren (fx- (car pos) (if start-ch 1 0)))
    (parens-start-y-set! paren (cdr pos))
    (until ret
      (let ((token (scan-shell-parens-or-directive ctx)))
        (cond
          ((not token) ; not a grouping token
             #f)

          ((eqv? token end-ch) ; found matching close token
             (set! ret #t))

          ((symbol? token)
            (unless (eqv? start-ch #\")
               ; recurse to other parser until end of current list
               (let* ((other-parser       (get-parser-or-false ctx token))
                      (other-parse-parens (and other-parser (parser-parse-parens other-parser)))
                      (other-parens       (and other-parse-parens (other-parse-parens ctx start-ch))))
                  (when other-parens
                    (parens-inner-append! paren other-parens)
                    (set! ret #t)))))

          ((or (fixnum? token) (memv token '(#\{ #\" #\`)))
             ;; inside double quotes, ${ is special but plain { isn't.
             ;; " inside double quotes is handled above by (eqv? token end-ch)
             (unless (and (eqv? token #\{) (eqv? start-ch #\"))
               ;; recursion: call shell parser on nested list
               (let ((start-inner (cond ((eqv? token dollar+lparen) #\() #|)|#
                                        ((eqv? token dollar+lbrace) #\{)
                                        (#t                       token))))
                 (parens-inner-append! paren (parse-shell-parens ctx start-inner)))))

          ((eqv? token #\()                  #| make vscode happy: #\) |#
             (unless (eqv? start-ch #\")
               ; parens not inside double quotes, switch to scheme parser
               ; recursion: call scheme parser on nested list
               (let* ((other-parser       (get-parser-or-false ctx 'scheme))
                      (other-parse-parens (and other-parser (parser-parse-parens other-parser)))
                      (other-parens       (if other-parse-parens
                                            (other-parse-parens ctx token)
                                            (parse-shell-parens ctx token))))
                 (when other-parens
                   (parens-inner-append! paren other-parens)))))

          ((eqv? token #\')       ; found single-quoted string
             (unless (eqv? start-ch #\")
               (let ((inner (make-parens 'shell token)))
                 (parens-start-x-set! inner (fx- (car pos) 1))
                 (parens-start-y-set! inner (cdr pos))
                 (when (parsectx-skip-until-char ctx #\')
                   (%paren-fill-end! inner)
                   (parens-inner-append! paren inner)))))

          ((eof-object? token)
             (set! ret 'err))

          ; ignore unexpected tokens
          )))

    (when (or (eq? #t ret) (not start-ch))
      (%paren-fill-end! paren))
    paren))


(define parser-shell
  (let ((ret (make-parser 'shell parse-shell parse-shell* parse-shell-list parse-shell-parens)))
    (lambda ()
      ret)))

) ; close library
