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
    (only (chezscheme) reverse! unread-char)
    (only (schemesh bootstrap) while)
    (schemesh containers charspan)
    (schemesh parser base))

(define (paren-type->string type)
  (case type
    ((lparen) "(") ((lbrack) "[") ((lbrace) "{")
    ((rparen) ")") ((rbrack) "]") ((rbrace) "}")
    ((backquote) "`") ((dollar+lparen) "$(")
    (else "???")))

;
; Categorize a single character according to shell syntax.
; Return the character's type.
;/
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
      ((#\' ) 'quote)
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
(define (op->symbol ch)
  (case ch
    ((#\newline #\;) '\x3c;)
    ((#\!) '!)
    ((#\&) '&)
    ((#\<) '<)
    ((#\>) '>)
    ((#\|) '\x7c;)
    (else (syntax-violation 'lex-shell
            "unexpected operator character, cannot convert to symbol" ch))))


; Peek a single character from textual input port 'in',
; and categorize it according to shell syntax.
; Return two values: character value (or eof) and its type.
(define (peek-shell-char in)
  (let ((ch (peek-char in)))
    (values ch (char->type ch))))


; Read a single character from textual input port 'in',
; and categorize it according to shell syntax.
; Return two values: character value (or eof) and its type.
(define (read-shell-char in)
  (let ((ch (read-char in)))
    (values ch (char->type ch))))


; Read a single character, suppressing any special meaning it may have
(define (read-char-after-backslash in csp-already-read)
  (let ((ch (read-char in)))
    (cond
      ((eof-object? ch)
        (syntax-violation 'lex-shell
          "unexpected end-of-file after backslash"
          (if csp-already-read (charspan->string csp-already-read) "")
          'eof))
      ((eqv? ch #\newline)
        ; backslash followed by newline -> ignore both
        #f)
      (#t ch))))


; Read a subword starting with ${
(define (read-subword-dollar-braced in)
  (assert (eqv? #\{ (read-char in)))
  (let ((csp (charspan))
        (again? #t))
    (while again?
      (let-values (((ch type) (read-shell-char in)))
        (case type
          ((eof)
            (syntax-violation 'parse-shell
              "unexpected end-of-file after ${" type))
          ((rbrace)
            (set! again? #f))
          (else
            (charspan-insert-back! csp ch)))))
    (list 'shell-env-ref (charspan->string csp))))


; Read an unquoted subword starting with $
(define (read-subword-dollar-unquoted in)
  (let ((csp (charspan))
        (again? #t))
    (while again?
      (let ((ch (read-char in)))
        (cond
          ((eof-object? ch)
            (set! again? #f))
          ((char=? #\\ ch)
            ; read next char, suppressing any special meaning it may have
            (let ((ch-i (read-char-after-backslash in csp)))
              (when ch-i (charspan-insert-back! csp ch-i))))
          ((or (char<=? #\0 ch #\9)
               (char<=? #\A ch #\Z)
               (char<=? #\a ch #\z)
               (char=?  #\_ ch))
            (charspan-insert-back! csp ch))
          (#t
            (set! again? #f)
            (try-unread-char ch in)))))
    (list 'shell-env-ref (charspan->string csp))))


; Read a subword starting with $
(define (read-subword-dollar in enabled-parsers)
  (assert (eqv? #\$ (read-char in)))
  (let-values (((ch type) (peek-shell-char in)))
    (case type
      ((eof)
        (syntax-violation 'parse-shell "unexpected end-of-file after $" 'eof))
      ((lparen)
        (read-char in) ; consume (
        ; read a shell list surrounded by $(...)
        (parse-shell-list 'dollar+lparen in '() enabled-parsers))
      ((lbrace)
        (read-subword-dollar-braced in))
      (else
        (read-subword-dollar-unquoted in)))))


; Read a single-quoted subword, stopping after the matching single quote.
; Example: 'some text'
(define (read-subword-quoted in)
  (assert (eqv? #\' (read-char in)))
  (let ((csp (charspan))
        (again? #t))
    (while again?
      (let ((ch (read-char in)))
        (cond
          ((eof-object? ch)
            (syntax-violation 'lex-shell "unexpected end-of-file inside quoted string:"
              (charspan->string csp)))
          ((eqv? ch #\')
            (set! again? #f)) ; end of string reached
          (#t
            (charspan-insert-back! csp ch)))))
    (charspan->string csp)))


; Read a subword AFTER double quotes, stopping BEFORE the matching double quote.
; Example: "some text"
(define (read-subword-inside-dquotes in)
  (let ((csp (charspan))
        (again? #t))
    (while again?
      (let-values (((ch type) (read-shell-char in)))
        (case type
          ((eof)
            (set! again? #f))
          ((dquote dollar backquote)
            (try-unread-char ch in)
            (set! again? #f))
          ((backslash)
            ; read next char, suppressing any special meaning it may have
            (let ((ch-i (read-char-after-backslash in csp)))
              (when ch-i (charspan-insert-back! csp ch-i))))
          (else
            ; single quote, newline, semicolon, operators and parentheses
            ; have no special meaning inside dquotes
            (charspan-insert-back! csp ch)))))
    (charspan->string csp)))


; Read an unquoted subword: a portion of a word, not inside single or double quotes
(define (read-subword-noquote in)
  (let ((csp (charspan))
        (again? #t))
    (while again?
      (let-values (((ch type) (read-shell-char in)))
        (case type
          ((backslash)
          ; read next char, suppressing any special meaning it may have
            (let ((ch-i (read-char-after-backslash in csp)))
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
            (try-unread-char ch in)))))
    (charspan->string csp)))

; Read a word, possibly containing single or double quotes and shell variables,
; as for example: some$foo' text'"other text ${bar} "
(define (parse-shell-word in enabled-parsers)
  (let* ((ret '())
         (again? #t)
         (dquote? #f)
         (%append (lambda (subword)
           (unless (and (string? subword) (fxzero? (string-length subword)))
             (set! ret (cons subword ret))))))
    (while again?
      (let-values (((ch type) (peek-shell-char in)))
        (case type
          ((eof)
            (when dquote?
              (syntax-violation 'parse-shell
                "unexpected end-of-file inside quoted string" (reverse! ret) type))
            (set! again? #f))
          ((quote)
            (%append (if dquote? (read-subword-inside-dquotes in)
                                 (read-subword-quoted in))))
          ((dquote)
            (set! dquote? (not dquote?))
            (read-char in))
          ((dollar)
            (%append (read-subword-dollar in enabled-parsers)))
          (else
            (cond
              (dquote?
                (%append (read-subword-inside-dquotes in)))
              ((memq type '(backslash char))
                (%append (read-subword-noquote in)))
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


; Read a single shell token from textual input port 'in'.
; Return two values: token value and its type.
; Does not skip initial whitespace, and does not recognize parser directives #!...
; use (lex-shell) for that.
;
; The definition of shell token is adapted from
; https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_03
;
(define (lex-shell-impl in enabled-parsers)
  (let-values (((ch type) (read-shell-char in)))
    (case type
      ((eof separator lparen rparen lbrack rbrack lbrace rbrace)
        (values ch type))
      ; TODO: also handle multi-character operators #... N< N<> N<& N> N>> N>| N>&
      ((op)
        (let ((ch2 (peek-char in)))
          (case ch
            ((#\&) (if (eqv? ch2 #\&)
                      (set! ch '&&)
                      (set! type 'separator)))
            ((#\|) (cond ((eqv? ch2 #\&) (set! ch '\x7c;&))
                          ((eqv? ch2 #\|) (set! ch '\x7c;\x7c;))))
            ((#\<) (cond ((eqv? ch2 #\>) (set! ch '<>))
                          ((eqv? ch2 #\&) (set! ch '<&))))
            ((#\>) (cond ((eqv? ch2 #\>) (set! ch '>>))
                          ((eqv? ch2 #\&) (set! ch '>&))
                          ((eqv? ch2 #\|) (set! ch '>\x7c;))))))
        (if (symbol? ch)
          (read-char in); consume peeked character
          (set! ch (op->symbol ch))) ; convert character to symbol
        (values ch type))
      ((dollar)
        (if (eqv? #\( (peek-char in))
          (values (read-char in) 'dollar+lparen)
          (begin
            (try-unread-char ch in)
            (values (parse-shell-word in enabled-parsers) 'string))))
      ((backquote)
        (values ch type))
      ((char quote dquote backslash)
     ;  TODO: handle ~ and path-based wildcards
        (try-unread-char ch in)
        (values (parse-shell-word in enabled-parsers) 'string))
      (else
        (syntax-violation 'lex-shell "unimplemented character type:" type)))))
;
; Read a single shell token from textual input port 'in'.
; Return two values: token value and its type.
; Also recognizes parser directives #!... and returns them with type 'parser.
;/
(define (lex-shell in enabled-parsers)
  (skip-whitespace in #f) ; don't skip newlines
  (let ((value (try-read-parser-directive in)))
    (if (symbol? value)
      (if (eq? 'eof value)
     ;   yes, #!eof is an allowed directive:
     ;   it injects (eof-object) in token stream, with type 'eof
     ;   thus simulating an actual end-of-file in input port.
     ;   Reason: historically used to disable the rest of a file, to help debugging
        (values (eof-object) 'eof)
     ;   cannot switch to other parser here: just return it and let caller switch
        (values (get-parser value enabled-parsers 'parse-shell) 'parser))
     ; read a single shell token
      (lex-shell-impl in enabled-parsers))))

;
; Repeatedly read from textual input port 'in' using (lex-shell)
; and construct corresponding shell form.
; Automatically change parser when directive #!... is found in a nested list.
;
; Return two values: parsed form, and #t.
; If end-of-file is reached, return (eof-object) and #f.
;/
(define (parse-shell in enabled-parsers)
  (let-values (((value type) (lex-shell in enabled-parsers)))
    (values
      (case type
     ;   cannot switch to other parser here: just return it and let caller switch
        ((eof parser) value)
        ((lbrace)
          ; read a shell list surrounded by {...}
          (parse-shell-list type in '() enabled-parsers))
        (else
          (parse-shell-impl value type in enabled-parsers #f)))
      (not (eq? 'eof type)))))

;
; Repeatedly read shell tokens from textual input port 'in' using (lex-shell)
; and construct corresponding form.
; Automatically change parser when directive #!... is found in a nested list.
;
; Return parsed form.
; Raise syntax-violation if end-of-file is reached before completely reading a form.
;/
(define (parse-shell* in enabled-parsers)
  (let-values (((value ok) (parse-shell in enabled-parsers)))
    (unless ok
      (syntax-violation 'parse-shell* "unexpected end-of-file" 'eof))
    (when (parser? value)
      (syntax-violation 'parse-shell
        "parser directive #!... can only appear in lists, not in single-form contexts: #!"
        (parser-name value)))
    value))

; Common backend of (parse-shell) (parse-shell*) and (parse-shell-list)
(define (parse-shell-impl value type in enabled-parsers is-inside-backquote?)
  (let ((ret (list 'shell))
        (again? #t)
        (reverse? #t))
    (while again?
      ; (format #t "parse-shell-impl: value = ~s, type = ~s~%" value type)
      (case type
        ((eof)
          (set! again? #f))
        ((parser)
          (syntax-violation 'parse-shell
            "parser directive #!... can only appear before or after a shell command, not in the middle of it: #!"
            (parser-name value)))
        ((separator)
          (when (eq? value '&) ; append final & to command
            (set! ret (cons value ret)))
          (set! again? #f))
        ((op string)
          (set! ret (cons value ret)))
        ((backquote dollar+lparen)
          (if (and is-inside-backquote? (eq? 'backquote type))
            ; we read one token too much - try to unread it
            (begin
              (set! again? #f)
              (try-unread-char value in))
            ; parse nested shell list surrounded by `...` or $(...)
            (set! ret (cons (parse-shell-list type in '() enabled-parsers) ret))))
        ((lparen lbrack)
          ; switch to Scheme parser for a single form.
          ; Convenience: if the first word is #\(, omit the initial (shell ...)
          ; and set again? to #f. This allows entering Scheme forms from shell syntax
          (when (equal? '(shell) ret)
            (set! ret '())
            (set! again? #f)
          ;   forms returned by (parser-parse-list) are already reversed
            (set! reverse? #f))
          (let* ((other-parse-list (parser-parse-list
                   (get-parser 'scheme enabled-parsers 'parse-shell)))
                 (form (other-parse-list type in '() enabled-parsers)))
            (set! ret (if (null? ret) form (cons form ret)))))
        ((lbrace)
          (if (or (null? (cdr ret)) (memq (car ret) '(! & && \x7c; \x7c;\x7c;)))
            ; parse nested shell list surrounded by {...}
            (begin
              (set! again? #f)
              (set! ret (cons (parse-shell-list type in '() enabled-parsers) ret)))
            ; character { is not allowed in the middle of a shell command
            (syntax-violation 'parse-shell
              "misplaced { in the middle of shell command, can only be at the beginning:"
              (reverse! (cons value ret)) type)))
        ((rparen rbrack rbrace)
          ; we read one token too much - try to unread it
          (set! again? #f)
          (try-unread-char value in))
        (else
          (syntax-violation 'parse-shell "unexpected token type"
            (reverse! ret) type)))
      ; if needed, read another token and iterate
      (when again?
        (let-values (((value-i type-i) (lex-shell in enabled-parsers)))
          (set! value value-i)
          (set! type type-i))))
    ; shell form is complete, return it
    (if reverse? (reverse! ret) ret)))

; Read shell forms from textual input port 'in' until a token } or ] or )
; matching the specified begin-type token is found.
; Automatically change parser when directive #!... is found.
;
; Return a list containing 'shell-list followed by such forms.
; Raise syntax-violation if mismatched end token is found, as for example ')' instead of '}'
;
(define (parse-shell-list begin-type in already-parsed-reverse enabled-parsers)
  (let* ((first-token (case begin-type
           ((backquote dollar+lparen)
             (unless (null? already-parsed-reverse)
               (syntax-violation 'parse-shell
                 "unimplemented backquote in the middle of non-shell commands, it currently can only be inside shell commands:"
                 (reverse! (cons begin-type already-parsed-reverse)) begin-type))
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
             (syntax-violation
               'parse-shell
               (string-append "unexpected token " (paren-type->string type)
                  ", expecting " (paren-type->string end-type))
               type)))))
    (while again?
      (let-values (((value type) (lex-shell in enabled-parsers)))
        ; (format #t "parse-shell-list ret=~s value=~s type=~s~%" (reverse ret) value type)
        (case type
          ((eof)
            (syntax-violation 'parse-shell-list "unexpected end-of-file after"
              (if reverse? (reverse! ret) ret)))
          ((parser)
            ; switch to other parser until the end of current list
            (let ((other-parse-list (parser-parse-list value)))
              (set! ret (other-parse-list begin-type in ret enabled-parsers)))
            (set! reverse? #f)
            (set! again? #f))
          ((rparen rbrack rbrace)
            (check-list-end type)
            (set! again? #f))
          ((lbrace)
            ; parse nested shell list
            (let ((nested-list (parse-shell-list type in '() enabled-parsers)))
              (set! ret (cons nested-list ret))))
          (else
            (if (and (eq? 'backquote begin-type) (eq? 'backquote type))
              ; end of backquote reached
              (begin
                (check-list-end type)
                (set! again? #f))
              ; parse a single shell form and accumulate it into ret
              (let ((value (parse-shell-impl value type in enabled-parsers
                             (eq? 'backquote begin-type))))
                (set! ret (cons value ret))))))))
    (if reverse? (reverse! ret) ret)))

(define parser-shell
  (let ((ret (make-parser 'shell parse-shell parse-shell* parse-shell-list)))
    (lambda ()
      ret)))

) ; close library
