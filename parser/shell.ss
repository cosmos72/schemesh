;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh parser shell (0 8 3))
  (export
    read-shell-char lex-shell parse-shell-word parse-shell-form1
    parse-shell-forms parser-shell)
  (import
    (rnrs)
    (only (chezscheme) append! fx1+ fx1- include inspect reverse! unread-char void)
    (only (schemesh bootstrap) assert* debugf until warnf while)
    (only (schemesh containers string) string-is-unsigned-base10-integer?)
    (schemesh containers charspan)
    (schemesh lineedit paren)
    (schemesh lineedit parser))


(include "parser/shell-read-token.ss")


;; Read a word, possibly containing single or double quotes, assignment operator =
;; wildcard operators ? * [...] and shell variable assignments, as for example:
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
    ; (debugf "-> parse-shell-word equal-is-operator?=~s, lbracket-is-subshell?=~s, inside-backquote?=~s" equal-is-operator? lbracket-is-subshell? inside-backquote?)
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
              ((and (eq? 'squote type) (not dquote?))
                (%append (read-subword-single-quoted ctx))
                (set! lbracket-is-subshell? #f))
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
                    (else
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
              (else
                (set! again? #f)))))))

    ; (debugf "<- parse-shell-word ret=~s" (reverse ret))
    (cond
      (splice?            (values (reverse! ret) 'splice))
      ((null? ret)        (values ""             'atom))
      ((%is-literal? ret) (values (car ret)      'atom))
      (else               (values (cons 'shell-wildcard (reverse! ret)) 'atom)))))


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
                         (else           (set! type 'separator))))
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
          (else ;; convert operator character to symbol
            (values (op->symbol ctx ch) type))))
      ((dollar lbrack)
        (cond
          ((and (eq? type 'dollar) (eqv? #\( (parsectx-peek-char ctx))) #| #\) |# ; help vscode
            (values (parsectx-read-char ctx) 'dollar+lparen))
          ((and (eq? type 'dollar) (eqv? #\[ (parsectx-peek-char ctx))) #| #\] |# ; help vscode
            (values (parsectx-read-char ctx) 'dollar+lbrack))
          ((and (eq? type 'lbrack) lbracket-is-subshell?)
            (values ch type))
          (else
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
                 (string-is-unsigned-base10-integer? value)
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
      (else
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
                     ((lbrack dollar+lbrack) 'rbrack)
                     ((lbrace dollar+lbrace) 'rbrace)
                     ((backquote) 'backquote)
                     ((eof)    'eof)
                     (else     'rparen)))
         (prefix (case begin-type
                     ((backquote dollar+lbrack) 'shell-backquote)
                     ((lbrack)  'shell-subshell)
                     (else      'shell)))
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
    ; (debugf "->   parse-shell-forms end-type=~s prefix=~s" end-type prefix)
    (until done?
      ;; (debugf "... parse-shell-forms equal-is-operator?=~s lbracket-is-subshell?=~s ret=~s" equal-is-operator? lbracket-is-subshell? (if prefix (reverse ret) ret))
      (let-values (((value type) (lex-shell ctx equal-is-operator? lbracket-is-subshell? #t inside-backquote?)))
        ; (debugf "... parse-shell-forms end-type=~s prefix=~s ret=~s value=~s type=~s" end-type prefix (if prefix (reverse ret) ret) value type)
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
            ; value can be '& #\; or #\newline
            (unless (and (eqv? #\newline value) (should-ignore-newlines? ret))
              (set! ret (cons (if (eq? value '&) '& '\x3B;) ret))
              (set! can-change-parser?    #t)
              (set! equal-is-operator?    #t)
              (set! lbracket-is-subshell? #t)))
          ((op string integer atom)
            (set! ret (cons value ret))
            (let ((flag (and (eq? type 'op)
                             (memq value '(&& \x7C;\x7C; \x7C; \x7C;&
                                          ))
                             #t)))
              (set! can-change-parser?    flag)
              (set! equal-is-operator?    flag)
              (set! lbracket-is-subshell? flag)))
          ((splice)
            (set! can-change-parser?      #f)
            ;; preserve equal-is-operator?
            (set! lbracket-is-subshell?   #f)
            (set! ret (append! (reverse! value) ret)))
          ((backquote)
            (set! can-change-parser?    #f)
            (set! equal-is-operator?    #f)
            (set! lbracket-is-subshell? #f)
            (if (eq? type end-type)
              (set! done? #t)
              ; TODO: `...` may be followed by other words without a space
              (let-values (((form _) (parse-shell-forms ctx type)))
                (unless (null? form)
                  (set! ret (cons form ret))))))
          ((lparen dollar+lparen)
            (set! can-change-parser?    #f)
            (set! equal-is-operator?    #f)
            (set! lbracket-is-subshell? #f)
            ; switch to Scheme parser for a single form.
            (let-values (((lisp-forms _) (parse-scheme-forms ctx type)))
              (if (and (null? ret) (eq? 'lparen type) (eq? 'eof end-type))
                ; lparen is the first token at shell top level:
                ; allow entering Scheme form
                (let ((return-lisp-form? (%after-lisp-forms-at-top-level ctx)))
                  (if return-lisp-form?
                    (let-values (((next-forms updated-parser) (parse-shell-forms ctx begin-type)))
                      (when updated-parser
                        (set! parser updated-parser))
                      (set! ret (cons lisp-forms next-forms))
                      (set! done? #t)
                      (set! prefix #f))
                    (set! ret (cons lisp-forms ret))))
                ; either lparen was in the middle of shell syntax, or dollar+lparen found:
                ; just insert parsed Scheme form into current shell command
                (set! ret (cons (if (eq? 'dollar+lparen type)
                                  (list 'shell-expr lisp-forms)
                                  lisp-forms)
                                ret)))))
          ((lbrace lbrack dollar+lbrack)
            (set! can-change-parser?    #f)
            (set! equal-is-operator?    #f)
            (set! lbracket-is-subshell? #f)
            ; TODO: $[...] may be followed by other words without a space
            (let-values (((form _) (parse-shell-forms ctx type)))
              ; (debugf "... parse-shell-forms nested_form=~s ret=~s" form ret)
              (unless (null? form)
                (set! ret (cons form ret)))))
          ((rbrace rbrack rparen)
            (set! can-change-parser?    #f)
            (set! equal-is-operator?    #f)
            (set! lbracket-is-subshell? #f)
            (unless (eq? type end-type)
              (syntax-errorf ctx 'parse-shell-forms "unexpected token ~a, expecting ~a"
                (paren-type->string type) (paren-type->string end-type)))
            (set! done? #t))
          (else
            (syntax-errorf ctx 'parse-shell-forms "unexpected ~a ~s after ~a" type value (reverse! ret))))))


    ; (debugf "... parse-shell-forms end-type=~s prefix=~s ret=~s" end-type prefix (if prefix (reverse ret) ret))
    (let ((simplified (%simplify-parse-shell-forms end-type prefix ret)))
      ; (debugf "<-  parse-shell-forms ret=~s" simplified)
      (values simplified parser))))


;; helper function to invoke scheme parser
(define (parse-scheme-forms ctx begin-type)
  (let ((lisp-parser (parser-parse-forms (get-parser ctx 'scheme 'parse-shell-forms))))
    (lisp-parser ctx begin-type)))


;; return #t in order to ignore newlines
;; after shell operators & ; ! && || | |& < <> > >> <& >& { [
(define (should-ignore-newlines? forms-revlist)
  (or (null? forms-revlist)
      (memq (car forms-revlist) '(! && \x3B; \x7C;\x7C; \x7C; \x7C;&
                                  < <> > >> <& >&))))


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
    (else
      (let ((form (cons prefix (reverse! ret))))
        (if (eq? 'eof end-type)
          (list form)
          form)))))


;; lparen was the first token, begin-type is eof,
;; and we read a whole Scheme form until the corresponding lparen.
;;
;; Convenience: if the first token after rparen is lparen, semicolon, newline or eof,
;; then instruct the caller to omit the initial (shell ...) and return the Scheme form as is.
;; This allows entering Scheme forms from shell syntax.
;;
;; Otherwise instruct the caller to wrap the Scheme form inside (shell ...)
;;
;; Return #t if Scheme form should be compiled and evaluated as is,
;; or #f is it should be inserted inside (shell ...)
(define (%after-lisp-forms-at-top-level ctx)
  (parsectx-skip-whitespace ctx #f) ; do not skip newlines
  (let-values (((ch type) (peek-shell-char ctx)))
    (case type
      ((eof lparen) #t)
      ((separator)  (read-shell-char ctx) #t) ; consume semicolon or newline
      (else         #f))))




(define dollar+lparen 1)
(define dollar+lbrack 2)
(define dollar+lbrace 3)

;; Read until one of ( ) { } ' " ` $( $[ is found, and return it.
;; ignore them if they are preceded by \
;; if $( or $[ is found, return $
;; Also recognize and return parser directives #!... and return them
(define (scan-shell-paren-or-directive ctx inside-dquote?)
  (let ((ret #f)
        (prev-char #f))
    (until ret
      (let ((ch (parsectx-read-char ctx)))
        (case ch
          ((#\()    #| ) |# ; help vscode
             (set! ret (if (eqv? prev-char #\$) dollar+lparen ch)))

          ((#\[)    #| ] |# ; help vscode
             (set! ret (if (eqv? prev-char #\$) dollar+lbrack ch)))

          ((#\{)
             (set! ret (if (eqv? prev-char #\$) dollar+lbrace ch)))

          #| ( |# ; help vscode
          ((#\) #\} #\[ #\] #\" #\' #\`) (set! ret ch))

          ((#\\)  (parsectx-read-char ctx)) ; consume one character after backslash

          ((#\#)
            (unless inside-dquote?
              (if (eqv? #\! (parsectx-read-char ctx))
                (set! ret (parsectx-read-directive ctx))
                ; #\# not followed by #\! is a comment line, skip it
                (parsectx-skip-line ctx))))

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

    (let-values (((x y) (parsectx-previous-pos ctx (if start-ch 1 0))))
      (paren-start-xy-set! paren x y))
    (until ret
      (let ((token (scan-shell-paren-or-directive ctx (eqv? start-ch #\"))))

        ; (debugf "parse-shell-paren start-ch=~a end-ch=~a token=~s" start-ch end-ch token)
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

          ((eqv? token #\()                  #| ) |# ; help vscode
             (unless (eqv? start-ch #\")
               ;; paren not inside double quotes -> switch to scheme parser
               ;; recursion: call scheme parser on nested list
               (let ((other-paren (parse-scheme-paren ctx token)))
                 (when other-paren
                   (paren-inner-append! paren other-paren)))))

          ((or (fixnum? token) (memv token '(#\{ #\[ #\" #\`)))
             ;; inside double quotes, $( $[ ${ are special but plain ( [ or { aren't.
             ;; " inside double quotes is handled above by (eqv? token end-ch)
             (unless (and (eqv? start-ch #\") (memv token '(#\[ #\{)))
               ;; recursion: call shell or lisp parser on nested list
               (let ((start-inner (cond ((eqv? token dollar+lparen) #\() #|)|#
                                        ((eqv? token dollar+lbrack) #\[) #|]|#
                                        ((eqv? token dollar+lbrace) #\{)
                                        (else                     token))))
                 (paren-inner-append! paren
                   (if (eqv? token dollar+lparen)
                     (parse-scheme-paren ctx start-inner)
                     (parse-shell-paren  ctx start-inner))))))

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


;; helper function to invoke scheme parser
(define (parse-scheme-paren ctx token)
  (let* ((other-parser      (get-parser-or-false ctx 'scheme))
         (other-parse-paren (and other-parser (parser-parse-paren other-parser))))
    (if other-parse-paren
      (other-parse-paren ctx token)
      (parse-shell-paren ctx token))))


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
