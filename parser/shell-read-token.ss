;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file parser/lisp.ss


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
      (else ch))))


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
          (else
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
            ;; read next char, suppressing any special meaning it may have
            (let ((ch-i (read-char-after-backslash ctx csp)))
              (when ch-i (charspan-insert-back! csp ch-i))))
          (else
            ;; single quote, newline, semicolon, operators and parentheses
            ;; have no special meaning inside dquotes
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
          (else
            (set! again? #f)))))
    ; (debugf "<   read-subwords-noquote ret=~s splice?=~s" (reverse ret) splice?)
    (cond
      (splice?            (values ret 'rsplice))
      ((null? ret)        (values "" 'atom))
      ((%is-literal? ret) (values (car ret) 'atom))
      (else               (values (cons 'shell-wildcard (reverse! ret)) 'atom)))))


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
                (else
                  (set! word '%)))
              ; return word before [
              (parsectx-unread-char ctx ch))
            (set! again? #f))
          (else
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
      (else
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
          (else
            (charspan-insert-back! word ch)))))
    (charspan->string word)))
