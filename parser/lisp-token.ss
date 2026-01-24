;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file parser/lisp.ss


(define (caller-for flavor)
  (if (eq? flavor 'r6rs)
    'parse-r6rs-forms
    'parse-scheme-forms))


;; Read a single r6rs or Chez Scheme token from parsectx
;;
;; Return two values: token value and its type.
;;
;; Compared to Chez Scheme (read-token), recognizes the following extensions:
;;   #!parser_name  as (values parser_object 'parser)
;;   #!...          treated as line comment
;;   $              as (values 'shell-expr 'quote)
;;   {              as (values #f 'lbrace)
;;   }              as (values #f 'rbrace)
;;   character literals #\x... representing valid UTF-8b codepoints
;;   string literals "..." also containing hexadecimal escape sequences \x...;
;;     representing valid UTF-8b codepoints
;;
;; For simplicity, internally calls Chez Scheme (read-token) in several cases,
;; but could be reimplemented in pure R6RS.
;;
;; returns two values:
;;   token value
;;   token type
(define (lex-lisp ctx flavor)
  (parsectx-skip-whitespace ctx 'also-skip-newlines)
  (let ((ch (parsectx-peek-char ctx)))
    (case ch
      ((#\")
        (lex-string ctx flavor))
      ((#\#)
        (lex-sharp ctx flavor))
      ((#\$)
        (parsectx-read-char ctx)
        (values 'shell-expr 'quote))
      ((#\')
        (parsectx-read-char ctx)
        (values 'quote 'quote))
      ((#\()               #| ) |#  ; help vscode
        (parsectx-read-char ctx)
        (values #f 'lparen))
      ((#\))               #| ( |#  ; help vscode
        (parsectx-read-char ctx)
        (values #f 'rparen))
      ((#\,)
        (parsectx-read-char ctx)
        (if (eqv? #\@ (parsectx-peek-char ctx))
          (begin
            (parsectx-read-char ctx)
            (values 'unquote-splicing 'quote))
          (values 'unquote 'quote)))
      ((#\;)
        ;; handle line comments ourselves, because they may be followed
        ;; by a token not supported by (lex-lisp-chezscheme)
        (parsectx-skip-line ctx)
        (lex-lisp ctx flavor))
      ((#\[)
        (parsectx-read-char ctx)
        (values #f 'lbrack))
      ((#\])
        (parsectx-read-char ctx)
        (values #f 'rbrack))
      ((#\`)
        (parsectx-read-char ctx)
        (values 'quasiquote 'quote))
      ((#\{)
        (parsectx-read-char ctx)
        (values #f 'lbrace))
      ((#\})
        (parsectx-read-char ctx)
        (values #f 'rbrace))
      (else
        (if (eof-object? ch)
          (values ch 'eof)
          (lex-lisp-chezscheme ctx flavor))))))


;; minimal wrapper around Chez Scheme (read-token)
;;
;; returns two values:
;;   token value
;;   token type
(define (lex-lisp-chezscheme ctx flavor)
  (try
    (let* ((in   (parsectx-in ctx))
           (pos0 (and (port-has-port-position? in) (port-position in))))
      (let-values (((type value start end) (read-token in)))
        ;; start, end are usually #f
        (let ((pos1 (and (fixnum? pos0) (port-position in))))
          (when (and (fixnum? pos1) (fx>? pos1 pos0))
            ;; (debugf "lex-lisp-chezscheme type=~s value=~s pos0=~s pos1=~s" type value pos0 pos1)
            (parsectx-increment-pos/n ctx (fx- pos1 pos0))))
        (values value type)))

    (catch (ex)
      (let ((l (condition-irritants ex)))
        (cond
          ((and (string? (car l)) (pair? (cdr l)) (pair? (cadr l))
                (string-index (car l) #\~))
            (syntax-errorf ctx (caller-for flavor) (car l) (caadr l)))
          ((format-condition? ex)
            (apply syntax-errorf ctx (caller-for flavor) (condition-message ex) l))
          (else
            (syntax-errorf ctx (caller-for flavor) "~a ~s" (condition-message ex) l)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; read a lisp string literal starting with double quotes.
;; also supports string literals "..." containing hexadecimal escape sequences \x...;
;;   representing valid UTF-8b codepoints
;;
;; returns two values:
;;   token value: the unescaped string
;;   token type: 'atomic
(define (lex-string ctx flavor)
  (assert* 'lex-string (eqv? #\" (parsectx-read-char ctx)))
  (let ((csp (charspan)))
    (let %again ()
      (let ((ch (lex-string-chars ctx flavor)))
        (when (char? ch)
          (charspan-insert-right! csp ch))
        (when ch
          (%again))))
    (%assert-next-char-is-separator ctx flavor "string")
    (values (charspan->string*! csp) 'atomic)))


;; read some characters inside a lisp string literal,
;; interpreting escape sequences, and returning the converted character.
;;
;; if it should be called again, returns #t or the converted character.
;; otherwise returns #f.
(define (lex-string-chars ctx flavor)
  (let ((ch (parsectx-read-char ctx)))
    (case ch
      ((#\\)
        (lex-string-chars-after-backslash ctx flavor))
      ((#\")
        #f)
      (else
        (when (eof-object? ch)
          (syntax-errorf ctx (caller-for flavor) "unexpected end-of-file reading string"))
        ch))))


;; read some characters after #\\ inside a lisp string literal,
;; interpreting escape sequences, and return the corresponding character.
;;
;; returns #t or the converted character.
(define (lex-string-chars-after-backslash ctx flavor)
  (let ((ch (parsectx-read-char ctx)))
    (case ch
      ((#\" #\\) ch)
      ((#\a) #\alarm)
      ((#\b) #\backspace)
      ((#\f) #\page)
      ((#\n) #\newline)
      ((#\r) #\return)
      ((#\t) #\tab)
      ((#\v) #\vtab)
      ((#\x)
        (lex-string-hex-sequence ctx flavor))
      ((#\newline #\linefeed #\page #\return)
        (skip-intraline-whitespace ctx))
      ((#\tab #\vtab #\space)
        (skip-intraline-whitespace-newline-intraline-whitespace ctx flavor))
      (else
        (cond
          ((not (char? ch))
            (syntax-errorf ctx (caller-for flavor) "unexpected end-of-file reading string"))
          ((and (eq? flavor 'scheme) (char=? ch #\'))
            ch)
          ((and (eq? flavor 'scheme) (char<=? #\0 ch #\7))
            (read-lisp-string-octal-sequence ch ctx flavor))
          (else
            (syntax-errorf ctx (caller-for flavor) "invalid string character ~s" ch)))))))


;; read an octal escape sequence after "\" inside a string literal,
;; and return the corresponding character.
;;
;; either returns a character or raises an exception.
(define (read-lisp-string-octal-sequence ch ctx flavor)
  (let %next ((val (%octal-digit->fixnum ch ctx flavor))
              (parsed 1))
    (if (fx=? parsed 3)
      (integer->char val)
      (let ((n (%octal-digit->fixnum (parsectx-read-char ctx) ctx flavor)))
        (%next (fxior n (fxarithmetic-shift-left val 3)) (fx1+ parsed))))))


(define (%octal-digit->fixnum ch ctx flavor)
  (unless (char? ch)
    (syntax-errorf ctx (caller-for flavor) "unexpected end-of-file reading string hex escape"))
  (if (char<=? #\0 ch #\7)
    (fx- (char->integer ch) 48)
    (syntax-errorf ctx (caller-for flavor) "invalid character ~s in string octal escape" ch)))


;; read an hexadecimal escape sequence after "\x" inside a string literal,
;; and return the corresponding character.
;;
;; either returns a character or raises an exception.
(define (lex-string-hex-sequence ctx flavor)
  (let %next ((ret 0))
    (let* ((ch (parsectx-peek-char ctx))
           (n  (%hex-digit->fixnum ch)))
      (cond
        (n
          (parsectx-read-char ctx)
          (%next (fxior n (fxarithmetic-shift-left ret 4))))
        ((eqv? ch #\;)
          (parsectx-read-char ctx)
          (integer->char* ret))
        ((eof-object? ch)
          (syntax-errorf ctx (caller-for flavor) "unexpected end-of-file reading string hex escape"))
        (else
          (syntax-errorf ctx (caller-for flavor) "invalid character ~s in string hex escape" ch))))))


;; read intraline whitespace after a backslash, followed by a newline and possibly more intraline whitespace.
;;
;; either returns #t or raises an exception.
(define (skip-intraline-whitespace-newline-intraline-whitespace ctx flavor)
  (skip-intraline-whitespace ctx)
  (let ((ch (parsectx-peek-char ctx)))
    ;; https://www.scheme.com/tspl4/grammar.html#grammar:strings
    ;; A line ending is one of:
    ;; 1. a newline character
    ;; 2. a next-line character
    ;; 3. a line-separator character
    ;; 4. a carriage-return character followed by a newline character - NOT IMPLEMENTED
    ;; 5. a carriage return followed by a next-line character  - NOT IMPLEMENTED
    ;; 6. a carriage return not followed by a newline or next-line character
    (unless (memq ch '(#\newline #\linefeed #\page #\return))
      (syntax-errorf ctx (caller-for flavor) "unexpected character ~s after \\<intraline whitespace>" ch)))
  (parsectx-read-char ctx) ; consume newline and similar
  (skip-intraline-whitespace ctx))


;; always returns #t
(define (skip-intraline-whitespace ctx)
  (while (memq (parsectx-peek-char ctx) '(#\tab #\vtab #\space))
    (parsectx-read-char ctx))
  #t)


;; return #t if ch is a scheme separator i.e. ends a token
(define (separator? ch)
  (or (not (char? ch))
      (case ch
        ((#\( #\) #\[ #\] #\{ #\} #\# #\' #\" #\` #\, #\;)
          #t)
        (else
          (char<=? ch #\space)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; read a lisp token starting with "#"
;;
;; returns two values:
;;   token value
;;   token type
(define (lex-sharp ctx flavor)
  (assert* 'lex-sharp (eqv? #\# (parsectx-peek-char ctx)))
  (let ((ch (parsectx-peek-char2 ctx)))
    (unless (char? ch)
      (parsectx-read-char ctx)
      (syntax-errorf ctx (caller-for flavor) "unexpected end-of-file after #"))
    (case ch
      ((#\!)
        (parsectx-read-char ctx) ; skip #\#
        (parsectx-read-char ctx) ; skip #\!
        (let ((value (parsectx-read-directive ctx)))
          (if (symbol? value)
            (case value
              ((bwp)
                ;; #!bwp is an allowed directive only in #!scheme syntax:
                ;; it injects (bwp-object) i.e. broken weak pair in token stream, with type 'atomic
                (unless (eq? 'scheme flavor)
                  (syntax-errorf ctx (caller-for flavor)
                    "directive #!~a is not allowed in #!r6rs syntax, requires #!scheme syntax" value))
                (values (bwp-object) 'atomic))
              ((eof)
                ;; #!eof is an allowed directive:
                ;; it injects (eof-object) in token stream, with type 'eof
                ;; simulating an actual end-of-file in input port.
                ;; Reason: traditionally used to disable the rest of a file, to help debugging
                (values (eof-object) 'eof))
              (else
                ;; cannot switch to other parser here: just return it and let caller switch
                (values (get-parser ctx value (caller-for flavor)) 'parser)))

            ;; (parsectx-read-directive) skipped a whole line.
            ;; read again by calling (lex-lisp)
            (lex-lisp ctx flavor))))
      ((#\&)
        (parsectx-read-char ctx)
        (parsectx-read-char ctx)
        (values #f 'box))
      ((#\')
        (parsectx-read-char ctx)
        (parsectx-read-char ctx)
        (values 'syntax 'quote))
      ((#\()                   #|  )  |# ; help vscode
        (parsectx-read-char ctx)
        (parsectx-read-char ctx)
        (values #f 'vparen))
      ((#\,)
        (parsectx-read-char ctx)
        (parsectx-read-char ctx)
        (if (eqv? #\@ (parsectx-peek-char ctx))
          (begin
            (parsectx-read-char ctx)
            (values 'unsyntax-splicing 'quote))
          (values 'unsyntax 'quote)))
      ((#\;)
        (parsectx-read-char ctx)
        (parsectx-read-char ctx)
        (values 'datum-comment 'quote))
      ((#\@)
        (parsectx-read-char ctx)
        (parsectx-read-char ctx)
        (values #f 'fasl))
      ((#\[)
        (parsectx-read-char ctx)
        (parsectx-read-char ctx)
        (values #f 'record-brack))
      ((#\\)
        (lex-character ctx flavor))
      ((#\`)
        (parsectx-read-char ctx)
        (parsectx-read-char ctx)
        (values 'quasisyntax 'quote))
      ((#\|)
        ;; handle block comments #| ... |# ourselves, because they may be followed
        ;; by a token not supported by (lex-lisp-chezscheme)
        (parsectx-read-char ctx) ; skip #\#
        (parsectx-read-char ctx) ; skip #\|
        (lex-skip-block-comment ctx flavor)
        (lex-lisp ctx flavor))
      ((#\% #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\F #\T #\d #\e #\f #\i #\o #\t #\v #\x #\{)
        ;; #%foo      expands to value '($primitive foo)
        ;; #0... to #9... expand to one of:
        ;;              a sized vector type:
        ;;                 'vnparen       if parsing #NNN(
        ;;                 'vfxnparen     if parsing #NNNvfx(
        ;;                 'vflnparen     if parsing #NNNvfl(
        ;;                 'vsnparen      if parsing #NNNvs(
        ;;              a number value in user-specified radix,        if parsing #NNNrMMM
        ;;              type 'mark i.e. a graph mark,                  if parsing #NNN=
        ;;              type 'insert i.e. a graph reference,           if parsing #NNN#
        ;;              a value '($primitive optimize-level some-name) if parsing #2%... or #3%...
        ;; #:foo      expands to a gensym value
        ;; #F...      expands to the #f value
        ;; #T...      expands to the #t value
        ;; #d...      expands to a decimal number value
        ;; #e...      expands to an exact number value
        ;; #f...      expands to the #f value
        ;; #i...      expands to an inexact number value
        ;; #o...      expands to an octal number value
        ;; #t...      expands to the #t value
        ;; #vfl(      expands to type 'vflparen
        ;; #vfx(      expands to type 'vfxparen
        ;; #vs(       expands to type 'vsparen
        ;; #vu8(      expands to type 'vu8paren
        ;; #x...      expands to an hexadecimal number value
        ;; #{foo bar} expands to a gensym value
        (lex-lisp-chezscheme ctx flavor))
      (else
        (syntax-errorf ctx (caller-for flavor) "invalid sharp-sign prefix #~a" ch)))))


;; read a lisp block comment starting with #| and ending with |#
;; note: block comments can be nested!
;;
;; return unspecified value
(define (lex-skip-block-comment ctx flavor)
  (let ((ch (parsectx-read-char ctx)))
    (unless (char? ch)
      (raise-eof-in-block-comment ctx flavor))
    (case ch
      ((#\#)
        (let ((ch2 (parsectx-read-char ctx)))
          (unless (char? ch)
            (raise-eof-in-block-comment ctx flavor))
          (when (eqv? #\| ch2)
            (lex-skip-block-comment ctx flavor)) ; recurse, read a nested block comment
          (lex-skip-block-comment ctx flavor)))  ; iterate
      ((#\|)
        (let ((ch2 (parsectx-read-char ctx)))
          (unless (char? ch2)
            (raise-eof-in-block-comment ctx flavor))
          (unless (eqv? #\# ch2)
            (lex-skip-block-comment ctx flavor)))) ; iterate, unless we found |#
      (else
        (lex-skip-block-comment ctx flavor)))))  ; iterate


(define (raise-eof-in-block-comment ctx flavor)
  (syntax-errorf ctx (caller-for flavor) "unexpected end-of-file reading block comment"))


;; read a lisp character literal starting with #\
;; also supports character literals #\x... representing valid UTF-8b codepoints
;;
;; returns two values:
;;   token value: the character value
;;   token type: 'atomic
(define (lex-character ctx flavor)
  (assert* 'lex-character (eqv? #\# (parsectx-read-char ctx)))
  (assert* 'lex-character (eqv? #\\ (parsectx-read-char ctx)))
  (let* ((ch (parsectx-peek-char ctx))
         (ret (if (parsectx-is-simple-identifier-char? ch)
                (let ((name (parsectx-read-simple-identifier ctx)))
                  (%char-name->char ctx flavor name))
                (parsectx-read-char ctx)))) ; consume and return peeked char
    (%assert-next-char-is-separator ctx flavor "character")
    (values ret 'atomic)))


;; standard character names
(define %char-names (hashtable string-hash string=?
   "alarm" #\alarm "backspace" #\backspace "delete" #\delete "esc" #\esc
   "linefeed" #\linefeed "newline" #\newline "page" #\page "return" #\return
   "space" #\space "tab" #\tab "vtab" #\vtab))


;; if name contains exactly three octal digits in the range 000 ... 377,
;; the convert them to a number and return (integer->char) of such number.
;;
;; This is a Chez Scheme extension and should be enabled only in #!scheme syntax.
(define (%chezscheme-octal-char name)
  (and (fx=? 3 (string-length name))
       (char<=? #\0 (string-ref name 0) #\3)
       (char<=? #\0 (string-ref name 1) #\7)
       (char<=? #\0 (string-ref name 2) #\7)
       (integer->char (string->number name 8))))


;; convert character name stored in string
;; to non-standard character allowed by Chez Scheme (char-name)
(define (%chezscheme-char-name name)
  (char-name (string->symbol name)))


;; convert a character name into a character
(define (%char-name->char ctx flavor name)
  (let ((ch0 (string-ref name 0)))
    (cond
      ((fx=? 1 (string-length name))
        ch0)
      ((char=? #\x ch0)
        ;; (integer->char*) allows all valid UTF-8b codepoints,
        ;; which are a superset of valid UTF-8 codepoints.
        (integer->char* (%hex-digits->fixnum name 1 (string-length name))))
      (else
        (let* ((ret (hashtable-ref %char-names name #f))
               (ext (or ret
                        (%chezscheme-octal-char name)
                        (%chezscheme-char-name name))))
          (cond
            (ret ret)
            (ext
              (unless (eq? 'scheme flavor)
                (syntax-errorf ctx (caller-for flavor)
                  "character name #\\~a is not allowed in #!r6rs syntax, requires #!scheme syntax" name))
              ext)
            (else
              (syntax-errorf ctx (caller-for flavor) "invalid character name #\\~a" name))))))))


;; parse characters in range [start, end) of string str
;; as a hexadecimal number, and return such number.
;;
;; returns #f if range is empty or some character in specified range
;; is not a hexadecimal digit [0-9A-Fa-f]
(define (%hex-digits->fixnum str start end)
  (and (fx<? start end)
    (let ((ret 0))
      (do ((i start (fx1+ i)))
          ((or (fx>=? i end) (not ret))
            ret)
        (let ((n (%hex-digit->fixnum (string-ref str i))))
          (set! ret (and n (fxior n (fxarithmetic-shift-left ret 4)))))))))


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

(define (%assert-next-char-is-separator ctx flavor label)
  (let ((ch (parsectx-peek-char ctx)))
    (case ch
      ((#\tab #\newline #\vtab #\page #\return #\space
        #\" #\# #\' #\( #\) #\, #\; #\[ #\] #\` #\{ #\})
       (void))
      (else
       (when (and (char? ch) (char>=? ch #\space))
         (syntax-errorf ctx (caller-for flavor) (string-append "invalid delimiter ~s for " label) ch))))))
