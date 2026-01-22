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


;; Wrapper around Chez Scheme (read-token), recognizes the following extensions:
;;   { as (values 'lbrace f ...)
;;   } as (values 'rbrace f ...)
;;   character literals #\x... representing valid UTF-8b codepoints
;;   string literals "..." also containing hexadecimal escape sequences \x...;
;;     representing valid UTF-8b codepoints
;;
;; Internally uses Chez Scheme (read-token) for simplicity, but could be reimplemented
;; in pure R6RS.
;;
;; returns two values:
;;   token value
;;   token type
(define (lex-token ctx flavor)
  (parsectx-skip-whitespace ctx 'also-skip-newlines) ; in case caller is not (lex-lisp)
  (case (parsectx-peek-char ctx)
    ((#\")
      (lex-string ctx flavor))
    ((#\#)
      (lex-sharp ctx flavor))
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
      (lex-number ctx flavor #f 10 '()))
    (else
      (let-values (((value type) (lex-token-chezscheme ctx)))
        (if (eq? 'atomic type)
          (case value
            ;; replace (values '{ 'atomic) with (values #f 'lbrace)
            ;; and replace (values '} 'atomic) with (values #f 'rbrace)
            ;; because we use them to switch to shell parser. For example,
            ;;    {ls -l > log.txt}
            ;; is equivalent to
            ;;    (#!shell ls -l > log.txt)
            ;;
            ;; also, replace (values '$ 'atomic) with (values 'quote 'shell-expr)
            ;; because we want to allow $(expr ...) also in Scheme syntax. For example,
            ;;    $(string-append "/home" "/user")
            ;; is equivalent to
            ;;    (shell-expr (string-append "/home" "/user"))
            ((\x7B;   ;  '{
                   )
                  (values #f 'lbrace))
            ((\x7D;   ;  '}
                   )
                  (values #f 'rbrace))
            (($)  (values 'shell-expr 'quote))
            (else (values value type)))
          (values value type))))))


;; minimal wrapper around Chez Scheme (read-token)
;;
;; returns two values:
;;   token value
;;   token type
(define (lex-token-chezscheme ctx)
  (let-values (((type value start end) (read-token (parsectx-in ctx))))
    ;; (debugf "lex-token-chezscheme type=~s value=~s start=~s end=~s" type value start end)
    (values value type)))


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
  (assert* 'lex-sharp (eqv? #\# (parsectx-read-char ctx)))
  (let ((ch (parsectx-read-char ctx)))
    (case ch
      ((#\@)
        (values #f 'fasl))
      ((#\\)
        (lex-character ctx flavor))
      ((#\&)
        (values #f 'box))
      ((#\')
        (values 'syntax 'quote))
      ((#\`)
        (values 'quasisyntax 'quote))
      ((#\,)
        (lex-unsyntax ctx flavor))
      ((#\;)
        (values 'datum-comment 'quote))
      ((#\:)
        ;; handle #:pretty-name
        (lex-gensym-short ctx flavor))
      ((#\%)
        (lex-primitive ctx flavor 0))
      ((#\()                    #| ) |# ; help vscode
        (values #f 'vparen))
      ((#\[)
        (values #f 'record-brack))
      ((#\{)
        ;; handle #{pretty-name unique}
        (lex-gensym-full ctx flavor))
      ((#\|)
        (lex-skip-block-comment ctx flavor)
        (lex-token ctx flavor))
      ((#\B #\b)
        (lex-number ctx flavor #f 2 (list ch #\#)))
      ((#\D #\d)
        (lex-number ctx flavor #f 10 (list ch #\#)))
      ((#\E #\e)
        (lex-number ctx flavor 'exact #f (list ch #\#)))
      ((#\F #\f)
        (lex-boolean ctx flavor #f))
      ((#\I #\i)
        (lex-number ctx flavor 'inexact #f (list ch #\#)))
      ((#\O #\o)
        (lex-number ctx flavor #f 8 (list ch #\#)))
      ((#\T #\t)
        (lex-boolean ctx flavor #t))
      ((#\v)
        (lex-vector ctx flavor))
      ((#\X #\x)
        (lex-number ctx flavor #f 16 (list ch #\#)))
      (else
        (unless (char? ch)
          (syntax-errorf ctx (caller-for flavor) "unexpected end-of-file after #"))
        (unless (char<=? #\0 ch #\9)
          (syntax-errorf ctx (caller-for flavor) "invalid sharp-sign prefix #~a" ch))
        (lex-sharp-number ctx flavor ch)))))


;; read next char after #,
;;
;; returns two values:
;;   either (values 'unsyntax-splicing 'quote)
;;   or     (values 'unsyntax          'quote)
(define (lex-unsyntax ctx flavor)
  (cond
    ((eqv? #\@ (parsectx-peek-char ctx))
      (parsectx-read-char ctx)
      (values 'unsyntax-splicing 'quote))
    (else
      (values 'unsyntax 'quote))))


;; read an identifier after #:
;; and return two values:
;;   token value: a symbol generated by calling (gensym identifier)
;;   token type: 'atomic
(define (lex-gensym-short ctx flavor)
  (let ((name (lex-identifier-or-raise ctx flavor "invalid identifier ~s reading gensym syntax #:")))
    (values
      (gensym (symbol->string name))
      'atomic)))

;; read two identifiers and } after #{
;; and return two values:
;;   token value: a symbol generated by calling (gensym identifier1 identifier2)
;;   token type: 'atomic
(define (lex-gensym-full ctx flavor)
  (let* ((name   (lex-identifier-or-raise ctx flavor "invalid identifier ~s reading gensym syntax #{"))
         (value2 (lex-identifier-or-raise ctx flavor "invalid identifier ~s reading gensym syntax #{"))
         (unique (if (eq? value2 '\x7d;)
                   (string->symbol "")
                   (let ((value3 (lex-identifier-or-raise ctx flavor "invalid identifier ~s reading gensym syntax #{")))
                     (unless (eq? value3 '\x7d;)
                       (syntax-errorf ctx (caller-for flavor) "found ~s instead of } reading gensym syntax #{" value3))
                      value2))))
    (values
      (gensym (symbol->string name) (symbol->string unique))
      'atomic)))


;; read an identifier after #% or #N%
;; and return two values:
;;   token value: a list ($primitive opt-level identifier)
;;   token type: 'atomic
(define (lex-primitive ctx flavor opt-level)
  (let ((identifier (lex-identifier-or-raise ctx flavor "invalid identifier ~s reading $primitive syntax #%")))
    (values
      (list '$primitive opt-level identifier)
      'atomic)))
    

;; read an identifier and return it.
;; raise condition if next token is not an identifier
(define (lex-identifier-or-raise ctx flavor error-fmt)
  (let-values (((type value) (lex-token-chezscheme ctx)))
    (unless (and (eq? 'atomic type) (symbol? value))
      (syntax-errorf ctx (caller-for flavor) error-fmt value))
    value))
  

;; read a lisp block comment starting with #| and ending with |#
;; note: block comments can be nested!
;;
;; return unspecified value
(define (lex-skip-block-comment ctx flavor)
  (let ((ch (parsectx-read-char ctx)))
    (unless (char? ch)
      (raise-eof-in-block-comment ctx flavor))
    (case ch
      ((#\|)
        (let ((ch2 (parsectx-read-char ctx)))
          (unless (char? ch)
            (raise-eof-in-block-comment ctx flavor))
          (unless (eqv? #\# ch)
            (lex-skip-block-comment ctx flavor)))) ; iterate, unless we found |#
      ((#\#)
        (let ((ch2 (parsectx-read-char ctx)))
          (unless (char? ch)
            (raise-eof-in-block-comment ctx flavor))
          (when (eqv? #\| ch)
            (lex-skip-block-comment ctx flavor)) ; recurse, read a nested block comment
          (lex-skip-block-comment ctx flavor)))  ; iterate
      (else
        (lex-skip-block-comment ctx flavor)))))  ; iterate


(define (raise-eof-in-block-comment ctx flavor)
  (syntax-errorf ctx (caller-for flavor) "unexpected end-of-file reading block comment"))


;; read one of: #f #t #true #false (case-insensitive)
;;
;; return two values:
;;   token value: either #t or #f
;;   token type:  'atomic
(define (lex-boolean ctx flavor value)
  (let ((ch (parsectx-peek-char ctx)))
    (if (separator? ch)
      (values value 'atomic) ; also allow end-of-file after boolean
      (lex-boolean-longform ctx flavor value))))


;; read one of: #true #false (case-insensitive)
;;
;; return two values:
;;   token value: either #t or #f
;;   token type:  'atomic
(define (lex-boolean-longform ctx flavor value)
  (let %loop ((i 1)
              (n  (if value 4 5))
              (lo (if value "true" "false"))
              (up (if value "TRUE" "FALSE")))
    (let* ((ch (parsectx-peek-char ctx))
           (sep? (separator? ch)))
      (unless (eq? sep? (fx=? i n))
        (raise-invalid-boolean ctx flavor ch))
      (if sep?
        (values value 'atomic)
        (begin
          (unless (or (eqv? ch (string-ref lo i))
                      (eqv? ch (string-ref up i)))
            (raise-invalid-boolean ctx flavor ch))
          (parsectx-read-char ctx)
          (%loop (fx1+ i) n lo up))))))
  

(define (raise-invalid-boolean ctx flavor ch)
  (syntax-errorf ctx (caller-for flavor) "invalid delimiter ~a for boolean"))


;; read a number in specified radix (which defaults to 10),
;; then optionally convert it to exact or inexact
;;
;; returns two values:
;;   token value: the number
;;   token type: 'atomic
(trace-define (lex-number ctx flavor exact-opt radix prefix)
  (let %loop ((prefix prefix) (buf (charspan)) (exact-opt exact-opt) (radix radix))
    (let ((ch (parsectx-peek-char ctx)))
      (cond
        ((separator? ch)
          (when (charspan-empty? buf)
            (raise-invalid-number ctx flavor prefix buf ch))
          (let ((num (string->number (charspan->string*! buf) (or radix 10))))
            (values
              (case exact-opt ((exact)   (exact num))
                              ((inexact) (inexact num))
                              (else      num))
              'atomic)))

        ((eqv? #\# ch)
          (when (null? prefix)
            ;; number starts with a digit, cannot have options
            (raise-invalid-number ctx flavor prefix buf ch))
          (parsectx-read-char ctx)
          (let* ((prefix (cons ch prefix))
                 (ch     (parsectx-read-char ctx)))
            (cond
              ((separator? ch)
                (raise-invalid-number ctx flavor prefix buf ch))

              ((or (eqv? #\E ch) (eqv? #\I ch) (eqv? #\e ch) (eqv? #\i ch))
                (when exact-opt ;; multiple #i and/or #e
                  (raise-invalid-number ctx flavor prefix buf ch))
                (let ((prefix (cons ch prefix))
                      (exact-opt (if (or (eqv? #\E ch) (eqv? #\e ch))
                                   'exact
                                   'inexact)))
                  (%loop prefix buf exact-opt radix)))

              ((char<=? #\0 ch #\9)
                ;; #NNNr is a Chez Scheme extension to specify radix
                (when (or radix (not (eq? 'scheme flavor)))
                  (raise-invalid-number ctx flavor prefix buf ch))
                (let* ((radix (lex-unsigned-decimal ctx flavor (fx- (char->integer ch) 48)))
                       (prefix (cons radix prefix))
                       (ch (parsectx-read-char ctx)))
                  (unless (and (fx<=? 2 radix 36)
                               (or (eqv? #\R ch) (eqv? #\r ch)))
                    (raise-invalid-number ctx flavor prefix buf ch))
                  (%loop (cons ch prefix) buf exact-opt radix)))

              (else
                (raise-invalid-number ctx flavor prefix buf ch)))))

        ((or (char<=? #\0 ch #\9) (char<=? #\A ch #\Z) (char<=? #\a ch #\z)
             (eqv? #\. ch) (eqv? #\+ ch) (eqv? #\- ch))
          (charspan-insert-right! buf ch)
          (%loop prefix buf exact-opt radix))

        (else
          (raise-invalid-number ctx flavor prefix buf ch))))))


;; parse zero or more decimal digits and return the corresponding decimal number
(define (lex-unsigned-decimal ctx flavor value)
  (let ((ch (parsectx-peek-char ctx)))
    (cond
      ((and (char? ch) (char<=? #\0 ch #\9))
        (parsectx-read-char ctx)
        (lex-unsigned-decimal ctx flavor (+ (* value 10) (fx- (char->integer ch) 48))))
      (else
        value))))


(define (raise-invalid-number ctx flavor prefix buf ch)
  (charspan-insert-right! buf ch)
  (for-list ((obj prefix))
    (cond
      ((char? obj)   (charspan-insert-left! buf obj))
      ((number? obj) (charspan-insert-left/string! buf (number->string obj)))))
  (syntax-errorf ctx (caller-for flavor) "invalid number syntax ~a" (charspan->string*! buf)))


;; parse #vu8( #vfl( #vfx( #vs(
;;
;; note: #v is already parsed
(define (lex-vector ctx flavor)
  ;; TODO: implement
  (values #f 'vu8paren))


;; parse #N= #N# #Nr #nR #N( #Nvfl( #Nvfx( #Nvu8(
;;
;; note: # and ch are already parsed
(define (lex-sharp-number ctx flavor ch)
  ;; TODO: implement
  (values #f 'vu8paren))


;; read a lisp character literal starting with #\
;; also supports character literals #\x... representing valid UTF-8b codepoints
;;
;; returns two values:
;;   token value: the character value
;;   token type: 'atomic
(define (lex-character ctx flavor)
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


;; if flavor is 'scheme and name contains exactly three octal digits in the range 000 ... 377,
;; the convert them to a number and return (integer->char) of such number.
(define (%chezscheme-octal-char flavor name)
  (and (eq? flavor 'scheme)
       (fx=? 3 (string-length name))
       (char<=? #\0 (string-ref name 0) #\3)
       (char<=? #\0 (string-ref name 1) #\7)
       (char<=? #\0 (string-ref name 2) #\7)
       (integer->char (string->number name 8))))


;; if flavor is 'scheme then search and return
;; non-standard character names allowed by Chez Scheme (char-name)
(define (%chezscheme-char-name flavor name)
  (and (eq? flavor 'scheme)
       (char-name (string->symbol name))))


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
        (or (hashtable-ref %char-names name #f)
            (%chezscheme-octal-char flavor name)
            (%chezscheme-char-name flavor name)
            (syntax-errorf ctx (caller-for flavor) "invalid character ~s" name))))))


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
