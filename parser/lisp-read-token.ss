;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


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
(define (read-lisp-token ctx flavor)
  (parsectx-skip-whitespace ctx 'also-skip-newlines) ; in case caller is not (lex-lisp)
  (case (parsectx-peek-char ctx)
    ((#\")
      (read-lisp-string ctx flavor))
    ((#\#)
      (read-lisp-sharp ctx flavor))
    (else
      (let-values (((value type) (%read-token ctx)))
        (if (eq? 'atomic type)
          (case value
            ;; replace (values '{ 'atomic) with (values #f 'lbrace)
            ;; and replace (values '} 'atomic) with (values #f 'rbrace)
            ;; because we use them to switch to shell parser. For example,
            ;;    {ls -l > log.txt}
            ;; is equivalent to
            ;;    (#!shell ls -l > log.txt)
            (({)  (values #f 'lbrace))
            ((})  (values #f 'rbrace))
            (else (values value type)))
          (values value type))))))


;; minimal wrapper around Chez Scheme (read-token)
;;
;; returns two values:
;;   token value
;;   token type
(define (%read-token ctx)
  (let-values (((type value start end) (read-token (parsectx-in ctx))))
    ;; (debugf "%read-token type=~s value=~s start=~s end=~s" type value start end)
    (values value type)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; read a lisp string literal starting with double quotes.
;; also supports string literals "..." containing hexadecimal escape sequences \x...;
;;   representing valid UTF-8b codepoints
;;
;; returns two values:
;;   token value: the unescaped string
;;   token type: 'atomic
(define (read-lisp-string ctx flavor)
  (assert* 'read-lisp-string (eqv? #\" (parsectx-read-char ctx)))
  (let ((csp (charspan)))
    (let %again ()
      (let ((ch (read-lisp-string-chars ctx flavor)))
        (when (char? ch)
          (charspan-insert-back! csp ch))
        (when ch
          (%again))))
    (%assert-next-char-is-separator ctx flavor "string")
    (values (charspan->string*! csp) 'atomic)))


;; read some characters inside a lisp string literal,
;; interpreting escape sequences, and returning the converted character.
;;
;; if it should be called again, returns #t or the converted character.
;; otherwise returns #f.
(define (read-lisp-string-chars ctx flavor)
  (let ((ch (parsectx-read-char ctx)))
    (case ch
      ((#\\)
        (read-lisp-string-chars-after-backslash ctx flavor))
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
(define (read-lisp-string-chars-after-backslash ctx flavor)
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
        (read-lisp-string-hex-sequence ctx flavor))
      ((#\tab #\vtab #\space)
        (skip-intraline-whitespace-newline-intraline-whitespace ctx flavor))
      (else
        (if (eof-object? ch)
          (syntax-errorf ctx (caller-for flavor) "unexpected end-of-file reading string")
          (syntax-errorf ctx (caller-for flavor) "invalid string character ~s" ch))))))


;; read an hexadecimal escape sequence after "\x" inside a string literal,
;; and return the corresponding character.
;;
;; either returns a character or raises an exception.
(define (read-lisp-string-hex-sequence ctx flavor)
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
        (#t
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
  (skip-intraline-whitespace ctx)
  #t)


(define (skip-intraline-whitespace ctx)
  (while (memq (parsectx-peek-char ctx) (#\tab #\vtab #\space))
    (parsectx-read-char ctx)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; read a lisp token starting with "#"
;;
;; returns two values:
;;   token value: the character value
;;   token type: 'atomic
(define (read-lisp-sharp ctx flavor)
  (assert* 'read-lisp-sharp (eqv? #\# (parsectx-read-char ctx)))
  (case (parsectx-peek-char ctx)
    ((#\\)
      (read-lisp-character ctx flavor))
    (else
      (parsectx-unread-char ctx #\#)
      (%read-token ctx))))


;; read a lisp character literal starting with "#\"
;; also supports character literals #\x... representing valid UTF-8b codepoints
;;
;; returns two values:
;;   token value: the character value
;;   token type: 'atomic
(define (read-lisp-character ctx flavor)
  (assert* 'read-lisp-character (eqv? #\\ (parsectx-read-char ctx)))
  (let* ((ch (parsectx-peek-char ctx))
         (ret (if (parsectx-is-simple-identifier-char? ch)
                (let ((name (parsectx-read-simple-identifier ctx)))
                  (%char-name->char ctx flavor name))
                (parsectx-read-char ctx)))) ; consume and return peeked char
    (%assert-next-char-is-separator ctx flavor "character")
    (values ret 'atomic)))


(define %char-names (hashtable string-hash string=?
   '("alarm" . #\alarm) '("backspace" . #\backspace) '("delete" . #\delete) '("esc" . #\esc)
   '("linefeed" . #\linefeed) '("newline" . #\newline) '("page" #\page) '("return" . #\return)
   '("space" . #\space) '("tab" . #\tab) '("vtab" . #\vtab)))


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
      (#t
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
