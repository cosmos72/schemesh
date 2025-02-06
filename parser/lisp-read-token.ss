;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file parser/lisp.ss



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


;; read a lisp string literal starting with double quotes.
;; also supports string literals "..." containing hexadecimal escape sequences \x...;
;;   representing valid UTF-8b codepoints
;;
;; returns two values:
;;   token value: the unescaped string
;;   token type: 'atomic
(define (read-lisp-string ctx flavor)
  (assert* 'read-lisp-sharp (eqv? #\" (parsectx-peek-char ctx)))
  ;; TODO: implement
  (%read-token ctx))


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
  (let ((ch (parsectx-peek-char ctx)))
    (if (parsectx-is-simple-identifier-char? ch)
      (let ((name (parsectx-read-simple-identifier ctx)))
        (values (%char-name->char ctx flavor name) 'atomic))
      (begin
        (parsectx-read-char ctx)
        ;; TODO: check that next char is whitespace or a parenthesis
        (values ch 'atomic)))))


;; minimal wrapper around Chez Scheme (read-token)
;;
;; returns two values:
;;   token value
;;   token type
(define (%read-token ctx)
  (let-values (((type value start end) (read-token (parsectx-in ctx))))
    (values value type)))



(define %char-names (hashtable string-hash string=?
   '("alarm" . #\alarm) '("backspace" . #\backspace) '("delete" . #\delete) '("esc" #\esc)
   '("linefeed" . #\linefeed) '("newline" . #\newline) '("page" #\page) '("return" #\return)
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
            (syntax-errorf ctx (caller-for flavor) "invalid character #\\~a" name))))))


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
