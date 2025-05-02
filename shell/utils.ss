;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh shell utils (0 9 0))
  (export
    c-username sh-autocomplete sh-expand-ps1 sh-current-time sh-default-ps1 sh-home->~ sh-make-linectx)
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme) current-date date-hour date-minute date-second foreign-procedure fx1+ fx1-
                       string->immutable-string)
    (schemesh bootstrap)
    (schemesh containers)
    (schemesh lineedit linectx)
    (schemesh lineedit lineedit)
    (only (schemesh lineedit paren) paren-name)
    (schemesh lineedit parser)
    (only (schemesh posix fd) c-hostname)
    (schemesh parser)
    (schemesh screen vlines io)
    (schemesh screen vscreen)
    (schemesh shell autocomplete)
    (schemesh shell job))


;; update linectx-completion-stem and linectx-completions with possible completions.
;; usually stored in parameter (linectx-completion-proc)
(define (sh-autocomplete lctx)
  (let* ((paren (lineedit-paren-find/surrounds-cursor lctx))
         (func  (%sh-autocomplete-func lctx paren))
         (completions-span (linectx-completions lctx)))
    ; (debugf "> sh-autocomplete paren = ~s, func = ~s" paren func)
    (span-clear! completions-span)
    (func lctx paren completions-span)
    ; (debugf "< sh-autocomplete completions = ~s" completions-span)
    ))


;; return the syntax-aware function that lists autocompletions.
;; The parser name to use is extracted from paren, or from current parser if paren is #f
(define (%sh-autocomplete-func lctx paren)
  (let ((parser-name (if paren (paren-name paren) (linectx-parser-name lctx))))
    (or (sh-autocomplete-func parser-name) sh-autocomplete-shell)))


;; return string containing current time in 24-hour HH:MM:SS format.
;; return number of appended bytes
(define (sh-current-time ch)
  (let* ((%display (lambda (str pos val)
           (let-values (((hi lo) (div-and-mod val 10)))
             (string-set! str pos        (integer->char (fx+ 48 (fxmod hi 10))))
             (string-set! str (fx1+ pos) (integer->char (fx+ 48 (fxmod lo 10)))))))
         (d (current-date))
         (hh (date-hour d))
         (len (case ch ((#\T #\t) 8) (else 5)))
         (str (make-string len #\:)))
    (%display str 0
      (case ch
        ((#\@ #\T)
           (let ((hh12 (fxmod hh 12)))
             (if (fxzero? hh12) 12 hh12)))
        (else hh)))
    (%display str 3 (date-minute d))
    (when (fx>=? len 8)
      (%display str 6 (date-second d)))
    str))


(define current-euid
  (let ((ret ((foreign-procedure "c_euid_get" () int))))
    (lambda ()
      ret)))

(define uid->username (foreign-procedure "c_get_username" (int) ptr))

(define c-username
  (let ((c-username-value (uid->username (current-euid))))
    (lambda ()
      (if (string? c-username)
        c-username-value
        (sh-env-ref #t "USER")))))

(define (color high? fg str)
  (string-append (if high? "\\[\\e[1;3" "\\[\\e[;3") fg "m\\]"
                 str
                 "\\[\\e[m\\]"))

(define (black    str) (color #f "0" str))
(define (red      str) (color #f "1" str))
(define (green    str) (color #f "2" str))
(define (yellow   str) (color #f "3" str))
(define (blue     str) (color #f "4" str))
(define (magenta  str) (color #f "5" str))
(define (cyan     str) (color #f "6" str))
(define (white    str) (color #f "7" str))

(define (black+   str) (color #t "0" str))
(define (red+     str) (color #t "1" str))
(define (green+   str) (color #t "2" str))
(define (yellow+  str) (color #t "3" str))
(define (blue+    str) (color #t "4" str))
(define (magenta+ str) (color #t "5" str))
(define (cyan+    str) (color #t "6" str))
(define (white+   str) (color #t "7" str))

(define e-host   "\\h")
(define e-syntax "\\s")
(define e-user   "\\u")
(define e-time   "\\t")
(define e-cwd    "\\w")

(define (window-title str)
  (string-append "\\[\\e]0;" str "\\a\\]"))

(define s+ string-append)

(define default-ps1
  (let ((user-color (if (eqv? 0 (current-euid)) red+ cyan+)))
    (string->immutable-string
     (s+ (window-title (s+ e-user "@" e-host " " e-cwd))
         (green        (s+ e-syntax " "))
         (user-color   e-user) "@"
         (yellow+      e-host) ":"
         (blue+        e-cwd)  ":"))))

(define (sh-default-ps1) default-ps1)

;; update linectx-prompt and linectx-prompt-length with new prompt
;; obtained by parsing environment variable $SCHEMESH_PS1
(define (sh-expand-ps1 lctx)
  (let* ((src (sh-env-ref #t "SCHEMESH_PS1" default-ps1)) ; string
         (prompt (linectx-prompt lctx))
         (prompt-len 0)
         (hidden  0)
         (escape? #f)
         (%append-char (lambda (ch)
           (bytespan-insert-right/char! prompt ch)
           (when (fx<=? hidden 0)
             (set! prompt-len (fx1+ prompt-len)))))
         (%append-string (lambda (str)
           (bytespan-insert-right/string! prompt str)
           (when (fx<=? hidden 0)
             (set! prompt-len (fx+ prompt-len (string-length str))))))
         (%append-charspan (lambda (csp)
           (bytespan-insert-right/charspan! prompt csp)
           (when (fx<=? hidden 0)
             (set! prompt-len (fx+ prompt-len (charspan-length csp)))))))
    (bytespan-clear! prompt)
    (bytespan-reserve-right! prompt (string-length src))
    (string-iterate src
      (lambda (i ch)
        (if escape?
          (begin
            (case ch
              ((#\[)     (set! hidden (fx1+ hidden)))
              ((#\])     (set! hidden (fx1- hidden)))
              ((#\a)     (%append-char     #\x07))
              ((#\e)     (%append-char     #\x1B))
              ((#\h #\H) (%append-string   (c-hostname)))
              ; ((#\n)   (%append-char     #\newline)) ; breaks computing prompt-end-x/y
              ; ((#\r)   (%append-char     #\return))  ; breaks computing prompt-end-x/y
              ((#\s)     (%append-string   (symbol->string (linectx-parser-name lctx))))
              ((#\@ #\A
                #\T #\t) (%append-string   (sh-current-time ch)))
              ((#\u)     (%append-string   (c-username)))
              ((#\w)     (%append-charspan (sh-home->~ (sh-cwd))))
              (else      (%append-char     ch)))
            (set! escape? #f))
          (case ch
            ((#\\)   (set! escape? #t))
            (else    (%append-char ch))))))
    (linectx-prompt-length-set! lctx prompt-len)))


;; if charspan path begins with user's home directory,
;; return a copy of it where the initial user's home directory is replaced by "~"
;;
;; otherwise return path.
(define (sh-home->~ path)
  (let* ((ret path)
         (home (sh-env-ref #t "HOME" #f))
         (home-len (if (string? home) (string-length home) 0))
         (path-len (charspan-length path)))
    (when (and (not (fxzero? home-len))
               (fx<=? home-len path-len)
               (charspan=? (string->charspan* home) 0 path 0 home-len))
      (set! ret (string->charspan "~"))
      (charspan-insert-right/charspan! ret path home-len path-len))
    ret))



(define sh-make-linectx
  (case-lambda
    (()
      (sh-make-linectx* (parsers) #f))
    ((enabled-parsers)
      (sh-make-linectx* enabled-parsers #f))
    ((enabled-parsers history-path)
      (sh-make-linectx* enabled-parsers history-path))))


(define (sh-make-linectx* enabled-parsers history-path)
  (make-linectx* (make-parenmatcher) enabled-parsers history-path))


(begin
  (unless (linectx-prompt-proc)
    (linectx-prompt-proc sh-expand-ps1))

  (unless (linectx-completion-proc)
    (linectx-completion-proc sh-autocomplete))

) ; close begin

) ; close library
