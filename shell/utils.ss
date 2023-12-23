;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh shell utils (0 1))
  (export
    sh-autocomplete sh-current-time sh-expand-ps1 sh-home->~ sh-make-linectx)
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme) current-date date-hour date-minute date-second fx1+ fx1-)
    (schemesh bootstrap)
    (schemesh containers)
    (schemesh lineedit io)
    (schemesh lineedit parenmatcher)
    (schemesh lineedit parser)
    (schemesh lineedit linectx)
    (schemesh lineedit)
    (schemesh posix misc)
    (schemesh parser)
    (schemesh shell jobs))

; update linectx-completion-stem and linectx-completions with possible completions
(define (sh-autocomplete lctx)
  ; TODO: handle lines longer than tty width
  (let ((line  #f) ; (linectx-line lctx))
        (pos   (linectx-ix lctx))
        (stem  (linectx-completion-stem lctx))
        (completions (linectx-completions lctx)))
    (charspan-clear! stem)
    (span-clear! completions)
    (while (and (fx>? pos 0) (char>=? (charline-ref line (fx1- pos)) #\space))
      (charspan-insert-front! stem (charline-ref line (fx1- pos)))
      (set! pos (fx1- pos)))
    (unless (charspan-empty? stem)
      (list-iterate (directory-u8-list #vu8(46) (charspan->string stem))
        (lambda (elem)
          (span-insert-back! completions
            (string->charspan* (utf8->string (cdr elem)))))))))

; return string containing current time in 24-hour HH:MM:SS format.
; return number of appended bytes
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

; update linectx-prompt and linectx-prompt-length with new prompt
(define (sh-expand-ps1 lctx)
  (let* ((src (sh-env-ref sh-globals "SCHEMESH_PS1")) ; string
         (prompt (linectx-prompt lctx))
         (prompt-len 0)
         (hidden  0)
         (escape? #f)
         (%append-char (lambda (ch)
           (bytespan-insert-back/utf8! prompt ch)
           (when (fx<=? hidden 0)
             (set! prompt-len (fx1+ prompt-len)))))
         (%append-charspan (lambda (csp)
           (bytespan-insert-back/cspan! prompt csp)
           (when (fx<=? hidden 0)
             (set! prompt-len (fx+ prompt-len (charspan-length csp))))))
         (%append-string (lambda (str)
           (%append-charspan (string->charspan* str)))))
    (bytespan-clear! prompt)
    (bytespan-reserve-back! prompt (string-length src))
    (string-iterate src
      (lambda (i ch)
        (if escape?
          (begin
            (case ch
              ((#\[)     (set! hidden (fx1+ hidden)))
              ((#\])     (set! hidden (fx1- hidden)))
              ((#\a)     (%append-char     #\x07))
              ((#\e)     (%append-char     #\x1b))
              ((#\h #\H) (%append-string (c-hostname)))
              ; ((#\n)   (%append-char     #\newline)) ; breaks computing prompt-end-x/y
              ; ((#\r)   (%append-char     #\return))  ; breaks computing prompt-end-x/y
              ((#\s)     (%append-string   (symbol->string (linectx-parser-name lctx))))
              ((#\@ #\A #\T #\t)    (%append-string (sh-current-time ch)))
              ((#\u)     (%append-string   (sh-env-ref sh-globals "USER")))
              ((#\w)     (%append-charspan (sh-home->~ (sh-cwd))))
              (else      (%append-char     ch)))
            (set! escape? #f))
          (case ch
            ((#\\)   (set! escape? #t))
            (else    (%append-char ch))))))
    (linectx-prompt-length-set! lctx prompt-len)))

; if charspan path begins with user's $HOME, replace it with ~
(define (sh-home->~ path)
  (let ((ret path)
        (home (sh-env-ref sh-globals "HOME" #f)))
    (when (string? home)
      (let ((home-len (string-length home))
            (path-len (charspan-length path)))
        (when (and (fx<=? home-len path-len)
                   (charspan-range=? (string->charspan* home) 0 path 0 home-len))
          (set! ret (string->charspan "~"))
          (charspan-insert-back/cspan! ret path home-len (fx- path-len home-len)))))
    ret))


(define (sh-make-linectx)
  (make-linectx* sh-expand-ps1 (make-parenmatcher) sh-autocomplete #f))

) ; close library
