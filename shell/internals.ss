;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh shell internals (0 1))
  (export
    %sh-redirect/fd-symbol->char  %sh-redirect/file-symbol->char
    %sh-redirect/fd-char->symbol  %sh-redirect/file-char->symbol)
  (import
    (rnrs)
    (only (schemesh bootstrap) raise-errorf))


(define (%sh-redirect/fd-symbol->char caller symbol)
  (case symbol
    ((<&) #\<)
    ((>&) #\>)
    (else
      (raise-errorf caller "invalid redirect to fd direction, must be <& or >&: ~a" symbol))))


(define (%sh-redirect/file-symbol->char caller symbol)
  (case symbol
    ((<) #\<)
    ((>) #\>)
    ((<>) (integer->char #x2276)) ; #\≶
    ((>>) (integer->char #x00bb)) ; #\»
    (else
      (raise-errorf caller "invalid redirect to file direction, must be < > <> or >>: ~a" symbol))))


(define (%sh-redirect/fd-char->symbol caller ch)
  (case ch
    ((#\<) '<&)
    ((#\>) '>&)
    (else
      (raise-errorf caller "invalid redirect to fd character, must be <& or >&: ~a" ch))))


(define (%sh-redirect/file-char->symbol caller ch)
  (case (char->integer ch)
    ((#x3c) '<)
    ((#x3e) '>)
    ((#x2276) '<>)
    ((#x00bb) '>>)
    (else
      (raise-errorf caller "invalid redirect to file character, must be < <> > or >>: ~a" ch))))



) ; close library
