;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;;;
;;; define an abstract obj-reader type: generates or reads from somewhere a finite or unlimited sequence of arbitrary values.
;;;
;;; each call to (obj-reader-get r) will return two values:
;;;  either (values elem truish) i.e. the next element
;;;  or (values #<unspecified> #f) when obj-reader is exhausted or after (obj-reader-close r) is called
;;;
;;; existing APIs that were considered, and reasons for not reusing them:
;;;
;;;  racket/stream: assumes that each call to (stream-rest s) returns a different stream,
;;;                 and that after calling (stream-rest s), the original s is not modified
;;;                 and its (stream-first s) returns the same value as before.
;;;
;;;  racket/generator: requires an efficient (yield), which Chez Scheme lacks.
;;;                    implementing it with continuations is very slow.
;;;
;;;  srfi 158 generators: requires generators to be thunks i.e. no-argument procedures,
;;;                       and cannot distinguish between generated value eq? to (eof-object) and generator being exhausted.
;;;                       Also, there is no simple mechanism to add more behavior,
;;;                       as for example closing a generator early,
;;                        or releasing OS-level resources (file descriptors ...)
;;;                       or waiting simultaneously on multiple generators
(library (scheme2k io obj (0 9 3))
  (export
    make-obj-reader obj-reader obj-reader? obj-reader-get obj-reader-close obj-reader-eof?
    make-obj-writer obj-writer obj-writer? obj-writer-put obj-writer-close obj-writer-eof?

    in-reader constant-reader empty-reader list-reader sequence-reader vector-reader
    discard-writer full-writer list-writer)
  (import
    (rnrs)
    (only (chezscheme)                    fx1+ include logbit? procedure-arity-mask record-type-descriptor record-writer reverse! void)
    (only (scheme2k bootstrap)            assert* forever fx<=?* raise-errorf))


(include "io/obj/reader.ss")
(include "io/obj/writer.ss")


;; customize how "obj-reader" objects are printed
(record-writer (record-type-descriptor obj-reader)
  (lambda (r port writer)
    (put-string port "(make-obj-reader ")
    (writer (obj-reader-get-proc r) port)
    (put-char port #\space)
    (writer (obj-reader-close-proc r) port)
    (put-char port #\space)
    (writer (if (obj-reader-eof? r) 'eof #f) port)
    (put-string port ")")))


;; customize how "obj-writer" objects are printed
(record-writer (record-type-descriptor obj-reader)
  (lambda (w port writer)
    (put-string port "(make-obj-reader ")
    (writer (obj-reader-get-proc w) port)
    (put-char port #\space)
    (writer (obj-reader-close-proc w) port)
    (put-char port #\space)
    (writer (if (obj-reader-eof? w) 'eof #f) port)
    (put-string port ")")))


) ; close library