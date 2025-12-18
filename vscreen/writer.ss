;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file vscreen/all.ss


;; customize how vcolors objects are printed
(record-writer (record-type-descriptor %vcolors)
  (lambda (cols port writer)
    (display "(vcolors " port)
    (vcolor-write (vcolors->fg cols) port)
    (display " " port)
    (vcolor-write (vcolors->bg cols) port)
    (display ")" port)))


;; customize how vcellspan objects are printed
(record-writer (record-type-descriptor %vcellspan)
  (lambda (csp port writer)
    (display "(string->vcellspan " port)
    (vcellspan-write csp port)
    (display ")" port)))


;; customize how vbuffer objects are printed
(record-writer (record-type-descriptor %vbuffer)
  (lambda (cgb port writer)
    (display "(string->vbuffer " port)
    (vbuffer-write cgb port)
    (display ")" port)))


;; customize how vline objects are printed
(record-writer (record-type-descriptor %vline)
  (lambda (line port writer)
    (display "(vline " port)
    (vline-write line port)
    (display ")" port)))


;; customize how vscreen objects are printed
(record-writer (record-type-descriptor vscreen)
  (lambda (screen port writer)
    (display "(vscreen " port)
    (vscreen-write screen port)
    (display ")" port)))


;; customize how vlines objects are printed
(record-writer (record-type-descriptor %vlines)
  (lambda (lines port writer)
    (display "(vlines" port)
    (vlines-iterate lines
      (lambda (i line)
        (display #\space port)
        (vline-write line port)))
    (display ")" port)))


;; customize how vhistory objects are printed
(record-writer (record-type-descriptor %vhistory)
  (lambda (hist port writer)
    (display "(vhistory" port)
    (gbuffer-iterate hist
      (lambda (i elem)
        (display #\space port)
        (writer elem port)))
    (display ")" port)))
