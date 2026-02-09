;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file io/obj/obj.ss


;; base type for readers that wrap another "inner" reader
(define-record-type (nested-reader %make-nested-reader nested-reader?)
  (parent obj-reader)
  (fields
    inner)  ;; wrapped reader
  (protocol
    (lambda (args->new)
      (lambda (get-proc skip-proc close-proc inner)
         ((args->new get-proc skip-proc close-proc)
            inner))))
  (nongenerative %nested-reader-7c46d04b-34f4-4046-b5c7-b63753c1be42))


;; can be called by subtypes to detect EOF in the inner reader
(define (nested-reader-inner-eof? rx)
  (obj-reader-eof? (nested-reader-inner rx)))


;; can be called by subtypes to close the inner reader
(define (nested-reader-inner-close rx)
  (obj-reader-close (nested-reader-inner rx)))


;; can be called by subtypes to get next element from the inner reader
(define (nested-reader-inner-get rx)
  (obj-reader-get (nested-reader-inner rx)))


;; can be called by subtypes to skip next element from the inner reader
(define (nested-reader-inner-skip rx)
  (obj-reader-skip (nested-reader-inner rx)))


