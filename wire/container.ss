;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; customize how "span" objects are serialized/deserialized

(define (len/span pos obj)
  (len/container pos obj (span-length obj) span-ref))

;; tag was already read and consumed. only read serialized n and elements
(define (get/span bv pos end)
  (get/container bv pos end make-span span-set!))

(define (put/span bv pos obj)
  (put/container bv pos tag-span obj (span-length obj) span-ref))


;; customize how "gbuffer" objects are serialized/deserialized

(define (len/gbuffer pos obj)
  (len/container pos obj (gbuffer-length obj) gbuffer-ref))

;; tag was already read and consumed. only read serialized n and elements
(define (get/gbuffer bv pos end)
  (get/container bv pos end make-gbuffer gbuffer-set!))

(define (put/gbuffer bv pos obj)
  (put/container bv pos tag-gbuffer obj (gbuffer-length obj) gbuffer-ref))
