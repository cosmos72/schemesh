;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers utf8b (0 1))
  (export
    integer->char* string->utf8b string->utf8b/0 utf8b->string)

  (import
    (rnrs)
    (rnrs mutable-pairs)
    (rnrs mutable-strings)
    (only (chezscheme) bytevector foreign-procedure fx1+ fx1-)
    (only (schemesh bootstrap) assert* raise-assertf)
    (only (schemesh containers misc) bytevector-fill-range!))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;     some additional char functions    ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; similar to (integer->char) but it INTENTIONALLY allows codepoints
;; in the range #xDC80..#xDCFF which is used by UTF-8b encoding
;; to represent raw bytes 0x80..0xFF converted to Unicode codepoints
(define integer->char*
  (let ((c-integer->char (foreign-procedure "c_integer_to_char" (unsigned-32) scheme-object)))
    (lambda (codepoint)
      (if (fx<=? #xDC80 codepoint #xDCFF)
        (c-integer->char codepoint)
        (integer->char codepoint))))) ; may raise exception


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;     string -> UTF-8b functions    ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; convert a portion of a string to UTF-8b, and return bytevector containing the conversion result.
;;
;; For a definition of UTF-8b, see
;;   https://peps.python.org/pep-0383
;;   https://web.archive.org/web/20090830064219/http://mail.nl.linux.org/linux-utf8/2000-07/msg00040.html
(define string-range->utf8b
  (let ((c-string->utf8b-append (foreign-procedure "c_string_to_utf8b_append"
                                  (scheme-object fixnum fixnum scheme-object fixnum) scheme-object))
        (c-string->utf8b-length (foreign-procedure "c_string_to_utf8b_length"
                                  (scheme-object fixnum fixnum) fixnum)))
    (lambda (str start n zeropad-byte-n)
      (assert* 'string->utf8b (fx<=? 0 start (fx+ start n) (string-length str)))
      (assert* 'string->utf8b (fx>=? zeropad-byte-n 0))
      (let ((byte-n (c-string->utf8b-length str start n)))
        (assert* 'string->utf8b (fixnum? byte-n))
        (let* ((bvec (make-bytevector (fx+ byte-n zeropad-byte-n)))
               (written-n (c-string->utf8b-append str start n bvec 0)))
          (unless (and (fixnum? written-n) (fx=? byte-n written-n))
            (string->utf8b-error written-n))
          (when (fx>? zeropad-byte-n 0)
            (bytevector-fill-range! bvec byte-n zeropad-byte-n 0))
          bvec)))))

(define (string->utf8b-error err)
  (if (char? err)
    (raise-assertf 'string->utf8b "~s is not a valid unicode scalar value" (char->integer err))
    (raise-assertf 'string->utf8b "invalid arguments")))

#| ;; slower
(define string-range->utf8b
  (let ((c-string->utf8b (foreign-procedure "c_string_range_to_utf8b"
                                          (scheme-object fixnum fixnum fixnum) scheme-object)))
    (lambda (str start n zeropad-byte-n)
      (assert* 'string->utf8b (fx<=? 0 start (fx+ start n) (string-length str)))
      (assert* 'string->utf8b (fx>=? zeropad-byte-n 0))
      (let ((bvec (c-string->utf8b str start n zeropad-byte-n)))
        (assert* 'string->utf8b (bytevector? bvec))
        bvec))))
|#

;; convert a string to UTF-8b, and return bytevector containing the conversion result.
(define (string->utf8b str)
  (string-range->utf8b str 0 (string-length str) 0))

;; convert a string to UTF-8b, append an additional byte 0 to conversion,
;; and return bytevector containing the conversion result.
(define (string->utf8b/0 str)
  (string-range->utf8b str 0 (string-length str) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;     UTF-8b -> string functions    ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; convert a portion of a bytevector from UTF-8b to string, and return string containing the conversion result.
;;
;; For a definition of UTF-8b, see
;;   https://peps.python.org/pep-0383
;;   https://web.archive.org/web/20090830064219/http://mail.nl.linux.org/linux-utf8/2000-07/msg00040.html
(define utf8b-range->string
  (let ((c-utf8b->string-append (foreign-procedure "c_utf8b_to_string_append"
                                  (scheme-object fixnum fixnum scheme-object fixnum) scheme-object))
        (c-utf8b->string-length (foreign-procedure "c_utf8b_to_string_length"
                                  (scheme-object fixnum fixnum) fixnum)))
    (lambda (bvec start n)
      (assert* 'utf8b->string (fx<=? 0 start (fx+ start n) (bytevector-length bvec)))
      (let* ((char-n (c-utf8b->string-length bvec start n))
             (str (make-string char-n))
             (written-n (c-utf8b->string-append bvec start n str 0)))
        (assert* 'utf8b->string (fx=? char-n written-n))
        str))))

;; convert a bytevector from UTF-8b to string, and return string containing the conversion result.
(define (utf8b->string bvec)
  (utf8b-range->string bvec 0 (bytevector-length bvec)))


) ; close library
