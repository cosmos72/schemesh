;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh containers utf8b (0 9 1))
  (export
    codepoint? codepoint-utf8b? integer->char* string->utf8b string->utf8b/0
    utf8b->string utf8b->string-copy! utf8b-bytespan->string)

  (import
    (rnrs)
    (rnrs mutable-pairs)
    (rnrs mutable-strings)
    (only (chezscheme) bytevector foreign-procedure fx1+ fx1- string-truncate!)
    (only (schemesh bootstrap)             assert* fx<=?* raise-assertf)
    (only (schemesh containers bytespan)   bytespan? bytespan-length bytespan-peek-data bytespan-peek-beg bytespan-peek-end)
    (only (schemesh containers bytevector) subbytevector-fill!))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;     some additional char functions    ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; similar to (integer->char) but it INTENTIONALLY allows codepoints
;; in the range #xDC80..#xDCFF that are used by UTF-8b encoding
;; to represent raw bytes #x80..#xFF converted to Unicode codepoints
;;
;; For a definition of UTF-8b, see
;;   https://peps.python.org/pep-0383
;;   https://web.archive.org/web/20090830064219/http://mail.nl.linux.org/linux-utf8/2000-07/msg00040.html
(define integer->char*
  (let ((surrogate-chars ((foreign-procedure "c_string_fill_utf8b_surrogate_chars" (ptr) ptr) (make-string #x80))))
    (lambda (codepoint)
      (if (fx<=? #xDC80 codepoint #xDCFF)
        (string-ref surrogate-chars (fxand #x7F codepoint))
        (integer->char codepoint))))) ; may raise exception


;; return #t if n is a fixnum in the range 0 .. #x10FFFF
;; that can be converted to unicode char according to UTF-8b specifications.
;;
;; never throws.
;;
;; if (codepoint-utf8b? n) returns #t, (integer->char* n) is guaranteed not to throw
(define (codepoint-utf8b? n)
  (and (fixnum? n)
       (or (fx<=?      0 n #xD7FF)
           (fx<=? #xDC80 n #xDCFF) ;; UTF-8b
           (fx<=? #xE000 n #x10FFFF))))


;; return #t if n is a fixnum in the range 0 .. #x10FFFF
;; that can be converted to unicode char according to Unicode specifications.
;;
;; never throws.
;;
;; if (codepoint? n) returns #t, (integer->char n) is guaranteed not to throw
(define (codepoint? n)
  (and (fixnum? n)
       (or (fx<=?      0 n #xD7FF)
           (fx<=? #xE000 n #x10FFFF))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;     string -> UTF-8b functions    ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (raise-string->utf8b-error err)
  (if (char? err)
    (raise-assertf 'string->utf8b "~s is not a valid unicode scalar value" (char->integer err))
    (raise-assertf 'string->utf8b "invalid arguments")))


;; convert a portion of a string to UTF-8b, and return bytevector containing the conversion result.
;;
;; For a definition of UTF-8b, see
;;   https://peps.python.org/pep-0383
;;   https://web.archive.org/web/20090830064219/http://mail.nl.linux.org/linux-utf8/2000-07/msg00040.html
(define string->utf8b
  (let ((c-string->utf8b-append (foreign-procedure "c_string_to_utf8b_append"
                                  (ptr fixnum fixnum ptr fixnum) ptr))
        (c-string->utf8b-length (foreign-procedure "c_string_to_utf8b_length"
                                  (ptr fixnum fixnum) fixnum)))
    (case-lambda
      ((str start end zeropad-byte-n)
        (assert* 'string->utf8b (fx<=?* 0 start end (string-length str)))
        (assert* 'string->utf8b (fx>=? zeropad-byte-n 0))
        (if (fx<? start end)
          (let ((byte-n (c-string->utf8b-length str start end)))
            (assert* 'string->utf8b (fixnum? byte-n))
            (let* ((bvec      (make-bytevector (fx+ byte-n zeropad-byte-n)))
                   (written-n (c-string->utf8b-append str start end bvec 0)))
              (unless (and (fixnum? written-n) (fx=? byte-n written-n))
                (raise-string->utf8b-error written-n))
              (when (fx>? zeropad-byte-n 0)
                (subbytevector-fill! bvec byte-n (fx+ byte-n zeropad-byte-n) 0))
              bvec))
          ; (fx>=? start end)
          (if (fxzero? zeropad-byte-n)
            #vu8()
            (make-bytevector zeropad-byte-n 0))))
      ((str)
        (string->utf8b str 0 (string-length str) 0)))))





;; convert a string to UTF-8b, append an additional byte 0 to conversion if not already present,
;; and return bytevector containing the conversion result.
(define (string->utf8b/0 str)
  (let* ((len (string-length str))
         (zeropad-len (if (or (fxzero? len) (not (char=? #\nul (string-ref str (fx1- len)))))
                          1
                          0)))
    (string->utf8b str 0 len zeropad-len)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;     UTF-8b -> string functions    ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; convert a bytevector range from UTF-8b to string, and return string containing the conversion result.
;;
;; For a definition of UTF-8b, see
;;   https://peps.python.org/pep-0383
;;   https://web.archive.org/web/20090830064219/http://mail.nl.linux.org/linux-utf8/2000-07/msg00040.html
(define utf8b->string
  (let ((c-utf8b->string-append (foreign-procedure "c_bytevector_utf8b_to_string_append"
                                  (ptr fixnum fixnum ptr fixnum fixnum ptr) void)))
    (case-lambda
      ((bvec start end)
        (assert* 'utf8b->string (fx<=?* 0 start end (bytevector-length bvec)))
        (let* ((max-n  (fx- end start))
               (str    (make-string max-n))
               (pair   (cons #t 0)))
          (c-utf8b->string-append bvec start end str 0 max-n pair)
          (let ((byte-n (car pair))
                (char-n (cdr pair)))
            (assert* 'utf8b->string (fx<=?* 0 char-n byte-n max-n))
            (when (fx<? char-n max-n)
              (string-truncate! str char-n))
            str)))
      ((bvec)
         (utf8b->string bvec 0 (bytevector-length bvec))))))


;; convert a bytevector range from UTF-8b to characters, and write such characters into specified string range.
;; return two values: the number of converted bytes, and the number of written characters.
(define utf8b->string-copy!
  (let ((c-utf8b->string-append (foreign-procedure "c_bytevector_utf8b_to_string_append"
                                  (ptr fixnum fixnum ptr fixnum fixnum ptr) void)))
    (case-lambda
      ((bvec bv-start bv-end str s-start s-end eof?)
        (assert* 'utf8b->string-copy! (fx<=?* 0 bv-start bv-end (bytevector-length bvec)))
        (assert* 'utf8b->string-copy! (fx<=?* 0 s-start  s-end  (string-length str)))
        (let ((pair (cons eof? 0)))
          (c-utf8b->string-append bvec bv-start bv-end str s-start s-end pair)
          (let ((byte-n (car pair))
                (char-n (cdr pair)))
            (assert* 'utf8b->string (fx<=?* 0 char-n byte-n (fx- bv-end bv-start)))
            (assert* 'utf8b->string (fx<=? char-n (fx- s-end s-start)))
            (values byte-n char-n))))
      ((bvec str eof?)
         (utf8b->string-copy! bvec 0 (bytevector-length bvec) str 0 (string-length str) eof?))
      ((bvec str)
         (utf8b->string-copy! bvec 0 (bytevector-length bvec) str 0 (string-length str) #t)))))


;; convert a bytespan range from UTF-8b to string, and return string containing the conversion result.
(define utf8b-bytespan->string
  (case-lambda
    ((bspan start end)
      (assert* 'utf8b-bytespan->string (bytespan? bspan))
      (assert* 'utf8b-bytespan->string (fx<=?* 0 start end (bytespan-length bspan)))
      (let ((offset (bytespan-peek-beg bspan)))
        (utf8b->string (bytespan-peek-data bspan) (fx+ offset start) (fx+ offset end))))
    ((bspan)
      (assert* 'utf8b-bytespan->string (bytespan? bspan))
      (utf8b->string (bytespan-peek-data bspan) (bytespan-peek-beg bspan) (bytespan-peek-end bspan)))))


) ; close library
