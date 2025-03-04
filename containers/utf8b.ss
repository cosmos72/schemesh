;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers utf8b (0 8 0))
  (export
    integer->char* string->utf8b string->utf8b/0
    utf8b->string utf8b->string utf8b-bytespan->string)

  (import
    (rnrs)
    (rnrs mutable-pairs)
    (rnrs mutable-strings)
    (only (chezscheme) bytevector foreign-procedure fx1+ fx1- string-truncate!)
    (only (schemesh bootstrap)             assert* raise-assertf)
    (only (schemesh containers bytespan)   bytespan? bytespan-length bytespan-peek-data bytespan-peek-beg bytespan-peek-end)
    (only (schemesh containers misc)       bytevector-fill-range!))


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
                                  (ptr fixnum fixnum ptr fixnum) ptr))
        (c-string->utf8b-length (foreign-procedure "c_string_to_utf8b_length"
                                  (ptr fixnum fixnum) fixnum)))
    (lambda (str start end zeropad-byte-n)
      (assert* 'string->utf8b (fx<=? 0 start end (string-length str)))
      (assert* 'string->utf8b (fx>=? zeropad-byte-n 0))
      (if (fx<? start end)
        (let ((byte-n (c-string->utf8b-length str start end)))
          (assert* 'string->utf8b (fixnum? byte-n))
          (let* ((bvec      (make-bytevector (fx+ byte-n zeropad-byte-n)))
                 (written-n (c-string->utf8b-append str start end bvec 0)))
            (unless (and (fixnum? written-n) (fx=? byte-n written-n))
              (raise-string->utf8b-error written-n))
            (when (fx>? zeropad-byte-n 0)
              (bytevector-fill-range! bvec byte-n (fx+ byte-n zeropad-byte-n) 0))
            bvec))
        ; (fx>=? start end)
        (if (fxzero? zeropad-byte-n)
          #vu8()
          (make-bytevector zeropad-byte-n 0))))))


(define (raise-string->utf8b-error err)
  (if (char? err)
    (raise-assertf 'string->utf8b "~s is not a valid unicode scalar value" (char->integer err))
    (raise-assertf 'string->utf8b "invalid arguments")))

#| ;; slower
(define string-range->utf8b
  (let ((c-string->utf8b (foreign-procedure "c_string_range_to_utf8b"
                                          (ptr fixnum fixnum fixnum) ptr)))
    (lambda (str start end zeropad-byte-n)
      (assert* 'string->utf8b (fx<=? 0 start end (string-length str)))
      (assert* 'string->utf8b (fx>=? zeropad-byte-n 0))
      (let ((bvec (c-string->utf8b str start end zeropad-byte-n)))
        (assert* 'string->utf8b (bytevector? bvec))
        bvec))))
|#

;; convert a string to UTF-8b, and return bytevector containing the conversion result.
(define (string->utf8b str)
  (string-range->utf8b str 0 (string-length str) 0))

;; convert a string to UTF-8b, append an additional byte 0 to conversion if not already present,
;; and return bytevector containing the conversion result.
(define (string->utf8b/0 str)
  (let* ((len (string-length str))
         (zeropad-len (if (or (fxzero? len) (not (char=? #\nul (string-ref str (fx1- len)))))
                          1
                          0)))
    (string-range->utf8b str 0 len zeropad-len)))

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
                                  (ptr fixnum fixnum ptr fixnum) ptr)))
    (case-lambda
      ((bvec)
         (utf8b->string bvec 0 (bytevector-length bvec)))
      ((bvec start end)
        (assert* 'utf8b->string (fx<=? 0 start end (bytevector-length bvec)))
        (let* ((max-n     (fx- end start))
               (str       (make-string max-n))
               (written-n (c-utf8b->string-append bvec start end str 0)))
          (if (fx<? written-n max-n)
            (string-truncate! str written-n)
            (assert* 'utf8b->string (fx<=? written-n max-n)))
          str)))))

;; convert a bytespan range from UTF-8b to string, and return string containing the conversion result.
(define utf8b-bytespan->string
  (case-lambda
    ((bspan)
      (assert* 'utf8b-bytespan->string (bytespan? bspan))
      (utf8b->string (bytespan-peek-data bspan) (bytespan-peek-beg bspan) (bytespan-peek-end bspan)))
    ((bspan start end)
      (assert* 'utf8b-bytespan->string (bytespan? bspan))
      (assert* 'utf8b-bytespan->string (fx<=? 0 start end (bytespan-length bspan)))
      (let ((offset (bytespan-peek-beg bspan)))
        (utf8b->string (bytespan-peek-data bspan) (fx+ offset start) (fx+ offset end))))))


) ; close library
