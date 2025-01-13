;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh posix pattern (0 1))
  (export
    sh-pattern sh-pattern? span->sh-pattern* sh-pattern->span* sh-pattern-match? sh-wildcard?)
  (import
    (rnrs)
    (only (chezscheme) fx1+ fx1- record-writer void)
    (only (schemesh bootstrap) assert* raise-assertf)
    (only (schemesh containers misc) string-find-char string-range=?)
    (schemesh containers span))


;; return #t if obj is a wildcard symbol, i.e. one of * ? ~ % %!
;; otherwise return #f
(define (sh-wildcard? obj)
  (and (symbol? obj) (memq obj '(* ? ~ % %!)) #t))


(define-record-type
  (%pattern %make-pattern sh-pattern?)
  (fields
     (immutable sp     pattern-span)
     (immutable fixed? pattern-fixed?)) ; #t if sp only contains strings
  ; (nongenerative #{%pattern ...})
  )


;; create a sh-pattern containing a list of strings and wildcard symbols.
;;
;; each symbol must be one of: * ? ~ % %!
;;   each % and %! must be followed by a string
(define (sh-pattern . strings-and-symbols)
  (span->sh-pattern* (list->span strings-and-symbols)))


;; view an existing span as a sh-pattern.
;;
;; each element in span must be a string or wildcard symbol.
;; each symbol must be one of: * ? ~ % %!
;;   each % and %! must be followed by a string
;;
;; Do NOT modify the span after calling this procedure.
(define (span->sh-pattern* sp)
  (assert* 'span->sh-pattern* (span? sp))
  (let* ((n (span-length sp))
         (fixed? (%validate-span sp 0 n #f #f)))
    ; adjacent strings are not allowed
    ; => if span contains only strings, it must have length 1
    (when (and fixed? (not (fx=? n 1)))
      (raise-assertf 'sh-pattern "adjacent strings are not allowed, consider merging them: ~s" sp))
    (%make-pattern sp fixed?)))


;; view a sh-pattern as a span.
;;
;; Do NOT modify the returned span.
(define (sh-pattern->span* p)
  (pattern-span p))


;; validate a span to be used inside a sh-pattern.
;; raises condition if span contents is not valid for sh-pattern.
;;
;; returns #t if span contains only strings,
;; otherwise returns #f
(define (%validate-span sp i n contains-wildcard? prev-is-string?)
  (if (fx>=? i n)
    (not contains-wildcard?)
    (let ((obj (span-ref sp i))
          (i+1 (fx1+ i)))
      (cond
        ((string? obj)
          (when prev-is-string?
            (raise-assertf 'sh-pattern "adjacent strings are not allowed, consider merging them: ~s ~s"
              obj (span-ref sp (fx1- i))))
          (%validate-span sp (fx1+ i) n contains-wildcard? #t))
        ((sh-wildcard? obj)
          (when (memq obj '(% %!))
            (when (fx=? i+1 n)
              (raise-assertf 'sh-pattern "missing string after wildcard symbol '~a" obj))
            (set! i i+1)
            (let ((obj2 (span-ref sp i)))
              (unless (string? obj2)
                (raise-assertf 'sh-pattern "expecting a string after wildcard symbol '~a, found ~s" obj obj2))))
          (%validate-span sp (fx1+ i) n #t #f))
        (#t
          (raise-assertf 'sh-pattern "expecting a string or sh-wildcard? symbol, found ~s" obj))))))


;; Determine whether sh-pattern matches specified string. Returns #t or #f.
;;
;; Note: if a sh-pattern contains one or more wildcard symbols,
;; it intentionally never matches the strings "." or ".."
(define (sh-pattern-match? p str)
  (assert* 'sh-pattern-match? (sh-pattern? p))
  (assert* 'sh-pattern-match? (string? str))
  (let* ((sp  (pattern-span p))
         (n   (span-length sp))
         (len (string-length str)))
    (cond
      ((pattern-fixed? p)
        (fx=? len (%pattern-match/string? (span-ref sp 0) str 0 len)))
      ((or (string=? str ".") (string=? str ".."))
        #f)
      (#t
        (%pattern-match/wildcard? sp 0 n str 0 len)))))


;; Determine whether range [str-start, str-end) of string str
;; starts with string key.
;;
;; if match is successful returns (string-length key),
;;   which is guaranteed to be <= (fx- str-end str-start)
;; otherwise returns #f.
(define (%pattern-match/string? key str str-start str-end)
  (let* ((key-len   (string-length key))
         (str-len   (fx- str-end str-start))
         (compare-n (fxmin key-len str-len)))
    (if (and (fx=? key-len compare-n)
             (string-range=? key 0 str str-start compare-n))
      key-len
      #f)))


;; Determine whether the range [sp-start, sp-end] of the span sp - which may also contain wildcards -
;; matches range [str-start, str-end) of string str.
;; Returns #t or #f.
(define (%pattern-match/wildcard? sp sp-start sp-end str str-start str-end)
  (if (fx>=? str-start str-end)
    (fx>=? sp-start sp-end)
    (let ((key (span-ref sp sp-start)))
      (cond
        ((string? key)
          (let ((match-n (%pattern-match/string? key str str-start str-end)))
            (if match-n
              ; string matched, continue matching rest of pattern
              (%pattern-match/wildcard? sp (fx1+ sp-start) sp-end
                                        str (fx+ str-start match-n) str-end)
              #f)))
        ((eq? key '?)
          ; '? matches any single character, except that it does not match an initial #\.
          (if (and (fxzero? str-start) (char=? #\. (string-ref str str-start)))
            #f
            (%pattern-match/wildcard? sp (fx1+ sp-start) sp-end
                                      str (fx1+ str-start) str-end)))
        ((eq? key '*)
          ; TODO: implement
          #f)
        ((memq key '(% %!))
          ; '% "ALTERNATIVES" matches a single character among ALTERNATIVES - does not match initial #\.
          ; '%! "ALTERNATIVES" matches a single character not among ALTERNATIVES - does not match initial #\.
          (let ((ch (string-ref str str-start)))
            (if (and (fxzero? str-start) (char=? #\. ch))
              #f
              (let ((match? (%pattern-match/alternative? key 0 (string-length key) ch)))
                (if (eq? match? (eq? key '%))
                  ; character matched, continue matching rest of pattern
                  (%pattern-match/wildcard? sp (fx1+ sp-start) sp-end
                                            str (fx1+ str-start) str-end)
                  ; character not matched
                  #f)))))
        (#t ; unexpected
          (raise-assertf 'sh-pattern-match? "pattern contains unsupported element: ~s" key))))))


;; Return #t if alternative characters listend in string alt, which may contain ranges as "a-z",
;; matches character ch
(define (%pattern-match/alternative? alt alt-start alt-end ch)
  (let* ((alt-len (fx- alt-start alt-end))
         (dash-pos (string-find-char alt alt-start alt-len #\-)))
    (if (and dash-pos (not fxzero? dash-pos))
      #f ; TODO implement
      (and (string-find-char alt alt-start alt-len ch) #t))))


;;  customize how "sh-pattern" objects are printed
(record-writer (record-type-descriptor %pattern)
  (lambda (p port writer)
    (display "(sh-pattern" port)
    (span-iterate (pattern-span p)
      (lambda (i elem)
        (if (symbol? elem)
          (display " '" port)
          (display #\space port))
        (writer elem port)))
    (display ")" port)))

) ; close library
