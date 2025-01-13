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
    (only (schemesh containers misc) string-find/char string-range=?)
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
        (let ((key (span-ref sp 0)))
          (and (fx=? len (string-length key))
               (string-range=? key 0 str 0 len))))
      ((or (string=? str ".") (string=? str ".."))
        #f)
      (#t
        (%pattern-match/left sp 0 n str 0 len)))))


;; recursively determine whether the range [sp-start, sp-end] of the span sp,
;; which may also contain wildcards, matches range [str-start, str-end) of string str.
;;
;; returns #t or #f.
(define (%pattern-match/left sp sp-start sp-end str str-start str-end)
  (if (fx>=? str-start str-end)
    (fx>=? sp-start sp-end)
    (let-values (((sp-step str-step) (%pattern-match/left1 sp sp-start sp-end str str-start str-end)))
      (if (and sp-step str-step)
        (%pattern-match/left sp (fx+ sp-start sp-step) sp-end
                             str (fx+ str-start str-step) str-end)
        #f))))


;; determine how many characters of string str range [str-start, str-end)
;; are matched by pattern key (span-ref sp sp-start).
;;
;; return two values:
;; 1. number of consumed span elements, or #f if match failed.
;; 2. number of matched string characters
(define (%pattern-match/left1 sp sp-start sp-end str str-start str-end)
  (let ((key (span-ref sp sp-start)))
    (cond
      ((string? key)
        (values
          (%pattern-match/string key str str-start str-end)
          (string-length key)))
      ((eq? key '?)
        (values
          (%pattern-match/char str str-start str-end)
          1))
      ((eq? key '*)
        (values
          (%pattern-match/left/star sp sp-start sp-end str str-start str-end)
          (fx- str-end str-start)))
      ((memq key '(% %!))
        (values
          (%pattern-match/alternative sp sp-start str str-start str-end)
          1))
      (#t ; unexpected
        (raise-assertf 'sh-pattern-match? "pattern contains unsupported element: ~s" key)))))


;; Determine whether range [str-start, str-end) of string str
;; starts with string key.
;;
;; if match is successful returns 1,
;; otherwise returns #f
(define (%pattern-match/string key str str-start str-end)
  (let* ((key-len   (string-length key))
         (str-len   (fx- str-end str-start))
         (compare-n (fxmin key-len str-len)))
    (if (and (fx=? key-len compare-n)
             (string-range=? key 0 str str-start compare-n))
      1
      #f)))


;; match ? i.e. any single character.
;; does not match an initial #\. i.e. at str-start = 0
;;
;; if match is successful returns 1,
;; otherwise returns #f
(define (%pattern-match/char str str-start str-end)
  (cond
    ((fx>=? str-start str-end)
      #f) ; end of string, cannot match a single character
    ((and (fxzero? str-start) (char=? #\. (string-ref str str-start)))
      #f) ; cannot match an initial #\.
    (#t
      1))) ; match any single character



;; match [ALT] or [!ALT] i.e. alternative characters listed in (span-ref sp (fx1+ sp-start)).
;; never matches an initial #\.
;;
;; [ALT] is represented as '% "ALT" and matches a single character among ALT.
;; [!ALT] is represented as '%! "ALT" and matches a single character not among ALT.
;;
;; ALT alternative characters may also contain ranges as "a-z"
;;
;; if match is successful returns 2,
;; otherwise returns #f
(define (%pattern-match/alternative sp sp-start str str-start str-end)
  (if (or (fx>=? str-start str-end) ; end of string, cannot match a single character
          (and (fxzero? str-start) (char=? #\. (string-ref str str-start)))) ; cannot match an initial #\.
    #f
    (let* ((ch  (string-ref str str-start))
           (alt (span-ref sp (fx1+ sp-start)))
           (alt-len (string-length alt)))
      (if (fxzero? alt-len)
        #f ; missing alternatives
        (let ((match?
                (if (string-find/char alt 1 (fxmax 1 (fx- alt-len 2)) #\-)
                  ; found #\- not at the beginning and not at the end:
                  ; search among alternatives listed as range(s)
                  (%pattern-match/range? alt ch)
                  ; plain string search among alternatives, returns fixnum or #f
                  (string-find/char alt 0 alt-len ch))))
          ; negate the meaning of match? if key is '%!
          (if (if (eq? '% (span-ref sp sp-start)) match? (not match?))
            2
            #f))))))


;; match [alt] i.e. alternative characters listed in string alt,
;; which also contains ranges as "a-z"
;;
;; if match is successful returns #t,
;; otherwise returns #f
(define (%pattern-match/range? alt ch)
  (let ((end (string-length alt)))
    (let %again ((i 0))
      (if (fx>=? i end)
        #f
        (let ((alt-ch (string-ref alt i)))
          (if (and (fx<? (fx+ i 2) end)
                   (char=? #\- (string-ref alt (fx1+ i))))
            ; range match, as "a-z"
            (if (char<=? alt-ch ch (string-ref alt (fx+ i 2)))
              #t                  ; range match successful, return #t
              (%again (fx+ i 3))) ; range match failed, iterate
            ; plain character match
            (if (char=? alt-ch ch)
              #t                       ; char match successful, return #t
              (%again (fx1+ i))))))))) ; char match failed, iterate


;; recursively determine whether the range [sp-start, sp-end] of the span sp,
;; which starts with '* matches range [str-start, str-end) of string str.
;;
;; if match is successful returns (fx- sp-end sp-start)
;; otherwise returns #f
(define (%pattern-match/left/star sp sp-start sp-end str str-start str-end)
  (cond
    ((span-iterate sp (lambda (i key) (eq? '* key)))
      ; a sequence of '* with nothing else matches anything
      (if (and (fxzero? str-start) (char=? #\. (string-ref str 0)))
        #f ; except that it cannot match an initial #\.
        (fx- sp-end sp-start)))
    (#t ; general case: TODO implement
      #f)))


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
