;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k posix pattern (0 9 3))
  (export
    sh-pattern sh-pattern? span->sh-pattern* sh-pattern->span*
    sh-pattern-ref/string sh-pattern-ref-right/string
    sh-pattern-match? wildcard?)
  (import
    (rnrs)
    (only (chezscheme) fx1+ fx1- record-writer void)
    (only (scheme2k bootstrap) assert* fx<=?* raise-assertf)
    (only (scheme2k containers string) string-index string-index-right substring=?)
    (scheme2k containers charspan)
    (scheme2k containers span))


;; return #t if obj is a wildcard symbol, i.e. one of * ? ~ % %!
;; otherwise return #f
(define (wildcard? obj)
  (and (symbol? obj) (memq obj '(* ? ~ % %!)) #t))


(define-record-type (pattern %make-pattern sh-pattern?)
  (fields
     span       ; span of strings and symbols
     min-len    ; length of shortest string that can be matched
     max-len    ; length of longest string that can be matched, or #f if unlimited
     fixed?)    ; #t if sp contains only strings
  (nongenerative pattern-7c46d04b-34f4-4046-b5c7-b63753c1be39))


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
    (when (and fixed? (fx>? n 1))
      (raise-assertf 'sh-pattern "adjacent strings are not allowed, consider merging them: ~s" sp))
    (let-values (((min-len max-len) (%pattern-minmax-length sp 0 n 0 0)))
      (%make-pattern sp min-len max-len fixed?))))


;; view a sh-pattern as a span.
;;
;; Do NOT modify the returned span.
(define (sh-pattern->span* p)
  (pattern-span p))


;; validate a span to be used inside a sh-pattern.
;; raises condition if span contents is not valid for sh-pattern.
;;
;; returns #t if span contains only strings, otherwise #f
(define (%validate-span sp i n contains-wildcard? prev-is-string?)
  (if (fx>=? i n)
    (not contains-wildcard?)
    (let ((obj (span-ref sp i))
          (i+1 (fx1+ i)))
      (cond
        ((string? obj)
          (let ((str-len (string-length obj)))
            (when prev-is-string?
              (raise-assertf 'sh-pattern "adjacent strings are not allowed, consider merging them: ~s ~s"
                obj (span-ref sp (fx1- i))))
            (when (fxzero? str-len)
              (raise-assertf 'sh-pattern "empty strings are not allowed: ~s" obj))
            (%validate-span sp (fx1+ i) n contains-wildcard? #t))) ; prev-is-string?
        ((wildcard? obj)
          (when (memq obj '(% %!))
            (when (fx=? i+1 n)
              (raise-assertf 'sh-pattern "missing string after wildcard symbol '~a" obj))
            (set! i i+1)
            (let ((obj2 (span-ref sp i)))
              (unless (string? obj2)
                (raise-assertf 'sh-pattern "expecting a string after wildcard symbol '~a, found ~s" obj obj2))))
          (%validate-span sp (fx1+ i) n #t ; contains-wildcard?
                             #f)) ; prev-is-string?
        (else
          (raise-assertf 'sh-pattern "expecting a string or wildcard? symbol, found ~s" obj))))))


;; analyze range [i, end) of span sp and return two values;
;;   1. the length of shortest string that can be matched
;;   2. the length of longest string that can be matched, or #f if unlimited
(define (%pattern-minmax-length sp i end min-len max-len)
  (if (fx>=? i end)
    (values min-len max-len)
    (let ((obj (span-ref sp i))
          (i+1 (fx1+ i)))
      (cond
        ((string? obj)
          (let ((str-len (string-length obj)))
            (%pattern-minmax-length sp (fx1+ i) end
              (fx+ min-len str-len)                    ; updated min-len
              (if max-len (fx+ max-len str-len) #f)))) ; updated max-len
        ((wildcard? obj)
          (%pattern-minmax-length sp
              (if (memq obj '(% %!)) (fx1+ i+1) i+1) ; updated i
              end
              (if (eq? obj '*) min-len (fx1+ min-len))                  ; updated min-len
              (if (and max-len (not (eq? obj '*))) (fx1+ max-len) #f))) ; updated max-len
        (else
          (raise-assertf 'sh-pattern "expecting a string or wildcard? symbol, found ~s" obj))))))


;; if first element in sh-pattern is a string, return it.
;; otherwise return #f
(define (sh-pattern-ref/string p)
  (let* ((sp     (pattern-span p))
         (key    (if (span-empty? sp) #f (span-ref sp 0))))
    (if (string? key) key #f)))


;; if last element in sh-pattern is a string, return it.
;; otherwise return #f
(define (sh-pattern-ref-right/string p)
  (let* ((sp     (pattern-span p))
         (sp-len (span-length sp))
         (key    (if (fxzero? sp-len) #f (%pattern-at sp (fx1- sp-len)))))
    (if (string? key) key #f)))


;; Determine whether sh-pattern p matches specified string.
;; Returns #t or #f.
;;
;; Notes:
;; 1. if sh-pattern p contains one or more wildcard symbols,
;;    it intentionally never matches the strings "." or ".."
;;
;; 2. if sh-pattern p starts with a wildcard symbol,
;;    it intentionally never matches strings starting with "."
;;
(define sh-pattern-match?
  (case-lambda
    ((p str str-start str-end)
      ; (debugf "sh-pattern-match p=~s str=~s" p str)
      (assert* 'sh-pattern-match? (sh-pattern? p))
      (assert* 'sh-pattern-match? (string? str))
      (let* ((sp  (pattern-span p))
             (n   (span-length sp))
             (len (fx- str-end str-start)))
        (assert* 'sh-pattern-match? (fx<=?* 0 str-start str-end (string-length str)))
        ;; handle special cases first
        (cond
          ((span-empty? sp)
            ; an empty pattern can only match the empty string
            (fxzero? len))
          ((or (fx<? len (pattern-min-len p))
               (and (pattern-max-len p)
                    (fx>? len (pattern-max-len p))))
            ; name is shorter than minimum length, or longer than maximum length - cannot be matched
            #f)
          ((fxzero? len)
            ; an empty name can only be matched by an empty pattern (already checked above)
            ; or by a sequence of '* with nothing else
            (%pattern-all-keys-are? sp 0 n '*))
          ((pattern-fixed? p)
            ; a non-empty pattern without wilcards only matches the string (span-ref sp 0)
            (let ((key (span-ref sp 0)))
              (and (fx=? len (string-length key))
                   (substring=? key 0 str str-start len))))
          ((and (fx<=? len 2) (substring=? str str-start ".." 0 len))
            ; the special directory names "." and ".." cannot be matched
            ; by any pattern containing wildcards
            #f)
          ((and (char=? #\. (string-ref str str-start)) (symbol? (span-ref sp 0)))
            ; names starting with #\. cannot be matched
            ; by a pattern starting with a wildcard
            #f)
          (else
            (if (%pattern-match/left sp 0 n 0 str str-start str-end)
              #t
              #f)))))
    ((p str)
      (assert* 'sh-pattern-match? (string? str))
      (sh-pattern-match? p str 0 (string-length str)))))


;; recursively determine whether the range [sp-start, sp-end) of the span sp,
;; which may also contain wildcards, matches range [str-start, str-end) of string str.
;;
;; returns (fx+ ret (fx- sp-end sp-start)) or #f.
(define (%pattern-match/left sp sp-start sp-end ret str str-start str-end)
  (if (fx>=? sp-start sp-end)
    ; consumed all keys. did we also consume the whole string?
    (if (fx>=? str-start str-end)
      ret
      #f)
    (let-values (((sp-step str-step) (%pattern-match/left1 sp sp-start sp-end str str-start str-end)))
      ; (debugf "%pattern-match/left sp-step=~s str-step=~s key=~s" sp-step str-step (span-ref sp sp-start))
      (if sp-step
        ; recurse
        (%pattern-match/left sp (fx+ sp-start sp-step) sp-end (fx+ ret sp-step)
                             str (fx+ str-start str-step) str-end)
        #f))))


;; recursively determine whether the range [sp-start, sp-end) of the span sp,
;; which may also contain wildcards, matches range [str-start, str-end) of string str.
;;
;; returns (fx+ ret (fx- sp-end sp-start)) or #f.
(define (%pattern-match/right sp sp-start sp-end ret str str-start str-end)
  (if (fx>=? sp-start sp-end)
    ; consumed all keys. did we also consume the whole string?
    (if (fx>=? str-start str-end)
      ret
      #f)
    (let-values (((sp-step str-step) (%pattern-match/right1 sp sp-start sp-end str str-start str-end)))
      ; (debugf "%pattern-match/right sp-step=~s str-step=~s key=~s" sp-step str-step (%pattern-at sp (fx1- sp-end)))
      (if sp-step
        ; recurse
        (%pattern-match/right sp sp-start (fx- sp-end sp-step) (fx+ ret sp-step)
                              str str-start (fx- str-end str-step))
        #f))))


;; determine how many characters of string str range [str-start, str-end)
;; starting from str-start and moving forward
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
          (%pattern-match/left/string key str str-start str-end)
          (string-length key)))
      ((eq? key '?)
        (values
          (%pattern-match/question str str-start str-end)
          1))
      ((eq? key '*)
        (values
          (%pattern-match/left/star sp sp-start sp-end str str-start str-end)
          (fx- str-end str-start)))
      ((memq key '(% %!))
        (values
          (and (fx<? str-start str-end)
               (fx<? (fx1+ sp-start) sp-end)
               (%pattern-match/alternative key (span-ref sp (fx1+ sp-start)) (string-ref str str-start)))
          1))
      (else ; unexpected
        (raise-assertf 'sh-pattern-match? "pattern contains unsupported element: ~s" key)))))


;; determine how many characters of string str range [str-start, str-end)
;; starting from (fx1- str-end) and moving backward
;; are matched by pattern key (span-ref sp (fx1- sp-end).
;;
;; return two values:
;; 1. number of consumed span elements, or #f if match failed.
;; 2. number of matched string characters
(define (%pattern-match/right1 sp sp-start sp-end str str-start str-end)
  (let ((key (%pattern-at sp (fx1- sp-end))))
    ; (debugf "->   %pattern-match/right1 sp=~s sp-start=~s sp-end=~s key=~s" sp sp-start sp-end key)
    ; (debugf "... %pattern-match/right1 str=~s str-start=~s str-end=~s" str str-start str-end)
    (cond
      ((string? key)
        (values
          (%pattern-match/right/string key str str-start str-end)
          (string-length key)))
      ((eq? key '?)
        (values
          (%pattern-match/question str str-start str-end)
          1))
      ((eq? key '*)
        (values
          (%pattern-match/right/star sp sp-start sp-end str str-start str-end)
          (fx- str-end str-start)))
      ((memq key '(% %!))
        (values
          (and (fx<? str-start str-end)
               (fx<? sp-start (fx1- sp-end))
               (%pattern-match/alternative key (span-ref sp (fx1- sp-end)) (string-ref str (fx1- str-end))))
          1))
      (else ; unexpected
        (raise-assertf 'sh-pattern-match? "pattern contains unsupported element: ~s" key)))))

;; return key at index i of span sp.
;; note: if element at index i is a string and is preceded by '% or '%!
;; we must return such preceding element
(define (%pattern-at sp i)
  (let ((key (span-ref sp i)))
    (if (or (fx<=? i 0) (not (string? key)))
      key
      (let ((key2 (span-ref sp (fx1- i))))
        (if (memq key2 '(% %!))
          key2
          key)))))


;; match ? i.e. any single character.
;;
;; if match is successful returns 1,
;; otherwise returns #f
(define (%pattern-match/question str str-start str-end)
  (if (fx<? str-start str-end) 1 #f))

;; match [ALT] or [!ALT] i.e. alternative characters against character ch.
;;
;; [ALT] is represented as '% "ALT" and matches a single character among ALT.
;; [!ALT] is represented as '%! "ALT" and matches a single character not among ALT.
;;
;; ALT alternative characters may also contain ranges as "a-z"
;;
;; if match is successful returns 2,
;; otherwise returns #f
(define (%pattern-match/alternative key alt ch)
  (let ((alt-len (string-length alt)))
    (if (fxzero? alt-len)
      #f ; missing alternatives
      (let ((match?
              (if (string-index alt #\- 1 (fxmax 1 (fx1- alt-len)))
                ; found #\- not at the beginning and not at the end:
                ; search among alternatives listed as range(s)
                (%pattern-match/range? alt ch)
                ; plain string search among alternatives, returns fixnum or #f
                (string-index alt ch 0 alt-len))))
        ; if key is '%! then negate the meaning of match?
        (if (if (eq? key '%) match? (not match?))
          2
          #f)))))


;; match [ALT] i.e. alternative characters listed in string alt,
;; which also contains ranges as "a-z", against character ch.
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


;; Determine whether range [str-start, str-end) of string str
;; starts with string key.
;;
;; if match is successful returns 1,
;; otherwise returns #f
(define (%pattern-match/left/string key str str-start str-end)
  (let* ((key-len   (string-length key))
         (str-len   (fx- str-end str-start))
         (compare-n (fxmin key-len str-len)))
    (if (and (fx=? key-len compare-n)
             (substring=? key 0 str str-start compare-n))
      1
      #f)))


;; Determine whether range [str-start, str-end) of string str
;; ends with string key.
;;
;; if match is successful returns 1,
;; otherwise returns #f
(define (%pattern-match/right/string key str str-start str-end)
  (let* ((key-len   (string-length key))
         (str-len   (fx- str-end str-start))
         (compare-n (fxmin key-len str-len)))
    (if (and (fx=? key-len compare-n)
             (substring=? key 0 str (fx- str-end compare-n) compare-n))
      1
      #f)))


;; recursively determine whether the range [sp-start, sp-end] of the span sp,
;; which starts with '* matches range [str-start, str-end) of string str.
;;
;; if match is successful returns (fx- sp-end sp-start)
;; otherwise returns #f
(define (%pattern-match/left/star sp sp-start sp-end str str-start str-end)
  (cond
    ((%pattern-all-keys-are? sp sp-start sp-end '*)
      ; a sequence of one or more '* with nothing else
      ; matches anything and consumes all '*
      (fx- sp-end sp-start))
    ((not (eq? '* (span-ref sp (fx1- sp-end))))
      ; match from right
      (%pattern-match/right sp sp-start sp-end 0 str str-start str-end))
    (else
      (let* ((sp-pos/not-star (span-index sp (fx1+ sp-start) sp-end (lambda (key) (not (eq? key '*)))))
             (sp-pos/string?  (and sp-pos/not-star (string? (span-ref sp sp-pos/not-star)))))
        (assert* 'sh-pattern-match? sp-pos/not-star)
        (let-values (((min-len max-len) (%pattern-minmax-length sp sp-pos/not-star sp-end 0 0)))
          (assert* 'sh-pattern-match? (fx>? min-len 0))
          (cond
            ((not (fx<=? min-len (fx- str-end str-start) (or max-len (greatest-fixnum))))
              ; name is too short or long, cannot be matched
              #f)
            (sp-pos/string?
              ; sequence of '* followed by a string: do an optimized recursion
              (%pattern-match/left/stars-then-string sp sp-start sp-end sp-pos/not-star
                                                     str str-start str-end min-len max-len))
            (else
              ; this is the hard case: no obvious optimizations.
              ; we must try skipping one character, then two... up to (fx- search-end str-start),
              ; and match recursively
              (%pattern-match/left/stars sp sp-start sp-end sp-pos/not-star
                                         str str-start str-end min-len max-len))))))))


;; iterate and recursively match a pattern starting with one or more '* followed by a string key
(define (%pattern-match/left/stars-then-string sp sp-start sp-end sp-pos/not-star
                                               str str-start str-end min-len max-len)
  (let ((ch (string-ref (span-ref sp sp-pos/not-star) 0))
        (search-start (if max-len (fx- str-end max-len) str-start))
        (search-end   (fx1+ (fx- str-end min-len))))
    (let %again/opt ((str-pos search-start))
      (let ((str-ch-pos (string-index str ch str-pos search-end )))
        (if str-ch-pos
          ; ch found: try to recursively match the remaining pattern after sequence of '*
          (or
            (%pattern-match/left sp sp-pos/not-star sp-end (fx- sp-pos/not-star sp-start) str str-ch-pos str-end) ; recursive match
            (%again/opt (fx1+ str-pos))) ; called if recursive match failed: try again immediately after ch
          #f))))) ; ch not found: match failed


;; iterate and recursively match a pattern starting with one or more '* NOT followed by a string key
(define (%pattern-match/left/stars sp sp-start sp-end sp-pos/not-star
                                   str str-start str-end min-len max-len)
  (let ((search-start (if max-len (fx- str-end max-len) str-start))
        (search-end   (fx1+ (fx- str-end min-len))))
    (let %again ((str-pos search-start))
      (if (fx>=? str-pos search-end)
        #f ; all recursive match attempts failed
        (or
          (%pattern-match/left sp sp-pos/not-star sp-end (fx- sp-pos/not-star sp-start) str str-pos str-end) ; recursive match
          (%again (fx1+ str-pos))))))) ; called if recursive match failed: try again skipping one more character



;; recursively determine whether the range [sp-start, sp-end] of the span sp,
;; which ends with '* matches range [str-start, str-end) of string str.
;;
;; if match is successful returns (fx- sp-end sp-start)
;; otherwise returns #f
(define (%pattern-match/right/star sp sp-start sp-end str str-start str-end)
  (cond
    ((%pattern-all-keys-are? sp sp-start sp-end '*)
      ; a sequence of one or more '* with nothing else
      ; matches anything and consumes all '*
      (fx- sp-end sp-start))
    ((not (eq? '* (span-ref sp sp-start)))
      ; match from left
      (%pattern-match/left sp sp-start sp-end 0 str str-start str-end))
    (else
      (let* ((sp-pos/not-star (span-index-right sp sp-start (fx1- sp-end) (lambda (key) (not (eq? key '*)))))
             (sp-pos/string?  (and sp-pos/not-star
                                   (string? (%pattern-at sp sp-pos/not-star)))))
        (assert* 'sh-pattern-match sp-pos/not-star)
        (let-values (((min-len max-len) (%pattern-minmax-length sp sp-pos/not-star sp-end 0 0)))
          (assert* 'sh-pattern-match (fx>? min-len 1))
          (cond
            ((not (fx<=? min-len (fx- str-end str-start) (or max-len (greatest-fixnum))))
              ; name is too short or long, cannot be matched
              #f)
            (sp-pos/string?
              ; sequence of '* followed by a string: do an optimized recursion
              (%pattern-match/right/stars-then-string sp sp-start sp-end sp-pos/not-star
                                                      str str-start str-end min-len max-len))
            (else
              ; this is the hard case: no obvious optimizations.
              ; we must try skipping one character, then two... up to (fx- search-end str-start),
              ; and match recursively
              (%pattern-match/right/stars sp sp-start sp-end sp-pos/not-star
                                          str str-start str-end min-len max-len))))))))


;; iterate and recursively match a pattern ending with one or more '* preceded by a string key
(define (%pattern-match/right/stars-then-string sp sp-start sp-end sp-pos/not-star
                                                str str-start str-end min-len max-len)
  (let* ((key (span-ref sp sp-pos/not-star))
         (ch  (string-ref key (fx1- (string-length key))))
         (search-start (fx1- (fx+ str-start min-len)))
         (search-end   (if max-len (fx+ str-start max-len) str-end)))
    (let %again/opt ((str-pos search-start))
      (let ((str-ch-pos (string-index str ch str-pos search-end)))
        (if str-ch-pos
          ; ch found: try to recursively match the remaining pattern after sequence of '*
          (or
            (%pattern-match/right sp sp-start (fx1+ sp-pos/not-star)
                                  (fx- sp-end (fx1+ sp-pos/not-star))
                                  str str-start (fx1+ str-ch-pos)) ; recursive match
            (%again/opt (fx1+ str-pos))) ; called if recursive match failed: try again immediately after ch
          #f))))) ; ch not found: match failed


;; iterate and recursively match a pattern ending with one or more '* NOT preceded by a string key
(define (%pattern-match/right/stars sp sp-start sp-end sp-pos/not-star
                                    str str-start str-end min-len max-len)
  (let ((search-start (fx1- (fx+ str-start min-len)))
        (search-end   (if max-len (fx+ str-start max-len) str-end)))
    (let %again ((str-pos search-start))
      (if (fx>=? str-pos search-end)
        #f ; all recursive match attempts failed
        (or
          (%pattern-match/right sp sp-start (fx1+ sp-pos/not-star)
                                (fx- sp-end (fx1+ sp-pos/not-star))
                                str str-start (fx1+ str-pos)) ; recursive match
          (%again (fx1+ str-pos))))))) ; called if recursive match failed: try again skipping one more character


;; return #t if all keys in the range [sp-start, sp-end] of the span sp are eq? key.
;; otherwise return #f
(define (%pattern-all-keys-are? sp sp-start sp-end key)
  (do ((i sp-start (fx1+ i)))
      ((or (fx>=? i sp-end) (not (eq? key (span-ref sp i))))
        (fx>=? i sp-end))))







;;  customize how "sh-pattern" objects are printed
(record-writer (record-type-descriptor pattern)
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
