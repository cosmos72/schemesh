;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file shell/job.ss


;; expand a path containing wildcards to the list of filesystem entries that match such wildcards.
;;
;; each w must be a string, a wildcard symbol ? * ~ % %!
;; or a closure (lambda (job) ...) or (lambda () ...) that returns a string or a list of strings.
;;
;; if first w is the symbol ~ expand it to specified user's home directory,
;; then call each closure and replace it with the returned string or list of strings,
;; finally expand wildcard symbols to matching filesystem paths.
;;
;; returns a non-empty list of strings, containing matching filesystem paths.
;; if w does not match any filesystem path, return a list containing a single string:
;;   w converted back to string with shell wildcard syntax.
(define (sh-wildcard job-or-id . w)
  (sh-wildcard* job-or-id w '(if-no-match? string-list)))


;; TL;DR similar to (sh-wildcard), with two differences:
;;  1. w must be passed as a list
;;  2. if w does not match any filesystem path, returned value depends on options:
;;     empty list, or w converted to string, or list containing w converted to string
;;
;; Full description:
;;
;; expand a path containing wildcards to the list of filesystem entries that match such wildcards.
;;
;; each element of list w must be a string, a wildcard symbol ? * ~ % %!
;; or a closure (lambda (job) ...) or (lambda () ...) that returns a string or a list of strings.
;;
;; if w starts with symbol ~ expand it to specified user's home directory,
;; then call each closure and replace it with the returned string or list of strings,
;; finally expand wildcard symbols to matching filesystem paths.
;;
;; returns a list of strings, containing matching filesystem paths.
;; if does not match any filesystem path, returned value depends on options - see above.
(define sh-wildcard*
  (case-lambda
    ((w)
      (sh-wildcard* #f w '()))
    ((job-or-id w)
      (sh-wildcard* job-or-id w '()))
    ((job-or-id w options)
      (let* ((job  (sh-job job-or-id))
             (w (sh-wildcard/apply job (sh-wildcard/expand-tilde job w))))
        (cond
          ((null? w)
            (%wildcard-wrap-string "" options))
          ((every string? w)
            ; all elements are strings -> concatenate them
            (let ((str (sh-wildcard->string w)))
              (if (file-type str '(catch symlinks))
                (list str) ; path exists, return a list containing only it
                (%wildcard-wrap-string str options)))) ; path does not exist
          (else
            ; actually expand wildcards and match them against filesystem paths
            (let* ((patterns (sh-wildcard->sh-patterns w))
                   (ret (sh-patterns/expand job patterns)))
              (if (pair? ret)
                ret
                (%wildcard-wrap-lazy w options)))))))))


(define (%wildcard-wrap-string str options)
  (case (plist-ref options 'if-no-match?)
    ((string)      str)
    ((string-list) (list str))
    (else          '())))


(define (%wildcard-wrap-lazy w options)
  (case (plist-ref options 'if-no-match?)
    ;; wildcard does not match any filesystem path
    ;; convert back wildcard to the string it was generated from,
    ;; except that ~ expanded by (sh-wildcard/expand-tilde) must be preserved.
    ;; reason: ~/foo/bar must be expanded to $HOME/foo/bar
    ;; even if no such path exists
    ((string)      (sh-wildcard->string w))
    ((string-list) (list (sh-wildcard->string w)))
    (else          '())))


(define (sh-wildcard->string w)
  ; (debugf "sh-wildcard->string w=~s" w)
  (let ((csp (charspan)))
    (%wildcard->charspan-append! w csp)
    (charspan->string*! csp)))


;; convert back wildcard w to the string it was generated from,
;; and append it to specified charspan.
;; return (void)
(define (%wildcard->charspan-append! w csp)
  (until (null? w)
    (let ((obj (car w)))
      (case obj
        ((* ?)
          (charspan-insert-right! csp (if (eq? '* obj) #\* #\?)))
        ((% %!)
          (charspan-insert-right! csp #\[)
          (when (eq? '%! obj)
            (charspan-insert-right! csp #\!))
          (charspan-insert-right/string! csp (cadr w))
          (charspan-insert-right! csp #\])
          (set! w (cdr w)))
        (else
          (charspan-insert-right/string! csp obj))))
    (set! w (cdr w))))


;; iterate on w - a list - and call any procedure.
;; each procedure must return a string or a list of strings.
;; return the new list of strings and wildcard symbols ? * ~ % %!
;; which may share data with w
(define (sh-wildcard/apply job w)
  (if (list-contains-procedure? w)
    (let* ((ret '())
           (%insert!
             (lambda (arg)
               (set! ret (cons arg ret)))))
      (for-list ((arg w))
        (cond
          ((or (string? arg) (sh-wildcard? arg))
            (%insert! arg))
          ((procedure? arg)
            (let ((obj (if (logbit? 1 (procedure-arity-mask arg))
                             (arg job) ; call (proc job)
                             (arg))))  ; call (proc)
              (if (string? obj)
                (%insert! obj)
                (begin
                  (assert-string-list? 'sh-wildcard obj)
                  (for-each %insert! obj)))))
          (else
            (raise-assert1 'sh-wildcard "(or (string? arg) (sh-wildcard? arg) (procedure? arg))" arg))))
      (reverse! ret))
    w))


;; if list w starts with symbol '~ then replace it with specified user's home.
;; replace every other occurrence of symbol '~ with string "~"
;; return list containing replacement result, which may share data with w.
;; does NOT modify w.
(define (sh-wildcard/expand-tilde job w)
  (if (list-contains-tilde? w)
    (let ((w (if (eq? '~ (car w))
               (expand-initial-tilde job w)
               w)))
      (if (list-contains-tilde? w)
        (list-replaceq w '~ "~")
        w))
    w))


(define (list-contains-procedure? l)
  (any procedure? l))

(define (list-contains-tilde? l)
  (and (memq '~ l) #t)) ; (memq) returns a list, not a boolean

(define (list-replaceq l old new)
  (map (lambda (elem) (if (eq? elem old) new elem)) l))


;; given a list w starting with '~ and possibly also containing strings and wildcards,
;; replace initial symbol '~ with specified user's home.
;;
;; return modified list, which may share data with w.
;; does NOT modify w.
(define (expand-initial-tilde job w)
  (let* ((tail (cdr w))
         (arg1 (if (null? tail) #f (car tail))))
    ; (debugf "expand-initial-tilde arg1=~s" arg1)
    (if (or (not (string? arg1)) (fxzero? (string-length arg1)) (char=? #\/ (string-ref arg1 0)))
      ;; expand ~ to environment variable "HOME", or to string "~" if such env. variable is not set
      (let ((userhome (sh-env-ref job "HOME" "~")))
        (cons userhome tail))
      (let* ((slash    (string-index arg1 #\/))
             (username (if slash (substring arg1 0 slash) arg1))
             (userhome (username->homedir (string->utf8b/0 username))))
        (if (string? userhome)
          (if slash
            ; remove the initial '~ and the portion of arg1 before the slash
            (cons userhome (cons (substring arg1 slash (string-length arg1)) w))
            ; remove the initial '~ and the whole arg1
            (cons userhome (cdr tail)))
          ;; (username->homedir) failed: replace symbol '~ with string "~", keep arg1
          (cons "~" tail))))))


;; given a list w containing strings and wildcards,
;; split the strings after each delimiter /
;;
;; and group into a sh-pattern the wildcards and strings
;; that refer to a single directory or file name.
;;
;; return a span of sh-pattern and strings.
;;
;; example:
;;   (list "a/b" '* "c/def/")
;; will be converted to
;;   (span "a/" (sh-pattern "b" '* "c/") "def/"))
(define (sh-wildcard->sh-patterns w)
  (let ((ret (span)))
    (span-insert-right! ret (span))
    (until (null? w)
      (when (%patterns/prepare1! (car w) ret)
        (let ((tail (cdr w)))
          (when (null? tail)
            (raise-errorf 'sh-wildcard "missing string after shell wildcard symbol '~s" (car w)))
          (let ((pattern (car tail)))
            (unless (string? pattern)
              (raise-errorf 'sh-wildcard "found ~s after shell wildcard symbol '~s, expected a string" pattern (car w)))
            (when (string-index pattern #\/)
              (%raise-invalid-wildcard-pattern (car w) pattern))
            (span-insert-right! (span-ref-right ret) pattern))
            (set! w tail)))
      (set! w (cdr w)))
    (%patterns/simplify! ret)))


(define (%raise-invalid-wildcard-pattern sym pattern)
  (raise-errorf 'sh-wildcard "invalid shell wildcard pattern ~s, must not contain /"
    (string-append
      (if (eq? sym '%) "[" "[!")
      pattern
      "]")))


;; if obj is a string, convert it to charspan, split it after each delimiter /
;; and append each fragment into sp. Return #f.
;; if obj is a wildcard, append it to sp and return truish if next obj is part of the wildcard,
;; otherwise return #f.
(define (%patterns/prepare1! obj sp)
  (cond
    ((symbol? obj)
      (span-insert-right! (span-ref-right sp) obj)
      (memq obj '(% %!)))
    (else
      (assert* 'sh-wildcard (string? obj))
      (let ((str-len (string-length obj)))
        (unless (fxzero? str-len)
          (%split-string->paths sp obj str-len))
        #f))))


;; split non-empty string str around each delimiter / which is omitted.
;; and append each fragment to subspans inside sp, starting from subspan.
(define (%split-string->paths sp str str-len)
  (let ((subspan (span-ref-right sp))
        (cspan #f))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i str-len))
      (let* ((ch     (string-ref str i))
             (slash? (char=? ch #\/)))
        ;; ignore duplicated consecutive /
        (unless (and slash? (%last-is-slash? str i sp subspan))
          (unless cspan
            (set! cspan (%ensure-ends-with-charspan! subspan)))
          (charspan-insert-right! cspan ch)
          (when slash?
            ; create a new subspan
            (set! cspan #f)
            (set! subspan (span))
            (span-insert-right! sp subspan)))))))


(define (%last-is-slash? str str-index sp subspan)
  ; (debugf "%last-is-slash? str=~s, str-index=~s, sp=~s subspan=~s" str str-index sp subspan)
  (or
    (and (fx>? str-index 0) (char=? #\/ (string-ref str (fx1- str-index))))
    ; by construction, an empty subspan is preceded by another subspan that ends with /
    ; unless the empty subspan is the only subspan
    (and (span-empty? subspan) (fx>? (span-length sp) 1))))


(define (%ensure-ends-with-charspan! subspan)
  (if (or (span-empty? subspan) (not (charspan? (span-ref-right subspan))))
    (let ((cspan (charspan)))
      (span-insert-right! subspan cspan)
      cspan)
    (span-ref-right subspan)))


;; simplify span in four steps:
;;
;; 1. for each subspan in sp containing only a single charspan,
;;    unwrap the charspan i.s. replace the subspan with the contained charspan.
;; 2. replace each charspan -> string.
;; 3. replace each non-empty subspan with a sh-pattern.
;; 4. if last element is an empty subspan, remove it.
;;
;; return sp, modified in-place
(define (%patterns/simplify! sp)
  (span-iterate sp
    (lambda (i subspan)
      (when (span? subspan)
        (case (span-length subspan)
          ((0)
            (void))
          ((1)
            (let ((elem (span-ref subspan 0)))
              (span-set! sp i (if (charspan? elem)
                                (charspan->string elem)
                                (span->sh-pattern* subspan)))))
          (else
            (span-iterate subspan
              (lambda (j elem)
                (when (charspan? elem)
                  (span-set! subspan j (charspan->string elem)))))
            (span-set! sp i (span->sh-pattern* subspan)))))))
  ;; if last element is an empty subspan, remove it.
  (unless (span-empty? sp)
    (let ((subspan (span-ref-right sp)))
      (when (and (span? subspan) (span-empty? subspan))
        (span-erase-right! sp 1))))
  sp)


;; actually expand sh-patterns in span sp and list matching filesystem entries.
;; return (possibly empty) list of strings,
;; where names in each directory are sorted lexicographically.
(define (sh-patterns/expand job sp)
  (try
    (if (span-empty? sp)
      '()
      (let* ((ret  (span))
             (p    (span-ref sp 0))
             (p0   (if (string? p) p (sh-pattern-ref/string p)))
             (p0-absolute? (and (string? p0) (char=? #\/ (string-ref p0 0))))
             (dir  (if p0-absolute?
                     "/"
                     (let ((job-dir (job-cwd-if-set job)))
                       (if job-dir
                         (charspan->string job-dir)
                         "")))))
        (span->list (%patterns/expand sp 0 (span-length sp) dir ret))))
    (catch (ex)
      (sh-exception-handler ex)
      '())))


;; recursive implementation of (sh-patterns/expand)
;; appends matching paths to span ret and returns it.
(define (%patterns/expand sp i sp-end path ret)
  ; (debugf "%patterns/expand patterns=~s, path=~s" (span-range->span* sp i sp-end) path)
  (cond
    ((fx>=? i sp-end) ; check that path exists
      (when (file-type path '(catch symlinks))
        ; if patterns do not end with #\/ then remove any final #\/ from path
        (when (and (string-suffix/char? path #\/)
                   (not (%patterns-end-with/char? sp #\/)))
          (string-truncate! path (fx1- (string-length path))))
        (span-insert-right! ret path))
      ret)
    ((string? (span-ref sp i))
      (let ((subpath (%path-append path (span-ref sp i))))
        ; check that subpath exists.
        (if (file-type subpath '(catch))
          (%patterns/expand sp (fx1+ i) sp-end subpath ret)
          ret)))
    (else
      (let* ((p       (span-ref sp i))
             (prefix  (or (sh-pattern-ref/string p) ""))
             (suffix  (or (sh-pattern-ref-right/string p) ""))
             (path-or-dot (if (fxzero? (string-length path)) "." path))
             (i+1     (fx1+ i)))
        ; pattern p may end with #\/ thus:
        ; 1. must add option 'append-slash to mark directories with a final #\/
        ; 2. cannot add option 'symlinks because it would not mark symlinks with a final #\/ even if they point to a directory
        (for-list ((name (directory-sort!
                           (directory-list path-or-dot
                                           (list 'append-slash 'catch 'prefix prefix 'suffix suffix)))))
          (when (sh-pattern-match? p name)
            (%patterns/expand sp i+1 sp-end (%path-append path name) ret)))
        ret))))

;; given a span of sh-pattern and strings,
;; return #t if last span element (either a span or a sh-pattern)
;; ends with character ch
(define (%patterns-end-with/char? sp ch)
  (if (span-empty? sp)
    #f
    (let* ((p   (span-ref-right sp))
           (key (if (string? p) p (sh-pattern-ref-right/string p))))
      (and (string? key) (string-suffix/char? key ch)))))


;; concatenate two filesystem paths
(define (%path-append path1 path2)
  (let* ((path1-len (string-length path1))
         (path2-len (string-length path2))
         (path1-slash? (string-suffix/char? path1 #\/))
         (path2-slash? (string-prefix/char? path2 #\/)))
    (cond
      ((fxzero? path1-len)
        path2)
      ((and path1-slash? path2-slash?)
        (let* ((path1-len-1 (fx1- path1-len))
               (ret         (make-string (fx+ path1-len-1 path2-len))))
          (string-copy! path1 0 ret 0 path1-len-1)
          (string-copy! path2 0 ret path1-len-1 path2-len)
          ret))
      ((or path1-slash? path2-slash?)
        (string-append path1 path2))
      (else
        (string-append path1 "/" path2)))))
