;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only from file ../job.ss


;; concatenate strings, wildcard symbols ? * ~ % %!
;; and closures (lambda (job) ...) or (lambda () ...) that return a string or a list of strings,
;; then expand wildcard symbols to matching filesystem paths.
;;
;; returns a non-empty list of strings, containing matching filesystem paths.
;; if args do not match any filesystem path, return a single string - not a list -
;;   containing args converted back to string with shell wildcard syntax.
(define (sh-wildcard job-or-id . args)
  (let* ((job (sh-job job-or-id))
         (sp  (sh-wildcard/apply job args))
         (n   (span-length sp)))
    (cond
      ((fxzero? n)
        "")
      ((span-iterate sp (lambda  (i elem) (string? elem)))
        ; all elements are strings -> concatenate them
        (let ((ret (charspan)))
          (span-iterate sp
            (lambda (i elem)
              (charspan-insert-back/string! ret elem 0 (string-length elem))))
          (charspan->string ret)))
      (#t
        ; actually expand wildcards and match them against filesystem
        (let ((ret (sh-wildcard/expand job
                     (sh-wildcard/prepare
                       (sh-wildcard/expand-tilde! job sp)))))
          (if (null? ret)
            ; convert back wildcard to the string it was generated from,
            ; except that ~ expanded by (sh-wildcard/expand-tilde!) must be preserved.
            ; reason: ~/foo/bar must be expanded to $HOME/foo/bar
            ; even if no such path exists
            (sh-wildcard->literal-string sp)
            ret))))))


(define (sh-wildcard->literal-string sp)
  (debugf "sh-wildcard->literal-string sp=~s" sp)
  (let ((ret (charspan)))
    (%wildcard->charspan-append! sp ret)
    (charspan->string ret)))

;; convert back a single element of a wildcard to the string it was generated from,
;; and append it to specified charspan
(define (%wildcard->charspan-append! sp csp)
  (let ((n (span-length sp)))
    (let %again ((i 0))
      (let ((obj (if (fx<? i n) (span-ref sp i) #f)))
        (case obj
          ((#f)
            (void))
          ((* ?)
            (charspan-insert-back! csp (if (eq? '* obj) #\* #\?))
            (%again (fx1+ i)))
          ((% %!)
            (charspan-insert-back! csp #\[)
            (when (eq? '%! obj)
              (charspan-insert-back! csp #\!))
            (let ((str (span-ref sp (fx1+ i))))
              (charspan-insert-back/string! csp str 0 (string-length str)))
            (charspan-insert-back! csp #\])
            (%again (fx+ i 2)))
          (else
            (charspan-insert-back/string! csp obj 0 (string-length obj))
            (%again (fx1+ i))))))))


;; iterate on args list and call any procedure.
;; each procedure must return a string or a list of strings.
;; return span of strings and wildcard symbols ? * ~ % %!
(define (sh-wildcard/apply job args)
  (let ((sp (span)))
    (list-iterate args
      (lambda (arg)
        (cond
          ((or (string? arg) (sh-wildcard? arg))
            (span-insert-back! sp arg))
          ((procedure? arg)
            (let ((obj (if (logbit? 1 (procedure-arity-mask arg)) (arg job) (arg))))
              (if (string? obj)
                (span-insert-back! sp obj)
                (begin
                  (assert-string-list? 'sh-wildcard obj)
                  (apply span-insert-back! sp obj)))))
          (#t
            (raise-assertv 'sh-wildcard '(or (string? arg) (sh-wildcard? arg) (procedure? arg)) arg)))))
    sp))


;; if span starts with symbol '~ then replace it with specified user's home.
;; replace every other occurrence of symbol '~ with string "~"
;; return sp
(define (sh-wildcard/expand-tilde! job sp)
  (when (eq? '~ (span-ref sp 0))
    (let ((arg1 (if (fx>? (span-length sp) 1) (span-ref sp 1) #f)))
      (if (or (not (string? arg1)) (fxzero? (string-length arg1)) (char=? #\/ (string-ref arg1 0)))
        ;; if environment variable "HOME" is not set, replace symbol '~ with string "~"
        (let ((userhome (sh-env job "HOME" "~")))
          (span-set! sp 0 userhome))
        (let ((userhome (get-userhome (string->utf8b/0 arg1))))
          (cond
            ((string? userhome)
              (span-erase-front! sp 1)   ; erase the initial symbol '~
              (span-set! sp 0 userhome)) ; replace username -> userhome
            (#t
              ;; (get-userhome) failed: replace symbol '~ with string "~"
              (span-set! sp 0 "~")))))))
  (span-iterate sp
    (lambda (i elem)
      (when (eq? '~ elem)
        (span-set! sp i "~"))))
  sp)


;; given a span containing strings and wildcards,
;; split the strings after each delimiter /
;;
;; and group into a sh-pattern the wildcards and strings
;; that refer to a single directory or file name.
;;
;; example:
;;   (span "a/b" '* "c/def/")
;; will be converted to
;;   (span "a/" (sh-pattern "b" '* "c/") "def/"))
(define (sh-wildcard/prepare sp)
  (let ((ret (span))
        (i 0)
        (n (span-length sp)))
    (span-insert-back! ret (span))
    (while (fx<? i n)
      (when (%wildcard/prepare1! (span-ref sp i) ret)
        (set! i (fx1+ i))
        (let ((pattern (span-ref sp i)))
          (if (string-find/char pattern 0 (string-length pattern) #\/)
            (%raise-invalid-wildcard-pattern (span-ref sp (fx1- i)) pattern)
            (span-insert-back! (span-back ret) pattern))))
      (set! i (fx1+ i)))
    (%wildcard/simplify! ret)))


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
(define (%wildcard/prepare1! obj sp)
  (cond
    ((symbol? obj)
      (span-insert-back! (span-back sp) obj)
      (memq obj '(% %!)))
    (#t
      (assert* 'sh-wildcard (string? obj))
      (let ((str-len (string-length obj)))
        (unless (fxzero? str-len)
          (%split-string->paths sp obj str-len))
        #f))))


;; split non-empty string str around each delimiter / which is omitted.
;; and append each fragment to subspans inside sp, starting from subspan.
(define (%split-string->paths sp str str-len)
  (let ((subspan (span-back sp))
        (cspan #f))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i str-len))
      (let* ((ch     (string-ref str i))
             (slash? (char=? ch #\/)))
        ;; ignore duplicated consecutive /
        (unless (and slash? (%last-is-slash? str i sp subspan))
          (unless cspan
            (set! cspan (%ensure-ends-with-charspan! subspan)))
          (charspan-insert-back! cspan ch)
          (when slash?
            ; create a new subspan
            (set! cspan #f)
            (set! subspan (span))
            (span-insert-back! sp subspan)))))))


(define (%last-is-slash? str str-index sp subspan)
  ; (debugf "%last-is-slash? str=~s, str-index=~s, sp=~s subspan=~s" str str-index sp subspan)
  (or
    (and (fx>? str-index 0) (char=? #\/ (string-ref str (fx1- str-index))))
    ; by construction, an empty subspan is preceded by another subspan that ends with /
    ; unless the empty subspan is the only subspan
    (and (span-empty? subspan) (fx>? (span-length sp) 1))))


(define (%ensure-ends-with-charspan! subspan)
  (if (or (span-empty? subspan) (not (charspan? (span-back subspan))))
    (let ((cspan (charspan)))
      (span-insert-back! subspan cspan)
      cspan)
    (span-back subspan)))


;; simplify span in four steps:
;;
;; 1. for each subspan in sp containing only a single charspan,
;;    unwrap the charspan i.s. replace the subspan with the contained charspan.
;; 2. replace each charspan -> string.
;; 3. replace each non-empty subspan with a sh-pattern.
;; 4. if last element is an empty subspan, remove it.
;;
;; return sp, modified in-place
(define (%wildcard/simplify! sp)
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
    (let ((subspan (span-back sp)))
      (when (and (span? subspan) (span-empty? subspan))
        (span-erase-back! sp 1))))
  sp)


;; actually expand sh-patterns in span sp and list matching files on disk.
;; returns a string or a list of strings.
(define (sh-wildcard/expand job sp)
  (try
    (if (span-empty? sp)
      '()
      (let* ((ret  (span))
             (p    (span-ref sp 0))
             (p0   (if (string? p) p (sh-pattern-front/string p)))
             (p0-absolute? (and (string? p0) (char=? #\/ (string-ref p0 0))))
             (dir  (if p0-absolute?
                     "/"
                     (let ((job-dir (job-cwd-if-set job)))
                       (if job-dir
                         (charspan->string job-dir)
                         "")))))
        (span->list (%wildcard/expand sp 0 (span-length sp) dir ret))))
    (catch (ex)
      (debug-condition ex) ;; save ex into thread-parameter (debug-condition)
      '())))


;; recursive implementation of (sh-wildcard/expand)
;; appends matching paths to span ret and returns it.
(define (%wildcard/expand sp i sp-end path ret)
  ;; (debugf "%wildcard/expand patterns=~s, path=~s" (span-range->span* sp i sp-end) path)
  (cond
    ((fx>=? i sp-end) ; check that path exists
      (when (file-stat path 'catch 'symlinks)
        ; if patterns do not end with #\/ then remove any final #\/ from path
        (when (and (string-ends-with/char? path #\/)
                   (not (%patterns-end-with/char? sp #\/)))
          (string-truncate! path (fx1- (string-length path))))
        (span-insert-back! ret path))
      ret)
    ((string? (span-ref sp i))
      (let ((subpath (%path-append path (span-ref sp i))))
        ; check that subpath exists.
        (if (file-stat subpath 'catch)
          (%wildcard/expand sp (fx1+ i) sp-end subpath ret)
          ret)))
    (#t
      (let* ((p       (span-ref sp i))
             (prefix  (or (sh-pattern-front/string p) ""))
             (suffix  (or (sh-pattern-back/string p) ""))
             (path-or-dot (if (fxzero? (string-length path)) "." path))
             (i+1     (fx1+ i)))
        ; pattern p may end with #\/ thus:
        ; 1. must add option 'append-slash to mark directories with a final #\/
        ; 2. cannot add option 'symlinks because it would not mark symlinks with a final #\/ even if they point to a directory
        (list-iterate (directory-sort! (directory-list path-or-dot 'append-slash 'catch 'prefix prefix 'suffix suffix))
          (lambda (type-and-name)
            (let ((name (cdr type-and-name)))
              (when (sh-pattern-match? p name)
                (%wildcard/expand sp i+1 sp-end (%path-append path name) ret))
              (void))))
        ret))))

;; given a span of sh-pattern and strings,
;; return #t if last span element (either a span or a sh-pattern)
;; ends with character ch
(define (%patterns-end-with/char? sp ch)
  (if (span-empty? sp)
    #f
    (let* ((p   (span-back sp))
           (key (if (string? p) p (sh-pattern-back/string p))))
      (and (string? key) (string-ends-with/char? key ch)))))


;; concatenate two filesystem paths
(define (%path-append path1 path2)
  (let* ((path1-len (string-length path1))
         (path2-len (string-length path2))
         (path1-slash? (string-ends-with/char? path1 #\/))
         (path2-slash? (string-starts-with/char? path2 #\/)))
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
      (#t
        (string-append path1 "/" path2)))))
