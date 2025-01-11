;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should only be included inside a (library ...) definition


;; concatenate strings, wildcard symbols ? * ~ % %!
;; and closures (lambda (job) ...) or (lambda () ...) that return a string or a list of strings,
;; then expand wildcard symbols to matching filesystem paths.
;;
;; returns a string or list of strings
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
        ; actually expand wildcards
        (sh-wildcard/expand-paths job
          (sh-wildcard/simplify-paths!
            (sh-wildcard/prepare-paths
              (sh-wildcard/expand-tilde! job sp))))))))


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


;; if span starts with symbol ~ then replace it with specified user's home.
;; return sp
(define (sh-wildcard/expand-tilde! job sp)
  (when (eq? '~ (span-ref sp 0))
    (let ((arg1 (if (fx>? (span-length sp) 1) (span-ref sp 1) #f)))
      (if (or (not (string? arg1)) (fxzero? (string-length arg1)) (char=? #\/ (string-ref arg1 0)))
        (span-set! sp 0 (sh-env job "HOME"))
        (let* ((userhome (get-userhome (string->utf8b/0 arg1))))
          (cond
            ((string? userhome)
              (span-erase-front! sp 1)   ; erase the initial symbol '~
              (span-set! sp 0 userhome)) ; replace username -> userhome
            (#t
              ;; (get-userhome) failed: replace symbol '~ with string "~"
              (span-set! sp 0 "~")))))))
  sp)


(define get-userhome (foreign-procedure "c_get_userhome" (ptr) ptr))


;; given a span containing strings and wildcards,
;; convert the strings to charspans, split them after each delimiter /
;; and group wildcards and charspans that refer to a single directory or file name.
;;
;; example:
;;   (span "a/b" '* "c/d")
;; will be converted to
;;   (span (string->charspan "a/")
;;         (span (string->charspan "b") '* (string->charspan "c/"))
;;         (string->charspan "d"))
(define (sh-wildcard/prepare-paths sp)
  (let ((ret (span))
        (i 0)
        (n (span-length sp)))
    (span-insert-back! ret (span))
    (while (fx<? i n)
      (when (sh-wildcard/prepare-path! (span-ref sp i) ret)
        (set! i (fx1+ i))
        (span-insert-back! (span-back ret) (string->charspan (span-ref sp i))))
      (set! i (fx1+ i)))
    ret))


;; if obj is a string, convert it to charspan, split it after each delimiter /
;; and append each fragment into sp. Return #f.
;; if obj is a wildcard, append it to sp and return truish if next obj is part of the wildcard,
;; otherwise return #f.
(define (sh-wildcard/prepare-path! obj sp)
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


;; split non-empty string str after each delimiter /
;; and append each fragment to subspans inside sp, starting from subspan.
(define (%split-string->paths sp str str-len)
  (let ((subspan (span-back sp))
        (cspan #f))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i str-len))
      (let ((ch (string-ref str i)))
        ;; ignore duplicated consecutive /
        (unless (and (char=? #\/ ch) (%last-is-slash? str i sp subspan))
          (unless cspan
            (set! cspan (%ensure-ends-with-charspan! subspan)))
          (charspan-insert-back! cspan ch)
          (when (char=? ch #\/)
            ; create a new subspan
            (set! cspan #f)
            (set! subspan (span))
            (span-insert-back! sp subspan)))))))


(define (%last-is-slash? str str-index sp subspan)
  ; (debugf "%last-is-slash? str=~s, str-index=~s, sp=~s subspan=~s~%" str str-index sp subspan)
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


;; unwrap single-element sub-spans in span sp.
;; if last element is an empty subspan, remove it.
;; return sp
(define (sh-wildcard/simplify-paths! sp)
  (span-iterate sp
    (lambda (i subspan)
      (when (and (span? subspan) (fx=? 1 (span-length subspan)))
        (span-set! sp i (span-ref subspan 0)))))
  ;; if last element is an empty subspan, remove it.
  (unless (span-empty? sp)
    (let ((subspan (span-back sp)))
      (when (and (span? subspan) (span-empty? subspan))
        (span-erase-back! sp 1))))
  sp)


;; actually expand wildcards in span sp to list matching files on disk.
;; returns a string or a list of strings.
(define (sh-wildcard/expand-paths job sp)
  ;; TODO: implement
  sp)
