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
         (sp  (%sh-wildcard-concat-strings (%sh-wildcard-apply job args)))
         (n   (span-length sp)))
    (span-iterate sp
      (lambda (i path)
        (unless (symbol? path)
          (assert* 'sh-wildcard (sh-path? path)))))
    (cond
      ((fxzero? n)
        "")
      ((and (fx=? 1 n) (not (symbol? (span-ref sp 0))))
        ; the only element is a charspan -> just convert it to string
        (charspan->string (span-ref sp 0)))
      (#t
        ; actually expand wildcards
        (%sh-wildcard-expand-paths job sp)))))


;; iterate on args and call any procedure.
;; returns span of strings and wildcard symbols ? * ~ % %!
(define (%sh-wildcard-apply job args)
  (let ((sp (span)))
    (list-iterate args
      (lambda (arg)
        (cond
          ((string? arg)
            (span-insert-back! sp arg))
          ((sh-wildcard? arg)
            (span-insert-back! sp arg))
          ((procedure? arg)
            (let ((value (if (logbit? 1 (procedure-arity-mask arg)) (arg job) (arg))))
              (if (string? value)
                (span-insert-back! sp value)
                (begin
                  (assert-string-list? 'sh-wildcard value)
                  (apply span-insert-back! sp value)))))
          (#t
            (raise-assertv 'sh-wildcard '(or (string? arg) (sh-wildcard? arg) (procedure? arg)) arg)))))
    sp))

;; iterate on span and convert strings into charspans, concatenating consecutive strings.
;; returns in-place modified span of charspans and wildcard symbols ? * ~ % %!
(define (%sh-wildcard-concat-strings sp)
  (let* ((cspan #f)
         (i 0)
         (j 0)
         (n (span-length sp))
         (%append (lambda (obj)
                    (span-set! sp j obj)
                    (set! j (fx1+ j)))))
    ; (debugf ">   %sh-wildcard-concat-strings sp=~s~%" sp)
    (while (fx<? i n)
      (let ((arg (span-ref sp i)))
        (cond
          ((symbol? arg)
            (assert* 'sh-wildcard (sh-wildcard? arg))
            (when cspan
              ;; flush buffered charspan before appending a symbol
              (%append cspan)
              (set! cspan #f))
            (%append arg)
            (set! i (fx1+ i))
            (when (memq arg '(% %!))
              ; wildcard symbols '% and '%! must be followed by a string
              (let* ((arg1 (if (fx<? i n) (span-ref sp i) #f)))
                (assert* 'sh-wildcard (string? arg1))
                (%append (string->charspan arg1))
                (set! i (fx1+ i)))))
          (#t
            ; not a symbol -> must be a string
            (assert* 'sh-wildcard (string? arg))
            (if cspan
              ; append string to buffered charspan
              (charspan-insert-back/string! cspan arg 0 (string-length arg))
              ; convert the string to a buffered charspan
              (set! cspan (string->charspan arg)))
            (set! i (fx1+ i)))))
      ; (debugf "... %sh-wildcard-concat-strings sp=~s cspan=~s~%" sp cspan)
      )
    (when cspan
      (%append cspan))
    (span-resize-back! sp j)
    ; (debugf "<   %sh-wildcard-concat-strings sp=~s~%" sp)
    sp))


;; actually expand wildcards in span sp to list matching files on disk.
;; returns a string or a list of strings.
(define (%sh-wildcard-expand-paths job sp)
  (assert* 'sh-wildcard (fx>? (span-length sp) 0))
  (%sh-wildcard-expand-tilde job sp)
  (let ((sp2 (%sh-wildcard-split-paths sp)))
    sp2))


(define (%sh-wildcard-expand-tilde job sp)
  (when (eq? '~ (span-ref sp 0))
    (let ((arg1 (if (fx>? (span-length sp) 1) (span-ref sp 1) #f)))
      (if (or (not (charspan? arg1)) (charspan-empty? arg1) (char=? #\/ (charspan-ref arg1 0)))
        (span-set! sp 0 (string->charspan (sh-env job "HOME")))
        (let* ((userhome (get-userhome (string->utf8b/0 (charspan->string arg1)))))
          ; erase the symbol '~
          (span-erase-front! sp 1)
          (if (bytevector? userhome)
            (span-set! sp 0 (string->charspan (utf8b->string userhome)))
            ;; keep a literal ~ at the beginning of sp
            (charspan-insert-front! (span-ref sp 0) #\~)))))))

(define get-userhome (foreign-procedure "c_get_userhome" (ptr) ptr))


;; given a span containing charspans and wildcards,
;; split the charspans after each delimiter /
;; and group wildcards and charspans that refer to a single directory or file name.
;;
;; example:
;;   (span (string->charspan "a/b") '* (string->charspan "c/d"))
;; will be converted to
;;   (span (string->charspan "a/")
;;         (span (string->charspan "b") '* (string->charspan "c/"))
;;         (string->charspan "d"))
(define (%sh-wildcard-split-paths sp)
  (let ((ret (span))
        (i 0)
        (n (span-length sp)))
    (span-insert-back! ret (span))
    (while (fx<? i n)
      (case (%sh-wildcard-split-path (span-ref sp i) ret)
        ((/) (span-insert-back! ret (span)))
        ((!)
          (set! i (fx1+ i))
          (span-insert-back! (span-back ret) (span-ref sp i))))
      (set! i (fx1+ i)))
    (%simplify-subspans! ret)))


;; if path is a charspan, split it after each delimiter /
;; and append each fragment into sp. Return '/ if path ends with delimiter /
;; otherwise return #f.
;; if path is a wildcard, append it to sp and return ! if next path is part of the wildcard,
;; otherwise return #f.
(define (%sh-wildcard-split-path path sp)
  (cond
    ((symbol? path)
      (span-insert-back! (span-back sp) path)
      (if (memq path '(% %!))
        '!
        #f))
    (#t
      ;; TODO split path
      (assert* 'sh-wildcard (charspan? path))
      (span-insert-back! (span-back sp) path)
      (if (and (not (charspan-empty? path)) (char=? #\/ (charspan-back path)))
        '/
        #f))))


;; unwrap the element in lenght-1 sub-spans of span sp.
;; return sp
(define (%simplify-subspans! sp)
  (span-iterate sp
    (lambda (i subspan)
      (when (span? subspan)
        (when (fx=? 1 (span-length subspan))
          (span-set! sp i (span-ref subspan 0))))))
  ; if last subspan is empty, remove it
  (when (and (not (span-empty? sp)) (span-empty? (span-back sp)))
    (span-erase-back! sp 1))
  sp)
