;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh lineedit paren (0 9 1))
  (export
    make-paren      make-paren/bad-close
    paren?          paren-name
    paren-start-token paren-end-token paren-end-token-set!
    paren-start-x   paren-start-y    paren-start-xy-set!
    paren-end-x     paren-end-y      paren-end-xy-set!
    paren-ok?       paren-recursive-ok?  paren-valid?
    paren-inner     paren-inner-empty?
    paren-inner-ref paren-inner-ref* paren-inner-append!
    paren->list     paren->hashtable paren-hashtable-ref
    paren-find/surrounds is-paren-char? debugf-paren)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) console-error-port format fx1+ fx1- record-writer void)
    (only (schemesh bootstrap) assert* debugf catch try until while)
    (schemesh containers span)
    (schemesh containers charspan))


;; paren is an object containing information about the matching parentheses/brackets/braces/quotes
;; in some text to be parsed
(define-record-type (paren %make-paren paren?)
  (fields
    name  ; symbol, name of parser that created this paren object (may differ in sub-objects)
    start-token         ; #f if missing, or #t if not visible, or character - one of: # ( [ { " ' ` |
    (mutable start-x)   ; fixnum, x position of start parenthesis/bracket/brace/quote
    (mutable start-y)   ; fixnum, y position of start parenthesis/bracket/brace/quote
    (mutable end-token) ; #f if missing, or #t if not visible, or character - one of:   ) ] } " ' ` |
    (mutable end-x)     ; fixnum, x position of end parenthesis/bracket/brace/quote
    (mutable end-y)     ; fixnum, y position of end parenthesis/bracket/brace/quote
    (mutable inner))    ; #f or span of nested paren appearing between start and end
  (nongenerative %paren-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define (make-paren name start-token)
  (assert* 'make-paren (symbol? name))
  (unless (boolean? start-token)
    (assert* 'make-paren (char? start-token)))
  (%make-paren name start-token 0 0 #f (greatest-fixnum) (greatest-fixnum) #f))


(define (make-paren/bad-close name end-token x y)
  (assert* 'make-paren (symbol? name))
  (assert* 'make-paren (char? end-token))
  (assert* 'make-paren (fixnum? x))
  (assert* 'make-paren (fixnum? y))
  (%make-paren name #f x y end-token x y #f))


;; return truish if paren start-token and end-token are both present,
;; otherwise return #f
(define (paren-ok? paren)
  (and (paren? paren) (paren-start-token paren) (paren-end-token paren)))


;; return #t if (paren-ok?) is truish,
;; and both start and end positions of paren are valid
(define (paren-valid? paren)
  (and (paren-ok? paren)
    (let ((start-xy (xy->key (paren-start-x paren) (paren-start-y paren)))
          (end-xy   (xy->key (paren-end-x paren)   (paren-end-y paren))))
      (fx<? -1 start-xy end-xy))))


;; return #t if (paren-ok?) is truish in paren and in all its inner paren.
(define (paren-recursive-ok? paren)
  (and (paren-ok? paren)
       (let* ((inner (paren-inner paren))
              (n (if (span? inner) (span-length inner) 0)))
         (do ((i 0 (fx1+ i)))
             ((or (fx>=? i n) (not (paren-recursive-ok? (span-ref inner i))))
              (fx>=? i n))))))


(define (paren-inner-empty? paren)
  (let ((inner (paren-inner paren)))
    (if (span? inner)
      (span-empty? inner)
      #t)))


;; return the i-th inner paren, or #f if out of range
(define (paren-inner-ref paren i)
  (let* ((inner (paren-inner paren))
         (n     (if (span? inner) (span-length inner) 0)))
    (if (fx<? -1 i n)
      (span-ref inner i)
      #f)))

(define (paren-inner-ref* paren . indexes)
  (let %recurse ((paren paren) (indexes indexes))
    (if (and paren (not (null? indexes)))
      (%recurse (paren-inner-ref paren (car indexes)) (cdr indexes))
      paren)))


;; append one nested paren to specified paren
(define (paren-inner-append! paren nested-paren)
  (when (and paren nested-paren)
    (assert* 'paren-inner-append! (paren? paren))
    (assert* 'paren-inner-append! (paren? nested-paren))
    (let ((inner (paren-inner paren)))
      (if (span? inner)
        (span-insert-right! inner nested-paren)
        (paren-inner-set! paren (span nested-paren))))))

;; traverse paren and convert it to a hashtable (+ x (* y 65536)) -> paren
(define (paren->hashtable paren)
  ; (debugf "(paren->hashtable ~s)" paren)
  (if paren
    (%paren->hashtable paren (make-eqv-hashtable))
    #f))

;; actual implementation of (paren->hashtable)
(define (%paren->hashtable paren htable)
  ; (debugf "(%paren->hashtable ~s)" paren)
  (let ((inner-span (paren-inner paren)))
    (when inner-span
      (span-iterate inner-span
        (lambda (i inner)
          (%paren->hashtable inner htable)))))
  ; if multiple paren start or end at the same position,
  ; the outer one wins.
  ; if they are at the same depth, the later one wins.
  ;
  ; reason: parser directive #!... creates a nested paren
  ; with the same end position of the outer one,
  ; but the nested paren starts at the end of the parser directive #!...
  ; while the outer paren usually starts at a parenthesis,
  ; and we want to highlight the latter when cursor is at the end position
  ; of both paren.
  (when (char? (paren-start-token paren))
    (%hashtable-put-paren-start htable paren))
  (when (char? (paren-end-token paren))
    (%hashtable-put-paren-end htable paren))
  htable)

;; add paren to hashtable for position start-x start-y
(define (%hashtable-put-paren-start htable paren)
  (let ((xy (xy->key (paren-start-x paren) (paren-start-y paren))))
    (when (fx>=? xy 0)
      (hashtable-set! htable xy paren))))

;; add paren to hashtable for position end-x end-y
(define (%hashtable-put-paren-end htable paren)
  (let ((xy (xy->key (paren-end-x paren) (paren-end-y paren))))
    (when (fx>=? xy 0)
      (hashtable-set! htable xy paren))))

(define (paren-start-xy-set! paren x y)
  (paren-start-x-set! paren x)
  (paren-start-y-set! paren y))

(define (paren-end-xy-set! paren x y)
  (paren-end-x-set! paren x)
  (paren-end-y-set! paren y))

(define (xy->key* x y xy-if-not-ok)
  (if (and (fx<=? 0 x 65535) (fx<=? 0 y 65535))
    (fxior x (fxarithmetic-shift-left y 16)) ; optimized version of (+ x (* y 65536))
    xy-if-not-ok))

(define (xy->key x y)
  (xy->key* x y -1))

;; search in hashtable generated by (paren->hashtable) and return paren matching x y, or #f
(define (paren-hashtable-ref htable x y)
  (hashtable-ref htable (xy->key x y) #f))


;; extract fields name token start-x start-y end-x end-y from paren and return them as a list
(define (paren->list paren)
  (list
    (paren-name        paren)
    (paren-start-token paren)
    (paren-start-x     paren)
    (paren-start-y     paren)
    (paren-end-token   paren)
    (paren-end-x       paren)
    (paren-end-y       paren)))


;; find the innermost paren that surrounds position x y and return it.
;; return #f if paren does not surround position x y
(define (paren-find/surrounds paren x y)
  (%paren-find/surrounds paren (xy->key x y) -1 (greatest-fixnum)))


;; actual implementation of (paren-find/surrounds)
(define (%paren-find/surrounds paren xy outer-start-xy outer-end-xy)
  (let-values (((start-xy end-xy surrounds?)
                  (%paren-surrounds? paren xy outer-start-xy outer-end-xy)))
    ; (debugf "  > paren-find/surrounds paren=~s, xy=~s, start-xy=~s, end-xy=~s, surrounds=~s, outer-start-xy=~s, outer-end-xy=~s"
    ;         paren xy start-xy end-xy surrounds? outer-start-xy outer-end-xy)
    (and surrounds?
         (%paren-find-inner/surrounds paren xy start-xy end-xy))))


;; if paren surrounds position x y, return (values start-xy end-xy #t)
;; otherwise return (values start-xy end-xy #f)
(define (%paren-surrounds? paren xy outer-start-xy outer-end-xy)
  (let ((start-xy (xy->key* (paren-start-x paren) (paren-start-y paren) outer-start-xy))
        (end-xy   (xy->key* (paren-end-x paren)   (paren-end-y paren)   outer-end-xy)))
    (values start-xy end-xy (fx<=? (if (eq? #t (paren-start-token paren)) start-xy (fx1+ start-xy))
                                   xy
                                   (if (or (char? (paren-end-token paren))
                                           (fx=? end-xy (greatest-fixnum)))
                                     end-xy
                                     (fx1+ end-xy))))))


;; among the inner parens of paren, find the innermost one that surrounds position x y and return it.
;; return paren if no inner paren surrounds position x y
(define (%paren-find-inner/surrounds paren xy outer-start-xy outer-end-xy)
  (let ((inner-span (paren-inner paren))
        (ret #f))
    (when (span? inner-span)
      (span-iterate inner-span
        (lambda (i inner)
          (set! ret (%paren-find/surrounds inner xy outer-start-xy outer-end-xy))
          (not ret))))
    (or ret paren)))


(define (display-paren obj port)
  (try
    (let ((start-token (paren-start-token obj))
          (end-token   (paren-end-token obj))
          (inner-span  (paren-inner obj)))
      (display-paren-token (or start-token (open-token-for end-token)) start-token port)
      (when (span? inner-span)
        (span-iterate inner-span
          (lambda (i inner)
            (unless (fxzero? i)
              (display #\space port))
            (display-paren inner port))))
      (display-paren-token (or end-token (close-token-for start-token)) end-token port))
    (catch (ex)
      (display ex port))))


(define (display-paren-token token ok? port)
  ; port can be #t, cannot use (put-string) or (put-char)
  (let ((token (if (char? token) token #\_)))
    (if ok?
      (display token port)
      (begin
         ;; show a missing token in dark gray
         (display "\x1B;[30;1m" port)
         (display token port)
         (display "\x1B;[m" port)))))


(define (debugf-paren obj)
  (try
    (begin (debugf "debugf-paren tokens ~s ~s, start-xy ~s ~s, end-xy ~s ~s, inner ~s\n"
      (paren-start-token obj) (paren-end-token obj) (paren-start-x obj) (paren-start-y obj)
      (paren-end-x obj) (paren-end-y obj) (paren-inner obj)))
    (catch (ex)
      (display ex (console-error-port)))))

(define (close-token-for token)
  (case token
    ((#\() #\)) ((#\[) #\]) ((#\{) #\})
    ((#\") #\") ((#\') #\') ((#\`) #\`)
    ((#\|) #\|) ((#\#) #\#) (else #\_)))

(define (open-token-for token)
  (case token
    ((#\)) #\() ((#\]) #\[) ((#\}) #\{)
    ((#\") #\") ((#\') #\') ((#\`) #\`)
    ((#\|) #\|) ((#\#) #\#) (else #\_)))

(define (is-paren-char? ch)
  (let ((vec '#(#\( #\) #\[ #\] #\{ #\} #\" #\' #\` #\| #\#)))
    (do ((i 0 (fx1+ i))
         (n (vector-length vec)))
        ((or (fx>=? i n) (eqv? ch (vector-ref vec i))) (fx<? i n)))))


;; customize how "paren" objects are printed
(record-writer (record-type-descriptor paren)
  (lambda (obj port writer)
    (display "#<paren " port)
    (display-paren obj port)
    (display ">" port)))


) ; close library
