;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh lineedit parens (0 1))
  (export
    make-parens parens? parens-name parens-token
    parens-start-x parens-start-x-set! parens-start-y parens-start-y-set!
    parens-end-x   parens-end-x-set!   parens-end-y   parens-end-y-set!
    parens-inner   parens-inner-append!
    parens->values parens->hashtable parens-hashtable-lookup

    is-parens-char?)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) fx1+ fx1- make-format-condition record-writer unread-char void)
    (only (schemesh bootstrap) try until while)
    (only (schemesh containers misc) list-iterate)
    (only (schemesh containers hashtable) hashtable-iterate)
    (schemesh containers span)
    (schemesh containers charspan))


;; parens is an object containing information about the matching parentheses/brackets/braces/quotes
;; in some text to be parsed
(define-record-type
  (parens %make-parens parens?)
  (fields
    name  ; symbol, name of parser that created this parens object (may differ in sub-objects)
    token ; character, one of: ( [ { " ' ` |
    (mutable start-x) ; fixnum, x position of start parenthesis/bracket/brace/quote
    (mutable start-y) ; fixnum, y position of start parenthesis/bracket/brace/quote
    (mutable end-x)   ; fixnum, x position of end parenthesis/bracket/brace/quote
    (mutable end-y)   ; fixnum, y position of end parenthesis/bracket/brace/quote
    (mutable inner))  ; #f or span of nested parens appearing between start and end
  (nongenerative #{parens e1s38b5dr3myvj5mwxrpzkl27-400}))


(define (make-parens name token)
  (assert (symbol? name))
  (when token
    (assert (char? token)))
  (%make-parens name token 0 0 (greatest-fixnum) (greatest-fixnum) #f))


;; append one nested parens to specified parens
(define (parens-inner-append! parens nested-parens)
  (assert (parens? nested-parens))
  (let ((inner (parens-inner parens)))
    (if (span? inner)
      (span-insert-back! inner nested-parens)
      (parens-inner-set! parens (span nested-parens)))))

;; traverse parens and convert it to a hashtable (+ x (* y 65536)) -> parens
(define (parens->hashtable parens)
  (%parens->hashtable parens (make-eqv-hashtable)))

;; traverse parens and convert it to a hashtable (+ x (* y 65536)) -> parens
(define (%parens->hashtable parens htable)
  (let ((inner-span (parens-inner parens)))
    (when inner-span
      (span-iterate inner-span
        (lambda (i inner)
          (%parens->hashtable inner htable))))
    (when (parens-token parens)
      (%hashtable-put-parens htable parens)))
  htable)

;; add parens to hashtable, using both positions start-x start-y and end-x end-y
(define (%hashtable-put-parens htable parens)
  (hashtable-set! htable (xy->key (parens-start-x parens) (parens-start-y parens)) parens)
  (hashtable-set! htable (xy->key (parens-end-x parens)   (parens-end-y parens))   parens)
  htable)

(define (xy->key x y)
  (fxior x (fxarithmetic-shift-left y 16))) ; optimized version of (+ x (* y 65536))

;; function to be used as lookup-func in a parenmatcher
;; returns matching parens or #f
(define (parens-hashtable-lookup htable x y)
  (hashtable-ref htable (xy->key x y) #f))


;; extract fields name token start-x start-y end-x end-y from parens and return them
(define (parens->values parens)
  (values
    (parens-name    parens)
    (parens-token   parens)
    (parens-start-x parens)
    (parens-start-y parens)
    (parens-end-x   parens)
    (parens-end-y   parens)))


(define (show-parens obj port)
  (try
    (let ((token (parens-token obj)))
      (display (if token token #\_) port)
      (let ((inner-span (parens-inner obj)))
        (when (span? inner-span)
          (span-iterate inner-span
            (lambda (i inner)
              (unless (fxzero? i)
                (display #\space port))
              (show-parens inner port)))))
      (display (close-token-for token) port))
    (catch (cond)
      (display cond port))))


(define (close-token-for token)
  (case token
    ((#\() #\)) ((#\[) #\]) ((#\{) #\})
    ((#\") #\") ((#\') #\') ((#\`) #\`)
    ((#\|) #\|) ((#\#) #\#) (else #\_)))

(define (is-parens-char? ch)
  (let ((vec '#(#\( #\) #\[ #\] #\{ #\} #\" #\' #\` #\| #\#)))
    (do ((i 0 (fx1+ i))
         (n (vector-length vec)))
        ((or (fx>=? i n) (eqv? ch (vector-ref vec i))) (fx<? i n)))))


;; customize how "parens" objects are printed
(record-writer (record-type-descriptor parens)
  (lambda (obj port writer)
    (display "#<parens " port)
    (show-parens obj port)
    (display ">" port)))


) ; close library
