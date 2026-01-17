;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;;; JSON pull parser, uses only standard R6RS without external libraries
;;;
(library (scheme2k io json pull (0 9 3))
  (export json-next-token make-json-pull-parser)
  (import (rnrs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(define (whitespace? b)
  (or (= b 32) (= b 9) (= b 10) (= b 13)))

(define (digit? b)
  (and (fixnum? b) (<= 48 b 57))) ; 0 ... 9

(define (hex-digit? b)
  (and (fixnum? b)
       (or (<= 48 b 57)     ; 0 ... 9
           (<= 65 b 70)     ; A ... F
           (<= 97 b 102)))) ; a ... f

(define (hex-value b)
  (cond ((digit? b) (- b 48))
        ((>= b 97) (+ 10 (- b 97)))
        (else (+ 10 (- b 65)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reader helpers

(define (skip-ws p)
  (let ((b (lookahead-u8 p)))
    (when (and (not (eof-object? b)) (whitespace? b))
      (get-u8 p)
      (skip-ws p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String parsing

(define (parse-string p)
  ;; opening quote already consumed
  (let %parse-string ((p p) (chars '()))
    (let ((b (get-u8 p)))
      (cond
        ((eof-object? b)
         (error 'json "unexpected EOF in json string"))
        ((= b 34)
         (list->string (reverse chars)))
        ((= b 92) ;; escape
         (let ((e (get-u8 p)))
           (cond
             ((not (fixnum? e))
               (error 'json "unexpected EOF in json string"))
             ((= e 34) (%parse-string p (cons #\" chars)))
             ((= e 92) (%parse-string p (cons #\\ chars)))
             ((= e 47) (%parse-string p (cons #\/ chars)))
             ((= e 98) (%parse-string p (cons #\backspace chars)))
             ((= e 102) (%parse-string p (cons #\page chars)))
             ((= e 110) (%parse-string p (cons #\newline chars)))
             ((= e 114) (%parse-string p (cons #\return chars)))
             ((= e 116) (%parse-string p (cons #\tab chars)))
             ((= e 117) ;; \uXXXX
              ;; FIXME: handle surrogate pairs \uXXXX\uYYYY
              (let %loop ((i 0) (v 0))
                (if (= i 4)
                    (%parse-string p (cons (integer->char v) chars))
                    (let ((h (get-u8 p)))
                      (unless (hex-digit? h)
                        (error 'json "invalid \\u escape"))
                      (%loop (+ 1 i)
                             (+ (* v 16) (hex-value h)))))))
             (else (error 'json "invalid escape")))))
        (else
          ;; FIXME: combine UTF-8 bytes into chars
          (%parse-string p (cons (integer->char b) chars)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Number parsing

(define (parse-number p first)
  (let %loop ((chars (list (integer->char first))))
    (let ((b (lookahead-u8 p)))
      ;; FIXME validate json number syntax
      (if (and (not (eof-object? b))
               (or (digit? b)
                   (= b 46) (= b 101) (= b 69)
                   (= b 43) (= b 45)))
          (begin
            (get-u8 p)
            (%loop (cons (integer->char b) chars)))
          (string->number
             (list->string (reverse chars)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Literal parsing

(define (expect-bytes p bytes)
  (for-each
   (lambda (b)
     (unless (= (get-u8 p) b)
       (error 'json "invalid literal")))
   bytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main pull tokenizer

(define (json-next-token p)
  (skip-ws p)
  (let ((b (get-u8 p)))
    (cond
      ((eof-object? b) 'eof)
      ((= b 123) 'lbrace)
      ((= b 125) 'rbrace)
      ((= b 91)  'lbracket)
      ((= b 93)  'rbracket)
      ((= b 58)  'colon)
      ((= b 44)  'comma)
      ((= b 34)  (parse-string p))
      ((or (digit? b) (= b 45))
        (parse-number p b))
      ((= b 116) (expect-bytes p '(114 117 101)) #t)    ; true
      ((= b 102) (expect-bytes p '(97 108 115 101)) #f) ; false
      ((= b 110) (expect-bytes p '(117 108 108)) '())   ; nil
      (else
        (error 'json "unexpected byte")))))


(define (make-json-pull-parser p)
  (let ((state-stack '(top))
        (finished? #f))

    (define (push s)
      (set! state-stack (cons s state-stack)))

    (define (pop)
      (set! state-stack (cdr state-stack)))

    (define (state)
      (car state-stack))

    (define (atomic-value-token? t)
      (or (boolean? t) (null? t) (number? t) (string? t)))

    (define (value-start-token? t)
      (or (atomic-value-token? t)
          (memq t '(lbrace lbracket))))

    (define (accept-value-start t)
      (case t
        ((lbrace)
         (push 'object-expect-key))
        ((lbracket)
         (push 'array-expect-value))
        (else
         ;; atomic value: nothing to push
         #f)))

    (define (next-token)
      (when finished?
        (error 'json "token requested after eof"))

      (let ((t (json-next-token p)))
        (let ((kind (car t)))
          (case (state)

            ;; =====================================================
            ;; Top level
            ((top)
             (cond
               ((equal? t 'eof)
                (error 'json "empty input"))
               ((value-start-token? t)
                (pop)
                (accept-value-start t)
                (push 'done)
                t)
               (else
                (error 'json "invalid top-level token" t))))

            ;; =====================================================
            ;; Done (only EOF allowed)
            ((done)
             (if (equal? t 'eof)
                 (begin
                   (set! finished? #t)
                   t)
                 (error 'json "trailing data after JSON value" t)))

            ;; =====================================================
            ;; Array
            ((array-expect-value)
             (cond
               ((equal? t 'rbracket)
                (pop)
                t)
               ((value-start-token? t)
                (pop)
                (accept-value-start t)
                (push 'array-after-value)
                t)
               (else
                (error 'json "expected value or ']'" t))))

            ((array-after-value)
             (cond
               ((equal? t 'comma)
                (pop)
                (push 'array-expect-value)
                t)
               ((equal? t 'rbracket)
                (pop)
                t)
               (else
                (error 'json "expected ',' or ']'" t))))

            ;; =====================================================
            ;; Object
            ((object-expect-key)
             (cond
               ((equal? t 'rbrace)
                (pop)
                t)
               ((eq? (car t) 'string)
                (pop)
                (push 'object-expect-colon)
                t)
               (else
                (error 'json "expected string key or '}'" t))))

            ((object-expect-colon)
             (if (equal? t 'colon)
                 (begin
                   (pop)
                   (push 'object-expect-value)
                   t)
                 (error 'json "expected ':'" t)))

            ((object-expect-value)
             (if (value-start-token? t)
                 (begin
                   (pop)
                   (accept-value-start t)
                   (push 'object-after-value)
                   t)
                 (error 'json "expected value" t)))

            ((object-after-value)
             (cond
               ((equal? t 'comma)
                (pop)
                (push 'object-expect-key)
                t)
               ((equal? t 'rbrace)
                (pop)
                t)
               (else
                (error 'json "expected ',' or '}'" t))))

            (else
             (error 'json "invalid parser state" state))))))

    next-token))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example usage for (json-next-token)
;;
;; (define p (open-bytevector-input-port
;;            (string->utf8 "{\"a\": [1, true]}")))
;; (let %loop ()
;;   (let ((t (json-next-token p)))
;;     (display t) (newline)
;;     (unless (equal? t 'eof) (%loop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example usage for (make-json-parser)
;;
;; (define parser
;;   (make-json-parser
;;     (open-bytevector-input-port
;;      (string->utf8 "{\"a\": [1, true]}"))))
;;
;; (let %loop ()
;;   (let ((t (parser)))
;;     (display t) (newline)
;;     (unless (equal? t 'eof)
;;       (%loop))))


) ; close library
