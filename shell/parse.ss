;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh shell parse (0 1))
  (export sh sh-parse)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) eval remq! reverse!)
    (only (schemesh bootstrap)       until)
    (only (schemesh containers misc) list-iterate list-quoteq!)
    (only (schemesh containers hashtable) eq-hashtable)
    (schemesh shell jobs))

; Return #t if token is a shell command separator: ; & && || | |&
(define (sh-separator? token)
  (and (symbol? token)
       (memq token '(& && \x3b; \x7c;\x7c; \x7c; \x7c;&
                     ))))

; Return #t if token is a shell redirection operator: < <> <& > >> >| >&
; TODO: recognize optional fd number [N] before redirection operator
(define (sh-redirect-operator? token)
  (and (symbol? token)
       (memq token '(< <> <& > >> >& >\x7c;
                     ))))

; Parse args using shell syntax, and return corresponding sh-cmd or sh-multijob object.
;
; Each element in args must be a symbol, string, closure or pair:
; 1. symbols are operators. Recognized symbols are: ; & && || | |& < <> <& > >> >| >&
;    TODO: implement fd number [N] before redirection operator
; 2. strings stand for themselves. for example (sh "ls" "-l")
;    is equivalent to (sh-cmd "ls" "-l")
; 3. integers are fd numbers, and must be followed by a redirection operator < <> <& > >> >| >&
; 4. closures must accept a single argument and return a string.
;    TODO: implement support for them.
; 5. pairs TBD
(define (sh . args)
  ; implementation: use sh-parse for converting shell commands to Scheme forms,
  ; then (eval) such forms
  (eval (sh-parse args)))


;; Parse list containing a sequence of shell commands separated by ; & && || | |&
;; Return list containing parsed args.
(define (sh-parse args)
  (let ((saved-args args)
        (ret '()))
    (until (null? args)
      (let-values (((parsed tail) (sh-parse-or args)))
        (set! ret (cons parsed ret))
        (set! args tail)
        ; (format #t "sh-parse          iterate: ret = ~s, args = ~s~%" (reverse ret) args)
        (cond
          ((null? args) #f)
          ((not (symbol? (car args)))
            (set! ret (cons '\x3b; ret)))
          ((memq (car args) '(& \x3b;
                              ))
            (set! ret (cons (car args) ret))
            (set! args (cdr args)))
          (#t
            (syntax-violation 'sh-parse "syntax error, unknown shell operator:"
              saved-args (car args))))))
    ; (format #t "sh-parse           return: ret = ~s, args = ~s~%" (reverse ret) args)
    (cond
      ((null? ret) '(sh-true))
      ((null? (cdr ret)) (car ret))
      (#t (cons 'sh-list (reverse! (list-quoteq! '(& \x3b;
                                                  ) ret)))))))


;; Parse list containing a sequence of shell commands separated by || && | |&
;; Return two values:
;;   A list containing parsed args;
;;   The remaining, unparsed args.
;;
(define (sh-parse-or args)
  (let ((ret '())
        (done? (null? args)))
    (until done?
      (let-values (((parsed tail) (sh-parse-and args)))
        (set! ret (cons parsed ret))
        (set! args tail))
      ; (format #t "sh-parse-or iterate: ret = ~s, args = ~s~%" (reverse ret) args)
      (cond
        ((null? args) (set! done? #t))
        ((eqv? (car args) '\x7c;\x7c;)
          (set! args  (cdr args))
          (set! done? (null? args)))
        (#t   (set! done? #t)))) ; unhandled token => exit loop
    ; (format #t "sh-parse-or  return: ret = ~s, args = ~s~%" (reverse ret) args)
    (values
      (cond
        ((null? ret)       ret)
        ((null? (cdr ret)) (car ret))
        (#t                (cons 'sh-or (reverse! ret))))
      args)))


;; Parse list containing a sequence of shell commands separated by && | |&
;; Return two values:
;;   A list containing parsed args;
;;   The remaining, unparsed args.
;;
(define (sh-parse-and args)
  (let ((ret '())
        (done? (null? args)))
    (until done?
      (let-values (((parsed tail) (sh-parse-pipe args)))
        (set! ret (cons parsed ret))
        (set! args tail))
      ; (format #t "sh-parse-and iterate: ret = ~s, args = ~s~%" (reverse ret) args)
      (cond
        ((null? args) (set! done? #t))
        ((eqv? (car args) '&&)
          (set! args  (cdr args))
          (set! done? (null? args)))
        (#t   (set! done? #t)))) ; unhandled token => exit loop
    ; (format #t "sh-parse-and  return: ret = ~s, args = ~s~%" (reverse ret) args)
    (values
      (cond
        ((null? ret)       ret)
        ((null? (cdr ret)) (car ret))
        (#t                (cons 'sh-and (reverse! ret))))
      args)))

;
; Parse list containing a sequence of shell commands separated by | |&
; Return two values:
;   A list containing parsed args;
;   The remaining, unparsed args.
;/
(define (sh-parse-pipe args)
  (let ((ret '())
        (done? (null? args)))
    (until done?
      (let-values (((parsed tail) (sh-parse-cmd args)))
        (set! ret (cons parsed ret))
        (set! args tail))
      ; (format #t "sh-parse-pipe  iterate: ret = ~s, args = ~s~%" (reverse ret) args)
      (cond
        ((null? args) (set! done? #t))
        ((memq (car args) '(\x7c; \x7c;&
                            ))
          (set! ret (cons (car args) ret))
          (set! args (cdr args))
          (set! done? (null? args)))
        (#t   (set! done? #t)))) ; unhandled token => exit loop
    ; (format #t "sh-parse-pipe   return: ret = ~s, args = ~s~%" (reverse ret) args)
    (values
      (cond
        ((null? ret) ret)
        ((null? (cdr ret)) (car ret))
        (#t (cons 'sh-pipe* (reverse! (list-quoteq! '(\x7c; \x7c;&
                                                      ) ret)))))
      args)))

; Parse args for a single shell command, i.e. everything before the first ; & && || | |&
; TODO: recognize numbers followed by redirection operators N< N<> N<& N> N>> N>| N>&
; Return two values:
;   A list containing parsed args;
;   The remaining, unparsed args.
;
(define (sh-parse-cmd args)
  (let ((saved-args args)
        (ret '())
        (redirections? #f)
        (done? (null? args)))
    (until done?
      ; (format #t "sh-parse-cmd iterate: ret = ~s, args = ~s~%" (reverse ret) args)
      (if (null? args)
        (set! done? #t)
        (let ((arg (car args)))
          (cond
            ((sh-separator? arg)
              (set! done? #t)) ; separator => exit loop
            ((or (sh-redirect-operator? arg) (pair? arg) (string? arg) (procedure? arg))
              (when (sh-redirect-operator? arg)
                (set! redirections? #t)
                ; quote redirection operator (a symbol) to use its name, not its value
                (set! arg (list 'quote arg)))
              (set! ret (cons arg ret))
              (set! args (cdr args)))
            (#t
              (syntax-violation 'sh-parse
                "syntax error, expecting a redirection operator, string, pair or procedure, found:"
                saved-args arg))))))
    ; (format #t "sh-parse-cmd  return: ret = ~s, args = ~s~%" (reverse ret) args)
    (values
      (cons (if redirections? 'sh-cmd<> 'sh-cmd) (reverse! ret))
      args)))

) ; close library
