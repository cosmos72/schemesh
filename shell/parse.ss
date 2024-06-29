;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
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
    (only (chezscheme) eval expand remq! reverse!)
    (only (schemesh bootstrap)       debugf until)
    (only (schemesh containers misc) list-iterate list-quoteq! string-contains-only-decimal-digits?)
    (only (schemesh containers hashtable) eq-hashtable)
    (schemesh shell jobs))

;; Return #t if token is a shell command separator: ; & && || |
(define (sh-separator? token)
  (and (symbol? token)
       (memq token '(& && \x3b; \x7c;\x7c; \x7c; \x7c;&
                     ))))

;; Return #t if token is a shell redirection operator: < <> <& > >> >&
;; TODO: recognize optional fd number [N] before redirection operator
(define (sh-redirect-operator? token)
  (and (symbol? token)
       (memq token '(< <> <& > >> >&))))

;; Parse args using shell syntax, and return corresponding sh-cmd or sh-multijob object.
;; Current implementation is (eval (sh-parse args)), which uses (sh-parse)
;; for converting shell commands to Scheme source forms, then (eval) such forms.
;;
;; See (sh-parse) for allowed args.
(define (sh . args)
  ; implementation: use (sh-parse) for converting shell commands to Scheme forms,
  ; then (eval) such forms
  (eval (sh-parse args)))


;; Parse list containing a sequence of shell commands separated by ; & && || |
;; Return parsed list, which typically consists of Scheme source forms
;; that will create sh-cmd or sh-multijob objects if evaluated.
;;
;; Each element in args must be a symbol, string, integer or pair:
;; 1. symbols are operators. Recognized symbols are: ; & ! && || | < <> > >> <& >&
;; 2. strings stand for themselves. for example (sh-parse '("ls" "-l"))
;;    returns the Scheme source form '(sh-cmd "ls" "-l")
;; 3. integers are fd numbers, and must be followed by a redirection operator < <> <& > >> >&
;; 4. pairs are not parsed: they are copied verbatim into returned list.
(define (sh-parse args)
  (when (pair? args)
    (sh-validate args))
  (let ((saved-args args)
        (ret '()))
    (until (null? args)
      (let-values (((parsed tail) (sh-parse-or args)))
        (set! ret (cons parsed ret))
        (set! args tail)
        ; (debugf "sh-parse          iterate: ret = ~s, args = ~s~%" (reverse ret) args)
        (cond
          ((null? args) #f)
          ((not (symbol? (car args)))
            (set! ret (cons '\x3b; ret)))
          ((memq (car args) '(& \x3b;
                              ))
            (set! ret (cons (car args) ret))
            (set! args (cdr args)))
          (#t
            (syntax-violation 'sh-parse "syntax error, unknown shell DSL operator:"
              saved-args (car args))))))
    ; (debugf "sh-parse           return: ret = ~s, args = ~s~%" (reverse ret) args)
    (cond
      ((null? ret) '(sh-cmd "true"))
      ((null? (cdr ret)) (car ret))
      (#t (cons 'sh-list (reverse! (list-quoteq! '(& \x3b;
                                                  ) ret)))))))

;; validate list containing a sequence of shell commands separated by ; & ! && || |
(define (sh-validate args)
  (until (null? args)
    (unless (null? (cdr args))
      (let ((arg1 (car args))
            (arg2 (cadr args)))
        (when (and (symbol? arg1) (symbol? arg2) (not (eq? '! arg2)))
          (unless (and (eq? arg1 '\x3b;)
                       (memq arg2 '(& \x3b;
                                    )))
            (syntax-violation 'sh-parse "syntax error, invalid consecutive shell DSL operators:"
              args arg2)))))
    (set! args (cdr args))))


;; Parse list containing a sequence of shell commands separated by || && |
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
      ; (debugf "sh-parse-or iterate: ret = ~s, args = ~s~%" (reverse ret) args)
      (cond
        ((null? args) (set! done? #t))
        ((eqv? (car args) '\x7c;\x7c;)
          (set! args  (cdr args))
          (set! done? (null? args)))
        (#t   (set! done? #t)))) ; unhandled token => exit loop
    ; (debugf "sh-parse-or  return: ret = ~s, args = ~s~%" (reverse ret) args)
    (values
      (cond
        ((null? ret)       ret)
        ((null? (cdr ret)) (car ret))
        (#t                (cons 'sh-or (reverse! ret))))
      args)))


;; Parse list containing a sequence of shell commands separated by && |
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
      ; (debugf "sh-parse-and iterate: ret = ~s, args = ~s~%" (reverse ret) args)
      (cond
        ((null? args) (set! done? #t))
        ((eqv? (car args) '&&)
          (set! args  (cdr args))
          (set! done? (null? args)))
        (#t   (set! done? #t)))) ; unhandled token => exit loop
    ; (debugf "sh-parse-and  return: ret = ~s, args = ~s~%" (reverse ret) args)
    (values
      (cond
        ((null? ret)       ret)
        ((null? (cdr ret)) (car ret))
        (#t                (cons 'sh-and (reverse! ret))))
      args)))

;; Parse list containing a sequence of shell commands separated by |
;; Return two values:
;;   A list containing parsed args;
;;   The remaining, unparsed args.
;;
(define (sh-parse-pipe args)
  (let ((ret '())
        (done? (null? args)))
    (until done?
      (let-values (((parsed tail) (sh-parse-not args)))
        (set! ret (cons parsed ret))
        (set! args tail))
      ; (debugf "sh-parse-pipe  iterate: ret = ~s, args = ~s~%" (reverse ret) args)
      (cond
        ((null? args) (set! done? #t))
        ((memq (car args) '(\x7c; \x7c;&
                            ))
          (set! ret (cons (car args) ret))
          (set! args (cdr args))
          (set! done? (null? args)))
        (#t   (set! done? #t)))) ; unhandled token => exit loop
    ; (debugf "sh-parse-pipe   return: ret = ~s, args = ~s~%" (reverse ret) args)
    (values
      (cond
        ((null? ret) ret)
        ((null? (cdr ret)) (car ret))
        (#t (cons 'sh-pipe* (reverse! (list-quoteq! '(\x7c; \x7c;&
                                                      ) ret)))))
      args)))


;; Parse a shell command prefixed by one or more !
;; Return two values:
;;   A list containing parsed args;
;;   The remaining, unparsed args.
;;
(define (sh-parse-not args)
  (let %again ((negate? #f)
               (args args))
    ; (debugf "sh-parse-not iterate: ret = ~s, negate? = ~s, args = ~s~%" (reverse ret) negate? args)
    (cond
      ((and (not (null? args)) (eq? '! (car args)))
        (%again (not negate?) (cdr args)))
      (negate?
        (let-values (((parsed tail) (sh-parse-cmd args)))
          (values (list 'sh-not parsed) tail)))
      (#t
        (sh-parse-cmd args)))))


;; Parse args for a single shell command, i.e. everything before the first ; & && || |
;; Return two values:
;;   A list containing parsed args;
;;   The remaining, unparsed args.
;
(define (sh-parse-cmd args)
  (let ((saved-args args)
        (ret '())
        (prefix 'sh-cmd)
        (done? (null? args)))
    (until (or done? (null? args))
      ; (debugf "sh-parse-cmd iterate: ret = ~s, args = ~s~%" (reverse ret) args)
      (let ((arg (car args)))
        ; (debugf "sh-parse-cmd iterate: ret = ~s, arg = ~s, args = ~s~%" (reverse ret) arg (cdr args))
        (cond
          ((sh-separator? arg)
            (set! done? #t)) ; separator => exit loop without consuming it
          ((and (pair? arg) (null? ret))
            ; shell command starts with a Scheme or shell subform
            ; => return it as-is, without wrapping in (sh-cmd ...)
            (set! ret arg)
            (set! args (cdr args))
            (set! done? #t)
            (set! prefix #f))
          ((or (string? arg) (integer? arg) (eq? '= arg) (pair? arg) (sh-redirect-operator? arg))
            (when (or (symbol? arg) (pair? arg))
              ; (sh-cmd) does not support env assignment, redirections and closures, use (sh-cmd*)
              (set! prefix 'sh-cmd*))
            (let-values (((arg1 arg2) (parse-operator arg (if (null? (cdr args)) '() (cadr args)))))
              (if arg2
                (begin
                  (set! ret (cons arg2 (cons arg1 ret)))
                  (set! args (cddr args)))
                (begin
                  (set! ret (cons arg1 ret))
                  (set! args (cdr args))))))
          (#t
            (syntax-violation 'sh-parse
              "syntax error, shell DSL atom must be a string, integer, pair, := or redirection operator, found:"
              saved-args arg)))))
    ; (debugf "sh-parse-cmd  return: ret = ~s, args = ~s~%" (reverse ret) args)
    (values
      (if prefix (cons prefix (reverse! ret)) ret)
      args)))


; quote operator (a symbol) to use its name, not its value
; also, operators <& >& must be followed by an fd number or "-" (we convert "-" to -1)
(define (parse-operator arg next)
  (cond
    ((or (eq? arg '<&) (eq? arg '>&))
      (cond
        ((equal? "-" next) ; replace "-" with -1
          (values (list 'quote arg) -1))
        ((string-contains-only-decimal-digits? next) ; replace "NNN" with NNN
          (values (list 'quote arg) (string->number next)))
        ((and (fixnum? next) (fx>=? next -1)) ; also accept fixnums >= -1, although (lex-shell) does not produce them
          (values (list 'quote arg) next))
        (#t
          (syntax-violation 'sh-parse
            "syntax error, redirection operators <& >& must be followed by - or unsigned integer, found:"
            (if (null? next) arg (list arg next))))))
    ((symbol? arg)
      (values (list 'quote arg) #f))
    (#t
      (values arg #f))))

) ; close library
