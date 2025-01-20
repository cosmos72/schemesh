;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh shell include (0 7 0))
  (export
    sh-read-file sh-read-file* sh-read-port* sh-read-parsectx*)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) datum eval-when void)
    (only (schemesh bootstrap) assert* raise-errorf sh-eval until)
    (only (schemesh containers misc) list-iterate string-ends-with?)
    (schemesh parser)
    (schemesh shell job))


(define (default-parser-for-file-extension path)
  (if (or (string-ends-with? path ".lisp")
          (string-ends-with? path ".scheme")
          (string-ends-with? path ".ss")
          (string-ends-with? path ".rkt")) ; racket
    'scheme
    'shell))

;; open specified file path, parse its contents with (sh-read-port*)
;; and return parsed form.
;;
;; optional arguments:
;;   initial-parser - one of the symbols: 'scheme 'shell 'r6rs
;;   enables-parsers - a list containing one or more symbols among: 'scheme 'shell 'r6rs
;;                     or a hashtable hashtable symbol -> parser
(define sh-read-file
  (case-lambda
    ((path)
       (sh-read-file* path (default-parser-for-file-extension path) (parsers)))
    ((path initial-parser)
       (sh-read-file* path initial-parser  (parsers)))
    ((path initial-parser enabled-parsers)
       (sh-read-file* path initial-parser enabled-parsers))))


(define (sh-read-file* path initial-parser enabled-parsers)
  (assert* 'sh-read-file (symbol? initial-parser))
  (let ((in (open-file-input-port path (file-options) (buffer-mode block)
              (make-transcoder (utf-8-codec) (eol-style lf) (error-handling-mode raise)))))
    (dynamic-wind
      void       ; before body
      (lambda () ; body
        (sh-read-port* in initial-parser enabled-parsers))
      (lambda () ; after body
        (close-port in)))))


(define (sh-read-port* in initial-parser enabled-parsers)
  (sh-read-parsectx*
    (make-parsectx in (parser-names->hashtable enabled-parsers))
    initial-parser))


(define (sh-read-parsectx* pctx initial-parser)
  (let-values (((forms updated-parser) (parse-forms pctx initial-parser)))
    (wrap-forms! forms)))


;; if enabled-parsers is a hashtable, return it unmodified.
;; otherwise check that enabled-parsers is a list of symbols containing parser names,
;;   convert it to a hashtable symbol -> parser and return the created hashtable
(define (parser-names->hashtable enabled-parsers)
  (if (hashtable? enabled-parsers)
    enabled-parsers
    (let ((ret (make-eq-hashtable))
          (all-parsers (parsers)))
      (assert* 'sh-read-file (list? enabled-parsers))
      (list-iterate enabled-parsers
        (lambda (parser-name)
          (assert* 'sh-read-file (symbol? parser-name))
          (let ((parser (hashtable-ref all-parsers parser-name #f)))
            (unless (parser? parser)
              (raise-errorf 'sh-read-file "unknown parser name: ~s" parser-name))
            (hashtable-set! ret parser-name parser))))
      ret)))


;; iterate on each form contained in forms list,
;;   and wrap each form (shell ...) or (shell-subshell) inside (sh-run ...)
;; return a single form, prefixing forms with (begin ...) if needed.
(define (wrap-forms! forms)
  (do ((tail forms (cdr tail)))
      ((null? tail))
    (let ((form (car forms)))
      (when (and (pair? form) (memq (car form) '(shell shell-subshell)))
        (set-car! forms (list 'sh-run form)))))
  (cond
    ((null? forms)       '(void))
    ((null? (cdr forms)) (car forms))
    (#t                  (cons 'begin forms))))

) ; close library
