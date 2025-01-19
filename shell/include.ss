;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh shell include (0 1))
  (export
    sh-include sh-parse-file sh-parse-port)
  (import
    (rnrs)
    (only (chezscheme) void)
    (only (schemesh bootstrap) assert* raise-errorf sh-eval until)
    (only (schemesh containers misc) list-iterate string-ends-with?)
    (schemesh parser)
    (schemesh shell job))

;; function read specified file path, parse it with (sh-parse-file)
;; and eval its contents.
;;
;; if top-level parse form is (shell ...) or (shell-subshell ...),
;;   evaluate it and run the corresponding shell commands with (sh-run/i (sh-eval form))
;;   otherwise only evaluate it with (sh-eval form)
(define (sh-include path . options)
  (let ((form (apply sh-parse-file path options)))
    (if (and (pair? form) (memq (car form) '(shell shell-subshell)))
      (sh-run/i (sh-eval form))
      (sh-eval form)))) ; may return multiple values


(define (sh-parse-file path . options)
  (let ((port #f)
        (options (if (memq 'parser options)
                     options
                     (cons 'parser (cons (%default-parser-for-file-extension path) options)))))
    (dynamic-wind
      (lambda ()
        (set! port (open-file-input-port
                     path
                     (file-options)
                     (buffer-mode block)
                     ;; TODO: use UTF-8b instead
                     (make-transcoder (utf-8-codec) (eol-style lf)
                                      (error-handling-mode raise)))))
      (lambda ()
        (apply sh-parse-port port options))
      (lambda ()
        (when port
          (close-port port))))))


(define (sh-parse-port port . options)
  (let-values (((initial-parser enabled-parsers) (%scan-options options)))
    (sh-parse-ctx (make-parsectx port enabled-parsers) initial-parser)))


(define (sh-parse-ctx pctx initial-parser)
  (let-values (((forms updated-parser) (parse-forms pctx  initial-parser)))
    (cond
      ((null? forms)
        '(void))
      ((null? (cdr forms))
        (car forms))
      ((list-iterate forms
         (lambda (form)
           (not (and (pair? form)
                     (memq (car form) '(shell shell-subshell shell-include))))))
        ; no form is (shell ..) or (shell-subshell ...) or (shell-include ...)
        ; -> wrap them in (begin ... )
        (cons 'begin forms))
      (#t
        ; one or more form is (shell ..) or (shell-subshell ...) or (shell-include ...)
        ; -> wrap them in (shell ... )
        (cons 'shell forms)))))


(define (%default-parser-for-file-extension path)
  (if (or (string-ends-with? path ".lisp")
          (string-ends-with? path ".scheme")
          (string-ends-with? path ".ss")
          (string-ends-with? path ".rkt")) ; racket
    'scheme
    'shell))

;; extract from options the values of keys 'parser and 'parsers
;; and return them.
;; if 'parser NAME is not present, default is 'shell
;; il 'parsers '(NAME ...) is not present, default is (parsers)
;;
;; returned values are:
;; 1. initial parser name - a symbol
;; 2. enabled parsers - a hashtable symbol -> parser
(define (%scan-options options)
  (let ((tail options)
        (initial-parser 'shell)
        (enabled-parsers #f)
        (all-parsers (parsers)))
    (until (null? tail)
      (when (null? (cdr tail))
        (raise-errorf 'sh-parse-port "invalid options ~s, missing value after key '~s" options (car tail)))
      (let ((key   (car tail))
            (value (cadr tail)))
        (case key
          ((parser)
            (assert* 'sh-parse-port (symbol? value))
            (set! initial-parser value))
          ((parsers)
            (unless enabled-parsers
              (set! enabled-parsers (make-eq-hashtable)))
            (list-iterate value
              (lambda (name)
                (assert* 'sh-parse-port (symbol? name))
                (let ((parser (hashtable-ref all-parsers name #f)))
                  (unless (parser? parser)
                    (raise-errorf 'sh-parse-port "unknown parser name '~s, not found in (parsers)" name))
                  (hashtable-set! enabled-parsers name parser)))))
          (else
            (raise-errorf 'sh-parse-port "unknown option '~s, expecting 'parser or 'parsers" key))))
        (set! tail (cddr tail)))
    (values initial-parser (or enabled-parsers all-parsers))))




) ; close library
