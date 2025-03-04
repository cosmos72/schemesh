;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh shell eval (0 8 0))
  (export
    sh-eval-file sh-eval-file* sh-eval-fd* sh-eval-port* sh-eval-parsectx* sh-eval-string*
    sh-read-file sh-read-file* sh-read-fd* sh-read-port* sh-read-parsectx* sh-read-string*)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)                 void)
    (only (schemesh bootstrap) assert* raise-errorf until)
    (only (schemesh containers list)   list-iterate)
    (only (schemesh containers string) assert-string-list? string-suffix? string-index-right)
    (only (schemesh containers utf8b)  utf8b->string)
    (only (schemesh posix fd)          fd-close fd-read-until-eof fd-write open-file-fd)
    (schemesh parser)
    (only (schemesh shell parameters)  sh-eval)
    (only (schemesh shell job)         sh-fd sh-builtins sh-builtins-help))


(define (default-parser-for-file-extension path)
  (if (or (string-suffix? path ".sh")
          (not (filename-index-right/char path #\.)))
    'shell
    'scheme))


;; return position of last character equal to ch in the filename part of path,
;; i.e. after the last slash.
;; return #f if ch is not present the filename part of path.
(define (filename-index-right/char path ch)
  (let* ((len   (string-length path))
         (slash (string-index-right path #\/ 0 len)))
    (string-index-right path ch (or slash 0) len)))


;; open specified file path, parse its multi-language source contents with (sh-read-port*)
;; and return the parsed source form.
;;
;; optional arguments:
;;   initial-parser - one of the symbols: 'scheme 'shell 'r6rs.
;;                    default: autodetect from file name's extension.
;;   enables-parsers - a list containing one or more symbols among: 'scheme 'shell 'r6rs
;;                     or a hashtable hashtable symbol -> parser
;;                     or #t that means all known parsers i.e. (parsers)
;;                     default: #t
(define sh-read-file
  (case-lambda
    ((path)
       (sh-read-file* path (default-parser-for-file-extension path) #t))
    ((path initial-parser)
       (sh-read-file* path initial-parser #t))
    ((path initial-parser enabled-parsers)
       (sh-read-file* path initial-parser enabled-parsers))))


;; same as (sh-read-file), with the difference that all arguments are mandatory
(define (sh-read-file* path initial-parser enabled-parsers)
  (assert* 'sh-read-file (symbol? initial-parser))
  (let ((fd #f))
    (dynamic-wind
      (lambda () ; before body
        (set! fd (open-file-fd path 'read)))
      (lambda () ; body
        (sh-read-fd* fd initial-parser enabled-parsers))
      (lambda () ; after body
        (when fd (fd-close fd) (set! fd #f))))))


;; read and parse multi-language source contents from specified file descriptor,
;; and return parsed form.
;; arguments:
;;   fd              - the file descriptor to read from
;;   initial-parser  - one of the symbols: 'scheme 'shell 'r6rs
;;   enables-parsers - a list containing one or more symbols among: 'scheme 'shell 'r6rs
;;                     or a hashtable hashtable symbol -> parser
;;                     or #t that means all known parsers i.e. (parsers)
(define (sh-read-fd* fd initial-parser enabled-parsers)
  (sh-read-string* (utf8b->string (fd-read-until-eof fd)) initial-parser enabled-parsers))


;; read and parse multi-language source contents from specified textual input port,
;; and return parsed form.
;; arguments:
;;   in              - the textual input port to read from
;;   initial-parser  - one of the symbols: 'scheme 'shell 'r6rs
;;   enables-parsers - a list containing one or more symbols among: 'scheme 'shell 'r6rs
;;                     or a hashtable hashtable symbol -> parser
;;                     or #t that means all known parsers i.e. (parsers)
(define (sh-read-port* in initial-parser enabled-parsers)
  (sh-read-parsectx*
    (make-parsectx in (enabled-parsers->hashtable enabled-parsers))
    initial-parser))


;; parse multi-language source contents of specified parsectx, and return parsed form.
;; arguments:
;;   pctx            - the parsectx to read from. also contains the enabled parsers.
;;   initial-parser  - one of the symbols: 'scheme 'shell 'r6rs
(define (sh-read-parsectx* pctx initial-parser)
  (let-values (((forms updated-parser) (parse-forms pctx initial-parser)))
    (wrap-forms! forms)))


;; parse multi-language source contained in specified string, and return parsed form.
;; arguments:
;;   str             - the string to read from
;;   initial-parser  - one of the symbols: 'scheme 'shell 'r6rs
;;   enables-parsers - a list containing one or more symbols among: 'scheme 'shell 'r6rs
;;                     or a hashtable hashtable symbol -> parser
;;                     or #t that means all known parsers i.e. #t
(define (sh-read-string* str initial-parser enabled-parsers)
  (sh-read-port* (open-string-input-port str) initial-parser enabled-parsers))


;; if enabled-parsers is #t, return (parsers)
;; otherwise if it's a hashtable, return it unmodified
;; otherwise check that it is a list of symbols containing parser names,
;;   convert it to a hashtable symbol -> parser and return the created hashtable
(define (enabled-parsers->hashtable enabled-parsers)
  (cond
    ((eq? #t enabled-parsers)
      (parsers))
    ((hashtable? enabled-parsers)
      enabled-parsers)
    (else
      (assert* 'sh-read-file (list? enabled-parsers))
      (let ((ret (make-eq-hashtable))
            (all-parsers (parsers)))
        (list-iterate enabled-parsers
          (lambda (parser-name)
            (assert* 'sh-read-file (symbol? parser-name))
            (let ((parser (hashtable-ref all-parsers parser-name #f)))
              (unless (parser? parser)
                (raise-errorf 'sh-read-file "unknown parser name: ~s" parser-name))
              (hashtable-set! ret parser-name parser))))
        ret))))


;; iterate on each form contained in forms list,
;;   and wrap each form (shell ...) or (shell-subshell) inside (sh-run ...)
;; return a single form, prefixing forms with (begin ...) if needed.
(define (wrap-forms! forms)
  (do ((tail forms (cdr tail)))
      ((null? tail))
    (let ((form (car tail)))
      (when (and (pair? form) (memq (car form) '(shell shell-subshell)))
        (set-car! tail (list 'sh-run form)))))
  (cond
    ((null? forms)       '(void))
    ((null? (cdr forms)) (car forms))
    (else                (cons 'begin forms))))


;; open specified file path, read and parse its multi-language source contents with (sh-read-port*),
;; and eval the parsed source form.
;;
;; arguments:
;;   path            - the filesystem path to read from
;; optional arguments:
;;   initial-parser  - one of the symbols: 'scheme 'shell 'r6rs
;;   enables-parsers - a list containing one or more symbols among: 'scheme 'shell 'r6rs
;;                     or a hashtable hashtable symbol -> parser
;;                     or #t that means all known parsers i.e. (parsers)
;;                     default: #t
(define sh-eval-file
  (case-lambda
    ((path)
       (sh-eval-file* path (default-parser-for-file-extension path) (parsers)))
    ((path initial-parser)
       (sh-eval-file* path initial-parser (parsers)))
    ((path initial-parser enabled-parsers)
       (sh-eval-file* path initial-parser enabled-parsers))))


;; open specified file path, read and parse its multi-language source contents with (sh-read-port*)
;; and eval the parsed source form.
;;
;; arguments:
;;   path            - the filesystem path to read from
;;   initial-parser  - one of the symbols: 'scheme 'shell 'r6rs
;;   enables-parsers - a list containing one or more symbols among: 'scheme 'shell 'r6rs
;;                     or a hashtable hashtable symbol -> parser
;;                     or #t that means all known parsers i.e. (parsers)
(define (sh-eval-file* path initial-parser enabled-parsers)
  (sh-eval (sh-read-file* path initial-parser enabled-parsers)))



;; read and parse multi-language source contents from specified file descriptor,
;; and return parsed form.
;; arguments:
;;   fd              - the file descriptor to read from
;;   initial-parser  - one of the symbols: 'scheme 'shell 'r6rs
;;   enables-parsers - a list containing one or more symbols among: 'scheme 'shell 'r6rs
;;                     or a hashtable hashtable symbol -> parser
;;                     or #t that means all known parsers i.e. (parsers)
(define (sh-eval-fd* fd initial-parser enabled-parsers)
  (sh-eval (sh-read-fd* fd initial-parser enabled-parsers)))


;; read and parse multi-language source contents of specified textual input port,
;; and eval the parsed source form.
;;
;; arguments:
;;   in              - the textual input port to read from
;;   initial-parser  - one of the symbols: 'scheme 'shell 'r6rs
;;   enables-parsers - a list containing one or more symbols among: 'scheme 'shell 'r6rs
;;                     or a hashtable hashtable symbol -> parser
;;                     or #t that means all known parsers i.e. (parsers)
(define (sh-eval-port* in initial-parser enabled-parsers)
  (sh-eval (sh-read-port* in initial-parser enabled-parsers)))


;; read and parse multi-language source contents of specified parsectx,
;; and eval the parsed source form.
;;
;; arguments:
;;   pctx            - the parsectx to read from. also contains the enabled parsers.
;;   initial-parser  - one of the symbols: 'scheme 'shell 'r6rs
(define (sh-eval-parsectx* pctx initial-parser)
  (sh-eval (sh-read-parsectx* pctx initial-parser)))


;; read and parse multi-language source contained in specified string,
;; and eval the parsed source form.
;;
;; arguments:
;;   str             - the string to read from
;;   initial-parser  - one of the symbols: 'scheme 'shell 'r6rs
;;   enables-parsers - a list containing one or more symbols among: 'scheme 'shell 'r6rs
;;                     or a hashtable hashtable symbol -> parser
;;                     or #t that means all known parsers i.e. (parsers)
(define (sh-eval-string* str initial-parser enabled-parsers)
  (sh-eval (sh-read-string* str initial-parser enabled-parsers)))



;; copy-pasted from shell/status.ss
;;
;; normalize job status, converting unexpected status values to '(failed ...)
(define (status-normalize status)
  (cond
    ((eq? (void) status)
      status)
    ((and (pair? status) (memq (car status) '(new running stopped ok exception failed killed)))
      status)
    (else
      (list 'failed status))))


;; the "source" builtin: read a file containing shell script or Scheme source and eval it.
;;
;; As all builtins do, must return job status.
(define (builtin-source job prog-and-args options)
  (assert-string-list? 'builtin-source prog-and-args)
  (cond
    ((null? (cdr prog-and-args))
      (fd-write (sh-fd 2)
        #vu8(115 99 104 101 109 101 115 104 58 32 115 111 117 114 99 101 58 32 116 111 111
             32 102 101 119 32 97 114 103 117 109 101 110 116 115 10)) ; "schemesh: source: too few arguments\n"
      '(failed 1))
    ((not (null? (cddr prog-and-args)))
      (fd-write (sh-fd 2)
        #vu8(115 99 104 101 109 101 115 104 58 32 115 111 117 114 99 101 58 32 116 111 111
             32 109 97 110 121 32 97 114 103 117 109 101 110 116 115 10)) ; "schemesh: source: too many arguments\n"
      '(failed 1))
    (else
      (status-normalize
        (sh-eval-file (cadr prog-and-args))))))


(begin
  (let ((t (sh-builtins)))
    ; additional builtins
    (hashtable-set! t "."          builtin-source)
    (hashtable-set! t "source"     builtin-source))

  (let ((t (sh-builtins-help)))
    (hashtable-set! t "."       (string->utf8 " filename
    read FILENAME and execute the contained shell script or Scheme source code.

    return exit status of last executed command, or value of last evaluated expression.\n"))
    (hashtable-set! t "source"  (hashtable-ref t "." ""))))

) ; close library
