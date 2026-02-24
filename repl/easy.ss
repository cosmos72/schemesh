;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;; #!r6rs ;; does not allow symbol @

;; this file should be included only by file repl/repl.ss


;; evaluate body ... with var ... bound to expr ... then always call (close expr-value) ...
;; even if body raises a condition or calls a continuation
;;
;; If used from a sh-expr, (close expr-value) ... will be called when job finishes.
(define-syntax with-sh-closable
  (syntax-rules ()
    ((_ () body ...)
      (begin0 body ...))
    ((_ ((var expr) ...) body ...)
      (with-sh-resource ((var expr close) ...) body ...))))


;; easy wrapper for (fd-read-all) (get-bytevector-all) (get-string-all) (reader->list)
(define (all from)
  (cond
    ((fixnum? from)      (fd-read-all from))
    ((port? from)        (if (binary-port? from)
                          (get-bytevector-all from)
                          (get-string-all from)))
    ((reader? from)  (reader->list from))
    (else
      (raise-errorf 'all "unsupported reader: ~s" from))))


;; easy wrapper for (fd-read-all) (get-bytevector-all) (get-string-all) (reader->vector)
(define (all/vector from)
  (cond
    ((fixnum? from)      (fd-read-all from))
    ((port? from)        (if (binary-port? from)
                          (get-bytevector-all from)
                          (get-string-all from)))
    ((reader? from)  (reader->vector from))
    (else
      (raise-errorf 'all/vector "unsupported reader: ~s" from))))


;; easy wrapper for (fd-close) (close-port) (reader-close) (writer-close)
(define (close obj)
  (cond
    ((fixnum? obj)      (fd-close obj))
    ((port? obj)        (close-port obj))
    ((reader? obj)  (reader-close obj))
    ((writer? obj)  (writer-close obj))))


;; current directory charspan must NOT be modified => copy it
(define (current-dir) (charspan->string (sh-cwd)))


;; easy wrapper for (make-dir-reader)
(define dir
  (case-lambda
    (()           make-dir-reader (current-dir))
    ((path)      (make-dir-reader path))
    ((path opts) (make-dir-reader path opts))))


;; easy wrapper for multiple (dir)
(define dirs
  (case-lambda
    ((paths opts)
      (let* ((opts (if (or (null? paths) (null? (cdr paths)))
                     opts
                     ;; two or more paths => add each path as prefix
                     (fxior opts (dir-reader-options dir-path-as-prefix))))
             (constructor (lambda (path) (make-dir-reader path opts))))
        (apply readers (map constructor paths))))
    ((paths)
      (dirs paths (dir-reader-options)))))


;; easy wrapper for (port-eof?) (reader-eof?) (writer-eof?)
(define (eof? obj)
  (cond
    ((port? obj)    (port-eof?   obj))
    ((reader? obj)  (reader-eof? obj))
    ((writer? obj)  (writer-eof? obj))))


(define (file-stat/symlinks path)
  (file-stat path '(symlinks)))


;; easy wrapper for (file-stat)
(define (file path)
  (let ((stat (file-stat/symlinks path)))
    (datum-reader stat)))


;; easy wrapper for multiple (file-stat)
(define (files . paths)
  (let ((stats (map file-stat/symlinks paths)))
    (list-reader stats)))


;; easy wrapper for (fd-read) (get-bytevector-some) (get-line) (reader-get)
;; always returns two values:
;;   either (values elem #t)
;;   or (values #<unspecified> #f) on eof
(define (get from)
  (cond
    ((fixnum? from)
      (let* ((bv (make-bytevector 4096 0))
             (n  (fd-read from bv)))
        (cond
          ((fxzero? n)
            (values #f #f))
          (else
            (bytevector-truncate! bv n)
            (values bv #t)))))
    ((port? from)
      (let ((got (if (binary-port? from)
                   (get-bytevector-some from)
                   (get-line from))))
        (values got (not (eof-object? got)))))
    ((reader? from)
      (reader-get from))
    (else
      (raise-errorf 'get "unsupported reader: ~s" from))))


;; easy wrapper for (fd-write-all) (put-bytevector) (put-string) (writer-put)
;; returns unspecified value
(define (put to datum)
  (cond
    ((fixnum? to)
      (assert* 'put (bytevector? datum))
      (fd-write-all to datum))
    ((port? to)
      (cond
        ((binary-port? to)
          (assert* 'put (bytevector? datum))
          (put-bytevector to datum))
        (else
          (assert* 'put (string? datum))
          (put-string to datum))))
    ((writer? to)
      (writer-put to datum))
    (else
      (raise-errorf 'put "unsupported writer: ~s" to))))


;; easy wrapper for (make-process-reader)
(define (proc)
  (make-process-reader))


;; easy wrapper for (get-line) (reader-skip)
;; always returns one value:
;;   #t if one element was skipped,
;;   or #f if reader is exhausted
(define (skip from)
  (cond
    ((and (port? from) (textual-port? from))
      (not (eof-object? (get-line from))))
    ((reader? from)
      (reader-skip from))
    (else
      (raise-errorf 'skip "unsupported reader: ~s" from))))


;; iterate (get from) then (put to) until from is exhausted
(define (copy-all from to)
  (let-values (((datum ok?) (get from)))
    (when ok?
      (put to datum)
      (copy-all from to))))


;; iterate (get from) then (put to) until from is exhausted.
;;
;; then always call (close from) and (close to),
;; even if (get from) or (put to) raise a condition or call a continuation
;;
;; return value of (close to)
(define (copy-all/close from to)
  (let ((%close-and-val
           (let ((closed-from? #f)
                 (closed-to?   #f)
                 (val          #f))
             (lambda ()
               (unless closed-from?
                 (close from)
                 (set! closed-from? #t))
               (unless closed-to?
                 (set! val (close to))
                 (set! closed-to? #t))
               val))))
    (sh-dynamic-wind
      void              ; before
      (lambda ()
        (copy-all from to)
        (%close-and-val))
      void              ; after
      %close-and-val))) ; on-finish


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; from-....


;; easy wrapper for (make-json-reader)
(define from-json
  (case-lambda
    ((in close-in?)
      (make-json-reader in close-in?))
    ((in)
      (make-json-reader in #f))
    (()
      (make-json-reader (sh-port #f 0 'binary) #f))))


;; easy wrapper for (list-reader)
;; l must be a list
(define (from-list l)
  (list-reader l))


;; easy wrapper for (make-queue-reader)
;; q must be a queue-writer
(define (from-queue q)
  (make-queue-reader q))


;; easy wrapper for (vector-reader)
;; v must be a vector
(define (from-vector l)
  (vector-reader l))


;; easy wrapper for (make-wire-reader)
(define from-wire
  (case-lambda
    ((in close-in?)
      (make-wire-reader in close-in?))
    ((in)
      (make-wire-reader in #f))
    (()
      (make-wire-reader (sh-port #f 0 'binary) #f))))


;; Dispatch to one of (from-...) functions depending on options
;;
;; If no options, create a reader that autodetects protocol upon the first call to (reader-get)
;; FIXME: currently creates a json-reader
(define from-port
  (case-lambda
    ((in close-in? options)
      (cond
        ((some-string-is? options "--from-json")  (from-json in close-in?))
        ((some-string-is? options "--from-wire")  (from-wire in close-in?))
        (else                                     (from-port in close-in?))))
    ((in close-in?)
      ;; FIXME: autodetect protocol upon the first call to (reader-get)
      (from-json in close-in?))
    ((in)
      (from-port in #f))))


;; Dispatch to one of (from-...) functions depending on options
;;
;; If no options, create a reader that autodetects protocol upon the first call to (reader-get)
;; FIXME: currently creates a json-reader
(define from-stdin
  (case-lambda
    ((options)
      (cond
        ((some-string-is? options "--from-json")  (from-json))
        ((some-string-is? options "--from-wire")  (from-wire))
        (else                                     (from-stdin))))
    (()
      (from-json)))) ;; FIXME: autodetect protocol upon the first call to (reader-get)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; to-....


;; easy wrapper for (make-wire-writer)
(define to-json
  (case-lambda
    ((rx out close-out?)
      (copy-all/close rx (make-json-writer out close-out?)))
    ((rx out)
      (to-json rx out #f))
    ((rx)
      (to-json rx (sh-port #f 1 'binary)))))


;; easy wrapper for (all)
(define (to-list rx)
  (with-sh-closable ((rx rx))
    (all rx)))


;; easy wrapper for (make-queue-writer)
(define (to-queue rx)
  (copy-all/close rx (make-queue-writer)))


;; easy wrapper for (make-table-writer)
(define to-table
  (case-lambda
    ((rx out theme colors)
      (copy-all/close rx (make-table-writer out #f theme colors #f)))
    ((rx out theme)
      (to-table rx out theme #f))
    ((rx out)
      (to-table rx out 'default #f))
    ((rx)
      (to-table rx (sh-port #f 1 'textual) 'default (eq? 'tty (fd-type (sh-fd 1)))))))


;; easy wrapper for (all/vector)
(define (to-vector rx)
  (with-sh-closable ((rx rx))
    (all/vector rx)))


;; easy wrapper for (make-wire-writer)
(define to-wire
  (case-lambda
    ((rx out close-out?)
      (copy-all/close rx (make-wire-writer out close-out?)))
    ((rx out)
      (to-wire rx out #f))
    ((rx)
      (when (eq? 'tty (fd-type (sh-fd 1)))
        (raise-errorf 'to-wire "refusing to write binary data to a terminal"))
      (to-wire rx (sh-port #f 1 'binary) #f))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (split-args-and-options prog-and-args)
  (let ((args    '())
        (options '())
        (options? #t))
    (do ((l (cdr prog-and-args) (cdr l)))
        ((null? l)
         (values (reverse! args) (reverse! options)))
      (let ((e (car l)))
        (if (and options? (string-prefix? e "-"))
          (if (string=? e "--")
            (set! options? #f)
            (set! options (cons e options)))
          (set! args (cons e args)))))))


(define (some-string-contains? args key)
  (any (lambda (s) (string-contains s key)) args))


(define (some-string-is? options key)
  (any (lambda (s) (string=? s key)) options))


;; Dispatch to one of (to-...) functions depending on options,
;; or on stdout fd type if no options:
;;   tty chardev => to-table
;;   socket      => to-wire
;;   else        => to-json
(define to-stdout
  (case-lambda
    ((rx options)
      (cond
        ((some-string-is? options "--to-json")  (to-json   rx))
        ((some-string-is? options "--to-table") (to-table  rx))
        ((some-string-is? options "--to-wire")  (to-wire   rx))
        (else                                   (to-stdout rx)))) ;; autodetect stdout fd type
    ((rx)
      (case (fd-type (sh-fd 1))
        ((tty chardev)
          (to-table rx (sh-port #f 1 'textual) 'default (tty-colors)))
        ((socket)
          (to-wire rx))
        (else
          (to-json rx))))))


;; detect number of colors supported by current terminal.
;; This is a rough heuristic.
;; TODO: move to lineedit and use also terminal autodetection with ESC [ c ?
(define (tty-colors)
  (let ((env (sh-env-ref #f "COLORTERM" #f)))
    (if env
      (if (or (string=? env "truecolor") (string=? env "24bit"))
        #t
        ;; COLORTERM set to an unexpected value
        256)
      (let ((env (sh-env-ref #f "TERM" #f)))
        (if env
          (cond
            ((string=? env "xterm")
              ;; assume that nowadays "xterm" means truecolor
              #t)
            ((or (string-suffix? env "256color") (string-contains env "xterm"))
              ;; assume that nowadays "...xterm..." means at least 256 colors
              256)
            (else
              ;; unknown terminal
              8))
          ;; $TERM is not set
          #f)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-syntax copy==>
  (syntax-rules ()
    ((_ body ...)
      (==> from-stdin => body ... => to-stdout))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; select


;; create and return a field-reader wrapping a user-provided reader.
;; usage: (select reader field-name ...)
;; arguments:
;;   rx             - the "inner" reader to be wrapped
;;   field-name ... - the fields to extract from elements generated by the "inner" reader.
;;                    each field-name must be either a symbol,
;;                    or a list (old-field-name new-field-name) - the latter will rename a field
;;
;; for each element generated by the wrapped reader,
;; the field reader will extract only the specified fields from it,
;; and generate a plist containing those field names and their values.
(define-syntax select
  (lambda (stx)
    (syntax-case stx ()
      ((_ reader field-name ...)
        (do ((l (syntax->datum #'(field-name ...)) (cdr l)))
            ((null? l))
          (let ((old-name-new-name (car l)))
            (unless (symbol? old-name-new-name)
              (assert* 'select (fx=? 2 (length old-name-new-name)))
              (assert* 'select (symbol? (car old-name-new-name)))
              (assert* 'select (symbol? (cadr old-name-new-name))))))
        #'(make-field-reader reader '(field-name ...) 'close-inner)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sort-by


;; create and return a sort-reader wrapping a user-provided reader.
;; usage: (sort-by reader field-name ...)
;; arguments:
;;   rx             - the "inner" reader to be wrapped
;;   field-name ... - the fields to use for ordering elements generated by the "inner" reader.
;;                    each field-name must be a symbol
(define-syntax sort-by
  (syntax-rules ()
    ((_ reader field-name ...)
      (make-sort-reader reader '(field-name ...) 'close-inner))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; first


;; create a range-reader that will generate only the first n elements of inner reader,
;; then it will be exhausted.
;;
;; usage: (first reader [n])
;;
;; n must be an exact integer >= 0 and defaults to 1.
;;
(define first
  (case-lambda
    ((reader n)
      (make-range-reader reader 0 n 'close-inner))
    ((reader)
      (make-range-reader reader 0 1 'close-inner))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; skip-first


;; created range-reader will skip the first n elements of inner reader,
;; then it generate the remaining elements (if any) of the inner reader.
;;
;; usage: (skip-first reader [n])
;;
;; n must be an exact integer >= 0 and defaults to 1.
;;
(define skip-first
  (case-lambda
    ((reader n)
      (make-range-reader reader n #t 'close-inner))
    ((reader)
      (make-range-reader reader 1 #t 'close-inner))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; where


;; create and return a filter reader wrapping a user-provided reader.
;; usage: (where reader expression)
;;   expression will be called once for each processed element,
;;   and should contain one or more forms ,name i.e. (unquote name) that will be expanded
;;   to the value of (field elem 'name) of the element being processed,
;;   or one or more symbols @@ that will be expanded to the element being processed.
;;
;; Note: works, but changes the meaning of unquote, and forces user-provided code to insert unquote in unexpected places,
;; thus breaks quasiquoting, both inside (where) own's definition and inside expressions passed to (where)
;; Also breaks (expand `(where reader user-provided-form-containing-unquote))
;;
;; See (where@) for a cleaner alternative.
(define-macro (where reader expr)
  (list 'make-filter-reader reader
    (list 'lambda '(elem cache)
      (list 'let-syntax '((@@ (identifier-syntax elem)))
        (list 'let-macro '((unquote name) (list 'field 'elem (list 'quote name) 'cache))
           expr)))
    ''close-inner))


;; create a where reader wrapping a user-provided reader.
;; usage: (where@ reader expression)
;;   expression will be called once for each processed element,
;;   and should contain one or more forms (@ name) that will be expanded
;;   to the value of (field elem 'name) of the element being processed,
;;   or one or more symbols @@ that will be expanded to the element being processed.
;;
;; Cleaner than (where), as it only changes the meaning of seldom-used @ and @@
(define-macro (where@ reader expr)
  `(make-filter-reader ,reader
     (lambda (elem cache)
       (let-syntax ((@@ (identifier-syntax elem))
                    (@  (syntax-rules ()
                          ((_ name) (field elem 'name cache)))))
         ,expr))
     'close-inner))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell builtin: dir


;; used by builtin-dir
(define (make-file-stat-or-dir-readers paths opts list-dirs-not-their-contents?)
  (let %loop ((paths paths) (opts opts) (l '()))
    (if (null? paths)
      (apply readers (reverse! l))
      (let* ((path (car paths))
             (stat (file-stat/symlinks path)))
        (unless (dir-entry? stat)
          (raise-c-errno 'dir 'lstat stat path))
        (let ((rx (if (or list-dirs-not-their-contents?
                          (not (eq? 'dir (dir-entry-type stat))))
                    (datum-reader stat)
                    (make-dir-reader path opts))))
          (%loop (cdr paths) opts (cons rx l)))))))
      

;; the "dir" builtin: display specified files or directories contents,
;; or current directory by default
;;
;; As all builtins do, must return job status.
(define (builtin-dir job prog-and-args options)
  (let-values (((paths options) (split-args-and-options prog-and-args)))
    ;; if no paths specified, list current directory
    (let* ((paths (if (null? paths) (list (sh-cwd)) paths))

           ;; hide files starting with "." by default. option -a shows them
           (opts (fxior
                   (if (some-string-contains? options "a")
                     (dir-reader-options)
                     (dir-reader-options dir-hide-dot-files))
                   (if (null? (cdr paths))
                     (dir-reader-options)
                     ;; two or more paths => add each path as prefix
                     (dir-reader-options dir-path-as-prefix))))

           (list-dirs-not-their-contents? (some-string-contains? options "d"))

           (rx (make-file-stat-or-dir-readers paths opts list-dirs-not-their-contents?))
           ;; show only some fields by default. option -l shows more fields, option -v shows all fields
           (rx (cond
                ((some-string-contains? options "v")
                  rx)
                ((some-string-contains? options "l")
                  (select rx name type size link modified accessed mode user group))
                (else
                  (select rx name type size link modified)))))
      (to-stdout (sort-by rx name) options))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell builtin: first


;; the "first" builtin: read from stdin autodetecting input format,
;; write to stdout the first N elements (1 by default) autodetecting output format.
;;
;; As all builtins do, must return job status.
(define (builtin-first job prog-and-args options)
  (let-values (((args options) (split-args-and-options prog-and-args)))
    (let* ((n (cond
                ((null? args)
                  1)
                ((null? (cdr args))
                  (let ((n (string->number (car args))))
                    (assert* 'first (integer? n))
                    (assert* 'first (exact? n))
                    n))
                (else
                 (raise-errorf 'first "too many arguments"))))
            (rx (from-stdin options))
            (rx (make-range-reader rx 0 n)))
      (to-stdout rx options))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell builtin: jobs


(define (status->verbose status)
  (if (eq? (void) status)
    (ok)
    status))


;; the "jobs" builtin: list known jobs
;;
;; As all builtins do, must return job status.
(define (builtin-jobs job prog-and-args options)
  (let ((sp (sh-jobs)))
    (do ((i 0 (fx1+ i))
         (n (span-length sp)))
        ((fx>=? i n))
      (let* ((pair (span-ref sp i))
             (id   (car pair))
             (job  (cdr pair)))
        (span-set! sp i (list 'id  id
                              'pid (sh-job-pid job)
                              'pgid (sh-job-pgid job)
                              'status (status->verbose (sh-job-status job))
                              'cmdline (sh-job->string job)))))
    (let-values (((args options) (split-args-and-options prog-and-args)))
      (to-stdout (span-reader sp) options))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell builtin: parse


;; the "parse" builtin:
;; open a file and read structured data from it with format autodetection, or with specified --from-FORMAT,
;; and write each element to standard output with format autodetection, or with specified --to-FORMAT.
;;
;; As all builtins do, must return job status.
(define (builtin-parse job prog-and-args options)
  (let-values (((args options) (split-args-and-options prog-and-args)))
    (when (null? args)
      (raise-errorf 'parse "too few arguments"))
    (unless (null? (cdr args))
      (raise-errorf 'parse "too many arguments"))
    (let ((in (file->port (car args) 'read '() 'binary (buffer-mode block))))
      (to-stdout (from-port in 'close-in options) options))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell builtin: proc


;; the "proc" builtin: display information about active processes.
;;
;; As all builtins do, must return job status.
;;
;; TODO: implement [-o fields] [-O fields]
(define (builtin-proc job prog-and-args options)
  (let-values (((args options) (split-args-and-options prog-and-args)))
    (let* ((user    (if (some-string-contains? args "a") #f (c-username)))
           (tty?    (if (some-string-contains? args "x") #f #t))
           (fields  (cond
                      ((some-string-contains? args "v")
                        #f)
                      ((some-string-contains? args "u")
                        '(user pid user-time mem-rss tty state start-time name))
                      (else
                        '(pid tty start-time name))))
           (rx   (proc))
           (rx   (if user
                   (make-filter-reader rx (lambda (elem cache)
                                            (equiv? user (field elem 'user cache)))
                                       'close-inner)
                  rx))
           (rx   (if tty?
                   (make-filter-reader rx (lambda (elem cache)
                                            (let ((tty (field elem 'tty cache)))
                                              (and (string? tty) (not (fxzero? (string-length tty))))))
                                       'close-inner)
                   rx))
           (rx   (if fields
                   (make-field-reader rx fields 'close-inner)
                   rx)))
      (to-stdout rx options))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell builtin: sort-by


;; the "sort-by" builtin: read from stdin autodetecting input format,
;; sort elements by specified field names,
;; write sorted elements to stdout autodetecting output format.
;;
;; As all builtins do, must return job status.
(define (builtin-sort-by job prog-and-args options)
  (let-values (((args options) (split-args-and-options prog-and-args)))
    (when (null? args)
      (raise-errorf 'sort-by "too few arguments"))

    (let* ((field-names (map string->symbol args))
           (rx (from-stdin options))
           (rx (make-sort-reader rx field-names 'close-inner)))
      (to-stdout rx options))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell builtin: to


;; the "to" builtin:
;; read structured data from standard input with format autodetection, or with specified --from-FORMAT,
;; and write each element to standard output with format autodetection, or with specified --to-FORMAT.
;;
;; As all builtins do, must return job status.
(define (builtin-to job prog-and-args options)
  (let-values (((args options) (split-args-and-options prog-and-args)))
    (when (null? args)
      (raise-errorf 'to "too few arguments"))
    (unless (null? (cdr args))
      (raise-errorf 'to "too many arguments"))
    (let* ((from (cond ((some-string-is? options "--from-json") from-json)
                       ((some-string-is? options "--from-wire") from-wire)
                       (else                                    from-stdin)))
           (arg (car args))
           (to (cond ((string=? arg "table")  to-table)
                     ((string=? arg "json")   to-json)
                     ((string=? arg "wire")   to-wire)
                     (else                    to-stdout))))
      (to (from)))))
