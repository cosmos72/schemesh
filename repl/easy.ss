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
    (()          (make-dir-reader (current-dir)))
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
(define (files paths)
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


;; easy wrapper for (make-name0-reader)
(define from-name0
  (case-lambda
    ((in close-in?)
      (make-name0-reader in close-in?))
    ((in)
      (make-name0-reader in #f))
    (()
      (make-name0-reader (sh-port #f 0 'binary) #f))))


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
        ((some-string-is? options "--from-json")  (from-json  in close-in?))
        ((some-string-is? options "--from-name0") (from-name0 in close-in?))
        ((some-string-is? options "--from-wire")  (from-wire  in close-in?))
        (else                                     (from-port  in close-in?))))
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
        ((some-string-is? options "--from-name0") (from-name0))
        ((some-string-is? options "--from-wire")  (from-wire))
        (else                                     (from-stdin))))
    (()
      (from-json)))) ;; FIXME: autodetect protocol upon the first call to (reader-get)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; to-....


;; easy wrapper for (make-json-writer)
(define to-json
  (case-lambda
    ((rx out close-out?)
      (copy-all/close rx (make-json-writer out close-out?)))
    ((rx out)
      (to-json rx out #f))
    ((rx)
      (to-json rx (sh-port #f 1 'binary)))))


;; easy wrapper for (make-json1-writer)
(define to-json1
  (case-lambda
    ((rx out close-out?)
      (copy-all/close rx (make-json1-writer out close-out?)))
    ((rx out)
      (to-json1 rx out #f))
    ((rx)
      (to-json1 rx (sh-port #f 1 'binary)))))


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
    ((rx out tty-width theme colors)
      (copy-all/close rx (make-table-writer out #f tty-width theme colors #f)))
    ((rx out tty-width theme)
      (to-table rx out theme tty-width))
    ((rx out tty-width)
      (to-table rx out tty-width 'default #f))
    ((rx out)
      (to-table rx out #f 'default #f))
    ((rx)
      (let* ((fd1  (sh-fd #f 1)) ; file descriptor 1 of current job
             (tty? (eq? 'tty (fd-type fd1)))
             (sz   (and tty? (tty-size fd1))))
      (to-table rx (sh-port #f 1 'textual) (and (pair? sz) (car sz)) 'default (and tty? (tty-colors)))))))


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


;; given a list of strings containing program name followed by mixed args and options,
;; return two values:
;;   a list of arguments i.e. all strings not starting with "-" or found after "--"
;;   a list of options i.e. all strings starting with "-" and found before "--"
(define (split-args-and-options prog-and-args)
  (let ((args    '())
        (options '())
        (options? #t))
    (do ((l (cdr prog-and-args) (cdr l))) ; skip program name
        ((null? l)
         (values (reverse! args) (reverse! options)))
      (let ((e (car l)))
        (if (and options? (string-prefix? e "-"))
          (if (string=? e "--")
            (set! options? #f)
            (set! options (cons e options)))
          (set! args (cons e args)))))))


;; given a list of strings containing program name followed by mixed args and options,
;; return two values:
;;   a list of all strings not starting with "--" or found after "--"
;;   a list of all strings starting with "--" and found before "--"
(define (split-double-hyphens prog-and-args)
  (let ((args    '())
        (options '())
        (options? #t))
    (do ((l (cdr prog-and-args) (cdr l))) ; skip program name
        ((null? l)
         (values (reverse! args) (reverse! options)))
      (let ((e (car l)))
        (if (and options? (string-prefix? e "--"))
          (if (string=? e "--")
            (set! options? #f)
            (set! options (cons e options)))
          (set! args (cons e args)))))))


(define (some-string-contains? args key)
  (any (lambda (s) (and (string? s) (string-contains s key))) args))


(define (some-string-is? options key)
  (any (lambda (s) (and (string? s) (string=? s key))) options))


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
        ((some-string-is? options "--to-json1") (to-json1  rx))
        ((some-string-is? options "--to-table") (to-table  rx))
        ((some-string-is? options "--to-wire")  (to-wire   rx))
        (else                                   (to-stdout rx)))) ;; autodetect stdout fd type
    ((rx)
      (case (fd-type (sh-fd 1))
        ((tty chardev)
          (to-table rx))
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


;; Start a job and return immediately.
;; Redirects job's stdout file descriptor to a pipe and return a reader connected to the other side of such pipe.
;;
;; Optional argument options must be a plist containing zero or more options described in (sh-options),
;; and may also contain a plist entry 'from 'FORMAT, as for example
;;  'from 'json
;; or
;;  'from 'wire
;;
;; May raise exceptions.
;;
;; Implementation note: job is always started in a subprocess,
;; because we need to read its standard output while it runs.
;; Doing that from the main process may deadlock if the job is a multijob or a builtin.
(define sh-start/reader1
  (case-lambda
    ((job options)
      (let* ((sym  (plist-ref options 'from))
             (options-from (if (symbol? sym)
                             (list (string-append "--from-" (symbol->string sym)))
                             '()))
             (fd   (sh-start/fd1 job options))
             (port (fd->port fd 'read 'binary (buffer-mode block)
                             (string-append "job-fd " (number->string fd))
                             (lambda () (fd-close fd)))))
        (from-port port #t options-from)))
    ((job)
      (sh-start/reader1 job '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-syntax filter==>
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
;; or current directory by default.
;; writes to standard output autodetecting output format, or with specified --to-FORMAT.
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


;; the "first" builtin:
;; parse elements from standard input autodetecting input format, or with specified --from-FORMAT,
;; and write the first N elements
;; to standard output autodetecting output format, or with specified --to-FORMAT.
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
;; shell builtin: from


;; the "from" builtin, mirror of the "to" builtin:
;; parse elements from standard input autodetecting input format, or with specified --from-FORMAT,
;; and write each element to standard output autodetecting output format, or with specified --to-FORMAT.
;;
;; As all builtins do, must return job status.
(define (builtin-from job prog-and-args options)
  (let-values (((args options) (split-args-and-options prog-and-args)))
    (when (null? args)
      (raise-errorf 'from "too few arguments"))
    (unless (null? (cdr args))
      (raise-errorf 'from "too many arguments"))
    (let* ((arg (car args))
           (from (cond ((string=? arg "json")   from-json)
                       ((string=? arg "name0")  from-name0)
                       ((string=? arg "wire")   from-wire)
                       (else                    from-stdin))))
      (to-stdout (from) options))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell builtin: jobs


(define (status->verbose status)
  (if (eq? (void) status)
    (ok)
    status))


;; the "jobs" builtin: write known jobs to standard output
;; autodetecting output format, or with specified --to-FORMAT.
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


;; the "parse" builtin: open a file
;; and parse elements from it autodetecting input format, or with specified --from-FORMAT,
;; and write each element to standard output autodetecting output format, or with specified --to-FORMAT.
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


;; the "proc" builtin: write to stdout information about active processes
;; autodetecting output format, or with specified --to-FORMAT.
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
;; shell builtin: select


;; the "select" builtin:
;; parse elements from stdin autodetecting input format, or with specified --from-FORMAT,
;; select only the fields specified in command line,
;; and write each element to standard output with format autodetection, or with specified --to-FORMAT.
;;
;; As all builtins do, must return job status.
(define (builtin-select job prog-and-args options)
  (let-values (((args options) (split-args-and-options prog-and-args)))
    (when (null? args)
      (raise-errorf 'select "too few arguments"))
    (let* ((rx (from-stdin options))
           (rx (make-field-reader rx (map string->symbol args))))
      (to-stdout rx options))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell builtin: skip


;; the "skip" builtin:
;; parse elements from stdin autodetecting input format, or with specified --from-FORMAT,
;; skip first N elements (1 by default),
;; write the remaining elements to stdout autodetecting output format, or with specified --to-FORMAT.
;;
;; As all builtins do, must return job status.
(define (builtin-skip job prog-and-args options)
  (let-values (((args options) (split-args-and-options prog-and-args)))
    (let* ((n (cond
                ((null? args)
                  1)
                ((null? (cdr args))
                  (let ((n (string->number (car args))))
                    (assert* 'skip (integer? n))
                    (assert* 'skip (exact? n))
                    n))
                (else
                 (raise-errorf 'skip "too many arguments"))))
            (rx (from-stdin options))
            (rx (make-range-reader rx n #t)))
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
    (let* ((convert-field-name (if (some-string-is? options "-r")
                                 (lambda (arg) (list '- (string->symbol arg)))
                                 string->symbol))
           (field-names  (map convert-field-name args))
           (rx           (from-stdin options))
           (rx           (make-sort-reader rx field-names 'close-inner)))
      (to-stdout rx options))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell builtin: to


;; the "to" builtin, mirror of the "from" builtin:
;; read structured data from standard input with format autodetection, or with specified --from-FORMAT,
;; and write each element to standard output with format autodetection, or with specified FORMAT.
;;
;; As all builtins do, must return job status.
(define (builtin-to job prog-and-args options)
  (let-values (((args options) (split-args-and-options prog-and-args)))
    (when (null? args)
      (raise-errorf 'to "too few arguments"))
    (unless (null? (cdr args))
      (raise-errorf 'to "too many arguments"))
    (let* ((arg (car args))
           (to (cond ((string=? arg "json")   to-json)
                     ((string=? arg "json1")  to-json1)
                     ((string=? arg "table")  to-table)
                     ((string=? arg "wire")   to-wire)
                     (else                    to-stdout))))
      (to (from-stdin options)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell builtin: threads

;; the "threads" builtin: display known threads as structured data
;;
;; As all builtins do, must return job status.
(define (builtin-threads job prog-and-args options)
  (let-values (((args options) (split-args-and-options prog-and-args)))
    (unless (null? args)
      (raise-errorf 'threads "too many arguments"))
    (let* ((iter (hash-cursor (threads-status)))
           (%threads-status-reader ;; name shown when displaying the closure
             (lambda (rx)
               (let ((cell (hash-cursor-next! iter)))
                 (if cell
                   (let* ((t+status+name (cdr cell))
                          (status (vector-ref t+status+name 1))
                          (name   (vector-ref t+status+name 2)))
                     (values (list 'id (car cell) 'status status 'name name) #t))
                   (values #f #f)))))
           (rx (make-reader %threads-status-reader #f #f)))
      (to-stdout rx options))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell builtin: ulimit


;; parse some "ulimit" options and append them to "parsed" span.
;; return remaining arguments to be parsed
(define ulimit-parse-some
  (let ((htable
          (plist->hashtable string-hash string=?
            '("-H" hard "-S" soft "-a" all "-c" coredump-size "-d" data-size "-e" nice
              "-f" file-size "-i" pending-signals "-l" locked-memory-size "-m" memory-size
              "-n" open-files "-p" pipe-size "-q" msgqueue-size "-r" realtime-priority
              "-s" stack-size "-t" cpu-time "-u" user-processes "-v" virtual-memory-size
              "-x" file-locks "-R" realtime-nonblocking-time))))
    (lambda (args parsed)
      (let* ((arg  (car args))
             (tail (cdr args))
             (parsed-i (hashtable-ref htable arg #f)))
        (cond
          (parsed-i
            (span-insert-right! parsed parsed-i)
            (if (null? (cdr args))
              tail
              (let ((arg2 (cadr args)))
                (cond
                  ((string=? arg2 "unlimited")
                    (span-insert-right! parsed 'unlimited)
                    (cdr tail))
                  ((string-is-unsigned-base10-integer? arg2)
                    (span-insert-right! parsed (string->number arg2))
                    (cdr tail))
                  (else
                    tail)))))
          ((string=? arg "--help")
            (sh-help "ulimit"))
          (else
            (let ((wbuf (make-bytespan 0)))
              (bytespan-insert-right/string! wbuf "schemesh: ulimit: ")
              (bytespan-insert-right/string! wbuf arg)
              (bytespan-insert-right/string! wbuf ": invalid option
ulimit: usage: ulimit [-SHacdefilmnpqrstuvxR] [LIMIT]\n")
              (fd-write/bytespan! (sh-fd #f 2) wbuf))
            (failed 1)))))))


;; parse all "ulimit" options and return them as a span.
;; if an invalid option is found, write error to stderr and return failure status
(define (ulimit-parse-args args)
  (let %ulimit-parse-args ((tail args) (parsed (make-span 0)))
    (if (null? tail)
      parsed
      (let ((next (ulimit-parse-some tail parsed)))
        (if (status? next)
          next
          (%ulimit-parse-args next parsed)))))) ;; argument parsed successfully, iterate


;; search (subspan parsed 0 pos) for the LAST occurrence
;; of either symbol 'hard 'soft and return such symbol.
;;
;; if neither symbol 'hard 'soft is present, return 'soft
(define (ulimit/hard-soft parsed pos)
  (if (fx<=? pos 0)
    #f
    (let ((arg (span-ref parsed (fx1- pos))))
      (if (memq arg '(hard soft))
        arg
        (ulimit/hard-soft parsed (fx1- pos))))))


;; implementation of "ulimit" builtin
(define (ulimit/apply parsed hard-soft start end options)
  (let* ((show-all? (span-index parsed start end (lambda (elem) (eq? elem 'all))))
         (toshow    (if show-all? #f (make-span 0))))
    (let %ulimit/apply ((pos start))
      (if (fx>=? pos end)
        (to-stdout (span-reader (if show-all? (rlimit-all) toshow)) options)
        (let ((arg (span-ref parsed pos))
              (pos+1 (fx1+ pos)))
          (if (memq arg '(all hard soft))
            (%ulimit/apply pos+1) ;; skip 'all 'hard 'soft
            (let* ((new-value  (and (fx<? pos+1 end) (span-ref parsed pos+1)))
                   (set-value? (or (eq? 'unlimited new-value) (integer? new-value))))
              (when set-value?
                (rlimit-set! arg (and (eq? 'soft hard-soft) new-value)
                                 (and (eq? 'hard hard-soft) new-value)))
              (unless show-all?
                (span-insert-right! toshow (rlimit-ref arg)))
              (%ulimit/apply
                (if set-value? (fx1+ pos+1) pos+1))))))))) ;; skip arg and new-value if present


;; the "ulimit" builtin: display or change resource limits
;;
;; As all builtins do, must return job status.
(define (builtin-ulimit job prog-and-args options)
  (let-values (((args options) (split-double-hyphens prog-and-args)))
    (let ((parsed (ulimit-parse-args args)))
      (if (status? parsed)
        parsed
        (let* ((len       (span-length parsed))
               (hard-soft (ulimit/hard-soft parsed len)))
          (ulimit/apply parsed hard-soft 0 len options))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell builtin: where


(define (first-string=? args str)
  (and (pair? args)
       (string? (car args))
       (string=? (car args) str)))


(define (first-string args)
  (and (pair? args)
       (string? (car args))
       (car args)))


(define (second-string args)
  (and (pair? args)
       (pair? (cdr args))
       (string? (cadr args))
       (cadr args)))


(define (third-string args)
  (and (pair? args)
       (pair? (cdr args))
       (pair? (cddr args))
       (string? (caddr args))
       (caddr args)))


(define (try-string->number str)
  (try
    (let ((num (string->number str)))
      (and (real? num) num))
    (catch (ex)
      #f)))


(define (where-equal? obj str-value num-value)
  (cond
    ((string? obj) (string=? obj str-value))
    ((number? obj) (and num-value (= obj num-value)))
    (else          #f)))


(define (where-not-equal? obj str-value num-value)
  (not (where-equal? obj str-value num-value)))


(define (where-less? obj str-value num-value)
  (cond
    ((string? obj) (string<? obj str-value))
    ((number? obj) (and num-value (< obj num-value)))
    (else          #f)))


(define (where-less-equal? obj str-value num-value)
  (cond
    ((string? obj) (string<=? obj str-value))
    ((number? obj) (and num-value (<= obj num-value)))
    (else          #f)))


(define (where-greater? obj str-value num-value)
  (cond
    ((string? obj) (string>? obj str-value))
    ((number? obj) (and num-value (> obj num-value)))
    (else          #f)))


(define (where-greater-equal? obj str-value num-value)
  (cond
    ((string? obj) (string>=? obj str-value))
    ((number? obj) (and num-value (>= obj num-value)))
    (else          #f)))


(define (where-contains? obj str-value num-value)
  (and (string? obj) (and (string-contains obj str-value) #t)))


(define (where-starts? obj str-value num-value)
  (and (string? obj) (string-prefix? obj str-value)))


(define (where-ends? obj str-value num-value)
  (and (string? obj) (string-suffix? obj str-value)))


(define parse-where/operators
  (hashtable string-hash string=?
    "-lt" where-less? "-le" where-less-equal? "-gt" where-greater? "-ge" where-greater-equal?
    "-eq" where-equal? "-ne" where-not-equal?
    "-contains" where-contains? "-starts" where-starts? "-ends" where-ends?))


(define (field* obj sym cache)
  (let ((value (field obj sym cache)))
    (if (symbol? value)
      (symbol->string value)
      value)))


(define (parse-where/cmp args)
  ;; (debugf "parse-where/cmp ~s" args)
  (if (first-string=? args "(")
    (let-values (((expr rest) (parse-where/or (cdr args))))
      (unless (first-string=? rest ")")
        (raise-errorf 'where "missing \")\" in arguments: ~s" args))
      (values expr (cdr rest)))
    (let* ((name      (first-string args))
           (str-op    (second-string args))
           (str-value (third-string args))
           (num-value (and str-value (try-string->number str-value))))
      (unless str-value
        (raise-errorf 'where "invalid comparison arguments: ~s" args))
      (let ((op (hashtable-ref parse-where/operators str-op #f)))
        (unless op
          (raise-errorf 'where "invalid comparison operator ~s in arguments: ~s" str-op args))
        (values (let ((sym (string->symbol name)))
                  (lambda (obj cache)
                    (op (field* obj sym cache) str-value num-value)))
                (cdddr args))))))


(define (parse-where/not args)
  ;; (debugf "parse-where/not ~s" args)
  (if (first-string=? args "-not")
    (let-values (((proc rest) (parse-where/not (cdr args))))
      (values (lambda (obj cache)
                 (not (proc obj cache)))
              rest))
    (parse-where/cmp args)))


(define (parse-where/and args)
  ;; (debugf "parse-where/and ~s" args)
  (let-values (((lhs args) (parse-where/not args)))
    (if (first-string=? args "-and")
      (let-values (((rhs args) (parse-where/and (cdr args))))
        (values (lambda (obj cache)
                   (and (lhs obj cache)
                        (rhs obj cache)))
                args))
      (values lhs args))))


(define (parse-where/or args)
  ;; (debugf "parse-where/or  ~s" args)
  (let-values (((lhs args) (parse-where/and args)))
    (if (first-string=? args "-or")
      (let-values (((rhs args) (parse-where/or (cdr args))))
        (values (lambda (obj cache)
                    (or (lhs obj cache)
                        (rhs obj cache)))
                args))
      (values lhs args))))


;; parse args, which may contain -not -and -or -eq -ne -ge -gt -le -lt
;; and return a closure that evaluates sp boolean expression on an object
(define (parse-where args)
  (let-values (((proc rest) (parse-where/or args)))
    (unless (null? rest)
      (raise-errorf 'where "unexpected arguments: ~s" rest))
    proc))


;; the "where" builtin:
;; read structured data from standard input with format autodetection, or with specified --from-FORMAT,
;; filter elements according to boolean condition specified in arguments,
;; and write each element matching the filter
;; to standard output with format autodetection, or with specified --to-FORMAT.
;;
;; As all builtins do, must return job status.
(define (builtin-where job prog-and-args options)
  (let-values (((args options) (split-double-hyphens prog-and-args)))
    (let* ((proc (parse-where args))
           (rx   (from-stdin options))
           (rx   (make-filter-reader rx proc)))
      (to-stdout rx options))))
