;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; thread parameter (repl-args) must be empty or a list
;;   (parser eval-func linectx repl-init-file-path repl-quit-file-path)
;; containing arguments of current call to (repl) or (repl*)
(define repl-args
  (sh-make-thread-parameter
    '()
    (lambda (args)
      (unless (list? args)
        (raise-errorf 'repl-args "~s is not a list" args))
      args)))


;; return the linectx contained in thread parameter (repl-args),
;; or #f if not present.
(define (repl-args-linectx)
  (let ((repl-args (repl-args)))
    (and (fx>=? (length repl-args) 3)
         (list-ref repl-args 2))))


;; React to uncaught conditions
(define sh-exception-handler
  (case-lambda
    ((obj proc-after-body)
      (let ((port (console-error-port)))
        (dynamic-wind
          (lambda () ; before body
            (put-string port "; ")
            (display-condition obj port)
            (newline port)
            (flush-output-port port))
          (lambda () ; body
            (when (or (serious-condition? obj) (not (warning? obj)))
              (debug-condition obj) ;; save obj into thread-parameter (debug-condition)
              (if (debug-on-exception)
                (debug)
                (put-string port "; type (debug) to enter the debugger, or (debug-condition) to retrieve the condition.\n"))
              (flush-output-port port)))
          proc-after-body)))
    ((obj)
      (sh-exception-handler obj void))))


;; implementation of "echo" builtin, writes user-specified arguments to file descriptor 1.
;; separator between arguments is #\space
;; terminating character after arguments is #\newline
(define (sh-echo . args)
  (let ((wbuf (make-bytespan 0))
        (fd   (sh-fd 1)))
    (do ((tail args (cdr tail)))
        ((null? tail))
      (unless (eq? args tail)
        (bytespan-insert-right/u8! wbuf 32)) ; space
      (bytespan-insert-right/string! wbuf (car tail))
      (when (fx>=? (bytespan-length wbuf) 4096)
        (fd-write/bytespan! fd wbuf)))
    (bytespan-insert-right/u8! wbuf 10) ; newline
    (fd-write/bytespan! fd wbuf))
  (void))


;; implementation of "echo0" builtin, writes user-specified arguments to file descriptor 1
;; separator between arguments is #\nul
;; terminating character after arguments is #\nul
(define (sh-echo0 . args)
  (let ((wbuf (make-bytespan 0))
        (fd   (sh-fd 1)))
    (do ((tail args (cdr tail)))
        ((null? tail))
      (bytespan-insert-right/string! wbuf (car tail))
      (bytespan-insert-right/u8! wbuf 0) ; #\nul
      (when (fx>=? (bytespan-length wbuf) 4096)
        (fd-write/bytespan! fd wbuf)))
    (fd-write/bytespan! fd wbuf))
  (void))


;; implementation of "false" builtin, always exits with failure exit status (failed 1)
(define (sh-false . ignored-args)
  (failed 1))


;; implementation of "help" builtin, display general help or help for specified builtin.
(define sh-help
  (case-lambda
    (()
      (let* ((lctx (repl-args-linectx))
             (wbuf (linectx-wbuf lctx)))
        (bytespan-insert-right/string! wbuf "schemesh version")
        (let-values ((version (sh-version-number)))
          (do ((l version (cdr l)))
              ((null? l))
            (bytespan-insert-right/u8! wbuf (if (eq? l version) 32 46))
            (bytespan-display-right/fixnum! wbuf (car l))))
        (bytespan-insert-right/string! wbuf "
Copyright (C) 2023-2025 Massimiliano Ghilardi <https://github.com/cosmos72/schemesh>

  schemesh comes with ABSOLUTELY NO WARRANTY; for details type 'help warranty'.
  This is free software, and you are welcome to redistribute it
  under certain conditions; type 'help copyright' for details.

Type 'help' to display this text. Type 'help name' for help about the builtin 'name'.
The following names are recognized as builtins:\n\n")

        (let ((names (hashtable-keys (sh-builtins))))
          (subvector-sort! string<? names)
          (lineedit-display-table lctx (vector->span* names)))
        (lineedit-flush lctx)
        (void)))
    ((name)
      (let ((wbuf (bytespan))
            (help-bvector (hashtable-ref (sh-builtins-help) name #f)))
        (if help-bvector
          (begin
            (bytespan-insert-right/string!  wbuf name)
            (bytespan-insert-right/bytevector! wbuf help-bvector)
            (fd-write/bytespan! (sh-fd 1) wbuf)
            (void))
          (begin
            (bytespan-insert-right/string! wbuf "schemesh: help: no help for builtin '")
            (bytespan-insert-right/string! wbuf name)
            (bytespan-insert-right/string! wbuf "'. Try 'help' or 'help help'.\n")
            (fd-write/bytespan! (sh-fd 1) wbuf)
            (failed 1)))))))



;; return gbuffer containing previous commands saved to history,
;; or #f if not available.
(define repl-history
  (case-lambda
    (()
      (let ((lctx (repl-args-linectx)))
        (and (linectx? lctx) (linectx-history lctx))))
    ((n)
      (let* ((ch (repl-history))
             (len (if ch (gbuffer-length ch) 0)))
        (and (fx<? -1 n len)
             (gbuffer-ref ch n))))))


;; implementation of "history" builtin, display previous commands saved to history.
(define repl-history-display
  (case-lambda
    (()
      (repl-history-display (repl-args-linectx)))
    ((lctx)
      (let ((fd   (sh-fd 1)))
        ; (debugf "repl-history-display ~s" lctx)
        (if (linectx? lctx)
          (let ((wbuf (make-bytespan 0)))
            (vhistory-iterate (linectx-history lctx)
              (lambda (i lines)
                (bytespan-insert-right/u8!      wbuf 32) ; space
                (bytespan-display-right/fixnum! wbuf i)
                (bytespan-insert-right/u8!      wbuf 9) ; tab
                (vlines-iterate lines
                  (lambda (j line)
                    (vline-display/bytespan line wbuf)))
                (bytespan-insert-right/u8! wbuf 10) ; newline
                (when (fx>=? (bytespan-length wbuf) 4096)
                  (fd-write/bytespan! fd wbuf))))
            (fd-write/bytespan! fd wbuf)
            (void)) ; return (void), means builtin finished successfully
          (failed 1))))))


;; implementation of "true" builtin, always exits successfully i.e. with exit status (void)
(define (sh-true . ignored-args)
  (void))



;; implementation of "ulimit" builtin, writes getrlimit() results to file descriptor 1.
(define (sh-ulimit . args)
  (let ((wbuf   (make-bytespan 0))
        (fd     (sh-fd 1))
        (parsed (make-span 0)))
    (let %sh-ulimit ((tail args))
      (if (null? tail)
        (let ((ret (ulimit/span parsed wbuf)))
          (fd-write/bytespan! fd wbuf)
          ret)
        (let ((ret (ulimit-parse-arg (car tail) wbuf parsed)))
          (if (status? ret)
            (begin  ;; error parsing argument, bail out
              (unless (bytespan-empty? wbuf)
                (fd-write/bytespan! fd wbuf))
              ret)
            (%sh-ulimit (cdr tail)))))))) ;; argument parsed successfully, iterate


;; parse a single (sh-ulimit) argument
(define ulimit-parse-arg
  (let ((htable
          (plist->hashtable string-hash string=?
            '("-H" hard "-S" soft "-a" all "-c" coredump-size "-d" data-size "-e" nice
              "-f" file-size "-i" pending-signals "-l" locked-memory-size "-m" memory-size
              "-n" open-files "-p" pipe-size "-q" msgqueue-size "-r" realtime-priority
              "-s" stack-size "-t" cpu-time "-u" user-processes "-v" virtual-memory-size
              "-x" file-locks "-R" realtime-nonblocking-time))))
    (lambda (arg wbuf parsed)
      (let ((parsed-i (hashtable-ref htable arg #f)))
        (cond
          (parsed-i
            (span-insert-right! parsed parsed-i)
            #t) ;; not a job status, tell ulimit/span to keep parsing
          ((equal? arg "--help")
            (builtin-help #f '("help" "ulimit") '()))
          (else
            (bytespan-insert-right/string! wbuf "schemesh: ulimit: ")
            (bytespan-insert-right/string! wbuf arg)
            (bytespan-insert-right/string! wbuf ": invalid option
ulimit: usage: ulimit [-SHacdefilmnpqrstuvxR] [limit]\n")
            (failed 1)))))))


;; return 'hard if (subspan sp-args 0 pos) contains symbol 'hard
;; and it appears *after* any symbol 'soft
;; otherwise return 'soft
(define (ulimit/hard-soft sp-args pos)
  (if (fx<? pos 0)
    'soft
    (let ((arg (span-ref sp-args pos)))
      (if (memq arg '(hard soft))
        arg
        (ulimit/hard-soft sp-args (fx1- pos))))))

(define ulimit-all-keys (list->span (rlimit-keys)))

;; called by (sh-ulimit) builtin
(define (ulimit/span sp-args wbuf)
  ;; (debugf "ulimit/span ~s" sp-args)
  (let* ((arg-n     (span-length sp-args))
         (hard-soft (ulimit/hard-soft sp-args (fx1- arg-n)))
         (args      (if (span-index sp-args 0 arg-n (lambda (elem) (eq? elem 'all)))
                      ulimit-all-keys
                      sp-args)))
    (for-span ((arg args))
      (unless (memq arg '(hard soft))
        (let ((value (rlimit-ref hard-soft arg)))
          (bytespan-insert-right/string!     wbuf (symbol->string arg))
          (bytespan-insert-right/bytevector! wbuf #vu8(9 9)) ;; tabs
          (cond
            ((symbol? value)
              (bytespan-insert-right/string!     wbuf (symbol->string value)))
            ((boolean? value)
              (bytespan-insert-right/bytevector! wbuf (if value #vu8(35 116) #vu8(35 102))))
            (else
              (bytespan-display-right/integer!   wbuf value)))
          (bytespan-insert-right/u8! wbuf 10)))))) ;; newline



;; the "echo" builtin: write arguments to (sh-fd 1)
;; separating each pair with a #\space and terminating them with a #\newline
;;
;; As all builtins do, must return job status.
(define (builtin-echo job prog-and-args options)
  (assert-string-list? 'builtin-echo prog-and-args)
  (apply sh-echo (cdr prog-and-args)))


;; the "echo0" builtin: write arguments to (sh-fd 1)
;; terminating each one with a #\nul
;;
;; As all builtins do, must return job status.
(define (builtin-echo0 job prog-and-args options)
  (assert-string-list? 'builtin-echo0 prog-and-args)
  (apply sh-echo0 (cdr prog-and-args)))


;; the "false" builtin: return (failed 1)
;;
;; As all builtins do, must return job status.
(define (builtin-false job prog-and-args options)
  (assert-string-list? 'builtin-false prog-and-args)
  (sh-false))


;; the "help" builtin: write help, or help for specified builtin, to (sh-fd 1).
;;
;; As all builtins do, must return job status.
(define (builtin-help job prog-and-args options)
  (assert-string-list? 'builtin-help prog-and-args)
  (if (null? (cdr prog-and-args))
    (sh-help)
    (sh-help (cadr prog-and-args))))


;; the "history" builtin: write current history to (sh-fd 1).
;;
;; As all builtins do, must return job status.
(define (builtin-history job prog-and-args options)
  (assert-string-list? 'builtin-history prog-and-args)
  (repl-history-display))


;; the "true" builtin: return (void)
;;
;; As all builtins do, must return job status.
(define (builtin-true job prog-and-args options)
  (assert-string-list? 'builtin-true prog-and-args)
  (sh-true))


;; the "ulimit" builtin: display resource limits
;;
;; As all builtins do, must return job status.
(define (builtin-ulimit job prog-and-args options)
  (assert-string-list? 'builtin-ulimit prog-and-args)
  (apply sh-ulimit (cdr prog-and-args)))


;; given a command line prog-and-args i.e. a list of strings,
;; extract the first string and return the corresponding builtin.
;; Return #f if no corresponding builtin is found.
;; Return builtin-true if prog-and-args is the empty list.
(define (sh-builtin-find prog-and-args)
  (if (null? prog-and-args)
    builtin-true ; empty command line, run it with (builtin-true)
    (hashtable-ref (sh-builtins) (car prog-and-args) #f)))



;; function returning the global hashtable name -> builtin.
;; Each builtin must be a function accepting as arguments:
;;   a job (actually a cmd)
;;   a prog-and-args i.e. a list of strings containing the builtin name and its arguments
;;   a list of options
;; and must execute the builtin then return its exit status.
(define sh-builtins
  (let ((t (make-hashtable string-hash string=?)))
    (hashtable-set! t ":"       builtin-true)
    (hashtable-set! t "echo"    builtin-echo)
    (hashtable-set! t "echo0"   builtin-echo0)
    (hashtable-set! t "false"   builtin-false)
    (hashtable-set! t "help"    builtin-help)
    (hashtable-set! t "history" builtin-history)
    (hashtable-set! t "true"    builtin-true)
    (hashtable-set! t "ulimit"  builtin-ulimit)
    (lambda () t)))


;; function returning the global hashtable name -> help text for builtin.
;; Each help text must be a bytevector starting with byte 32 i.e. space and ending with byte 10 i.e. newline.
(define sh-builtins-help
  (let ((t (make-hashtable string-hash string=?)))

    (hashtable-set! t ":"       (string->utf8 " [arg ...]
    ignore arguments. return success i.e. (void).\n"))

    (hashtable-set! t "copyright" (string->utf8 "

schemesh, a fusion between interactive Unix shell and Chez Scheme REPL.
Copyright (C) 2023-2025 Massimiliano Ghilardi <https://github.com/cosmos72/schemesh>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along
  with this program; if not, write to the Free Software Foundation, Inc.,
  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

Informative note: an online version of the GNU General Public License version 2.0,
is usually available at <https://www.gnu.org/licenses/old-licenses/gpl-2.0.html#SEC1>\n"))

    (hashtable-set! t "echo"    (string->utf8 " [arg ...]
    write space-separated arguments to standard output, followed by a single newline.

    return success.\n"))

    (hashtable-set! t "echo0"   (string->utf8 " [arg ...]
    write NUL-terminated arguments to standard output.

    return success.\n"))

    (hashtable-set! t "false"   (string->utf8 " [arg ...]
    ignore arguments. return failure i.e. (failed 1).\n"))

    (hashtable-set! t "help"    (string->utf8 " [name]
    display available builtins, or help about builtin NAME.

    return success, unless NAME is not found.\n"))

    (hashtable-set! t "history" (string->utf8 " [arg ...]
    ignore arguments, write history to standard output.

    return success.\n"))

    (hashtable-set! t "true"    (hashtable-ref t ":" ""))

    (hashtable-set! t "ulimit" (string->utf8 " [-SHacdefilmnpqrstuvxR] [limit]
    Modify shell resource limits.

    Provides control over the resources available to the shell and processes it creates.

    Options:
      -S        use the `soft' resource limit
      -H        use the `hard' resource limit
      -a        all current limits are reported
      -c        the maximum size of core files created
      -d        the maximum size of a process's data segment
      -e        the maximum scheduling priority (`nice')
      -f        the maximum size of files written by the shell and its children
      -i        the maximum number of pending signals
     [-k        the maximum number of kqueues allocated for this process]
      -l        the maximum size a process may lock into memory
      -m        the maximum resident set size
      -n        the maximum number of open file descriptors
      -p        the pipe buffer size
      -q        the maximum number of bytes in POSIX message queues
      -r        the maximum real-time scheduling priority
      -s        the maximum stack size
      -t        the maximum amount of cpu time in seconds
      -u        the maximum number of user processes
      -v        the size of virtual memory
      -x        the maximum number of file locks
      -R        the maximum time a real-time process can run before blocking

    If LIMIT is given, it is the new value of the specified resource; the
    special LIMIT values `soft', `hard', and `unlimited' stand for the
    current soft limit, the current hard limit, and no limit, respectively.
    Otherwise, the current value of the specified resource is printed.  If
    no option is given, then -f is assumed.

    Return success, unless an invalid option is supplied or an error occurs.\n"))

    (hashtable-set! t "warranty"       (string->utf8 "
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.\n"))


    (lambda () t)))
