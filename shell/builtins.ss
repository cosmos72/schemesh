;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh shell builtins (0 7 5))
  (export sh-builtins sh-builtins-help sh-find-builtin sh-exception-handler
          sh-echo sh-false sh-help sh-history sh-repl-args sh-repl-args-linectx sh-test sh-true)
  (import
    (rnrs)
    (only (chezscheme)        console-error-port debug debug-condition debug-on-exception
                                 display-condition reset-handler void)
    (only (schemesh bootstrap)     raise-errorf sh-make-thread-parameter sh-version while)
    (schemesh containers bytespan)
    (only (schemesh containers charlines) charlines-iterate)
    (only (schemesh containers gbuffer)   gbuffer-iterate)
    (only (schemesh containers hashtable) hashtable-iterate)
    (only (schemesh containers misc)      list-iterate)
    (only (schemesh containers sort)      vector-sort*!)
    (only (schemesh containers span)      span span-insert-back! span-iterate vector->span*)
    (only (schemesh containers string)    assert-string-list? string-contains-only-decimal-digits?)
    (schemesh containers utf8b utils)
    (only (schemesh posix fd)             fd-write)
    (schemesh lineedit charhistory)
    (only (schemesh lineedit linectx)     linectx? linectx-history linectx-wbuf)
    (only (schemesh lineedit lineedit)    lineedit-display-table lineedit-flush)
    (only (schemesh shell fds)            sh-fd-stdout sh-fd-stderr))


;; copy-pasted from shell/builtins2.ss
;;
;; write contents of bytespan bsp to file descriptor fd,
;; then clear bytespan bsp
;;
;; returns (void)
(define (fd-write/bspan! fd bsp)
  ; TODO: loop on short writes and call sh-consume-sigchld
  (fd-write fd (bytespan-peek-data bsp)
            (bytespan-peek-beg bsp) (bytespan-peek-end bsp))
  (bytespan-clear! bsp))


;; thread parameter (sh-repl-args) must be empty or a list
;;   (parser eval-func linectx repl-init-file-path repl-quit-file-path)
;; containing arguments of current call to (sh-repl) or (sh-repl*)
(define sh-repl-args
  (sh-make-thread-parameter
    '()
    (lambda (args)
      (unless (list? args)
        (raise-errorf 'sh-repl-args "invalid value, must be a list: " args))
      args)))


;; return the linectx contained in thread parameter (sh-repl-args),
;; or #f if not present.
(define (sh-repl-args-linectx)
  (let ((repl-args (sh-repl-args)))
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
        (fd   (sh-fd-stdout)))
    (do ((tail args (cdr tail)))
        ((null? tail))
      (unless (eq? args tail)
        (bytespan-insert-back/u8! wbuf 32)) ; space
      (bytespan-insert-back/string! wbuf (car tail))
      (when (fx>=? (bytespan-length wbuf) 4096)
        (fd-write/bspan! fd wbuf)))
    (bytespan-insert-back/u8! wbuf 10) ; newline
    (fd-write/bspan! fd wbuf))
  (void))


;; implementation of "echo0" builtin, writes user-specified arguments to file descriptor 1
;; separator between arguments is #\nul
;; terminating character after arguments is #\nul
(define (sh-echo0 . args)
  (let ((wbuf (make-bytespan 0))
        (fd   (sh-fd-stdout)))
    (do ((tail args (cdr tail)))
        ((null? tail))
      (bytespan-insert-back/string! wbuf (car tail))
      (bytespan-insert-back/u8! wbuf 0) ; #\nul
      (when (fx>=? (bytespan-length wbuf) 4096)
        (fd-write/bspan! fd wbuf)))
    (fd-write/bspan! fd wbuf))
  (void))


;; implementation of "false" builtin, always exits with failure exit status '(exited . 1)
(define (sh-false . ignored-args)
  '(exited . 1))


;; implementation of "help" builtin, display general help or help for specified builtin.
(define sh-help
  (case-lambda
    (()
      (let* ((lctx (sh-repl-args-linectx))
             (wbuf (linectx-wbuf lctx)))
        (bytespan-insert-back/string! wbuf "schemesh version")
        (let ((version (sh-version)))
          (do ((l version (cdr l)))
              ((null? l))
            (bytespan-insert-back/u8! wbuf (if (eq? l version) 32 46))
            (bytespan-display-back/fixnum! wbuf (car l))))
        (bytespan-insert-back/string! wbuf "
Copyright (C) 2023-2035 Massimiliano Ghilardi <https://github.com/cosmos72/schemesh>

  schemesh comes with ABSOLUTELY NO WARRANTY; for details type 'help warranty'.
  This is free software, and you are welcome to redistribute it
  under certain conditions; type 'help copyright' for details.

Type 'help' to display this text. Type 'help name' for help about the builtin 'name'.
The following names are recognized as builtins:\n\n")

        (let ((names (hashtable-keys (sh-builtins))))
          (vector-sort*! string<? names)
          (lineedit-display-table lctx (vector->span* names)))
        (lineedit-flush lctx)
        (void)))
    ((name)
      (let ((wbuf (bytespan))
            (help-bvector (hashtable-ref (sh-builtins-help) name #f)))
        (if help-bvector
          (begin
            (bytespan-insert-back/string!  wbuf name)
            (bytespan-insert-back/bvector! wbuf help-bvector)
            (fd-write/bspan! (sh-fd-stdout) wbuf)
            (void))
          (begin
            (bytespan-insert-back/string! wbuf "schemesh: help: no help for builtin '")
            (bytespan-insert-back/string! wbuf name)
            (bytespan-insert-back/string! wbuf "'. Try 'help' or 'help help'.\n")
            (fd-write/bspan! (sh-fd-stdout) wbuf)
            '(exited . 1)))))))




;; implementation of "history" builtin, display previous commands saved to history.
(define sh-history
  (case-lambda
    (()
      (sh-history (sh-repl-args-linectx)))
    ((lctx)
      (let ((fd   (sh-fd-stdout)))
        ; (debugf "sh-history ~s" lctx)
        (if (linectx? lctx)
          (let ((wbuf (make-bytespan 0)))
            (gbuffer-iterate (linectx-history lctx)
              (lambda (i lines)
                (bytespan-insert-back/u8!      wbuf 32) ; space
                (bytespan-display-back/fixnum! wbuf i)
                (bytespan-insert-back/u8!      wbuf 9) ; tab
                (charlines-iterate lines
                  (lambda (j line)
                    (bytespan-insert-back/cbuffer! wbuf line)))
                (bytespan-insert-back/u8! wbuf 10) ; newline
                (when (fx>=? (bytespan-length wbuf) 4096)
                  (fd-write/bspan! fd wbuf))))
            (fd-write/bspan! fd wbuf)
            (void)) ; return (void), means builtin exited successfully
          '(exited . 1))))))


;; implementation of "test" builtin, exits with user-specified exit status
(define (sh-test . args)
  ; (debugf "sh-test ~s" args)
  (if (pair? args)
    (let ((arg (car args)))
      (cond
        ((integer? arg)
          (if (fxzero? arg)
            (void) ; '(exited . 0) is always abbreviated to (void)
            (cons 'exited arg)))
        ((and (string? arg) (string-contains-only-decimal-digits? arg))
          (let ((num (string->number arg)))
            (if (zero? num)
              (void) ; '(exited . 0) is always abbreviated to (void)
              (cons 'exited num))))
        ((and (pair? arg) (symbol? (car arg)) (or (integer? (cdr arg)) (symbol? (cdr arg))))
          arg)
        (#t
          '(exited . 1))))
    '(exited . 1)))


;; implementation of "true" builtin, always exits successfully i.e. with exit status (void)
(define (sh-true . ignored-args)
  (void))


;; the "echo" builtin: write arguments to (sh-fd-stdout)
;; separating each pair with a #\space and terminating them with a #\newline
;;
;; As all builtins do, must return job status.
(define (builtin-echo job prog-and-args options)
  (assert-string-list? 'builtin-echo prog-and-args)
  (apply sh-echo (cdr prog-and-args)))


;; the "echo0" builtin: write arguments to (sh-fd-stdout)
;; terminating each one with a #\nul
;;
;; As all builtins do, must return job status.
(define (builtin-echo0 job prog-and-args options)
  (assert-string-list? 'builtin-echo0 prog-and-args)
  (apply sh-echo0 (cdr prog-and-args)))


;; the "false" builtin: return '(exited . 1)
;;
;; As all builtins do, must return job status.
(define (builtin-false job prog-and-args options)
  (assert-string-list? 'builtin-false prog-and-args)
  (sh-false))


;; the "help" builtin: write help, or help for specified builtin, to (sh-fd-stdout).
;;
;; As all builtins do, must return job status.
(define (builtin-help job prog-and-args options)
  (assert-string-list? 'builtin-help prog-and-args)
  (if (null? (cdr prog-and-args))
    (sh-help)
    (sh-help (cadr prog-and-args))))


;; the "history" builtin: write current history to (sh-fd-stdout).
;;
;; As all builtins do, must return job status.
(define (builtin-history job prog-and-args options)
  (assert-string-list? 'builtin-history prog-and-args)
  (sh-history))


;; the "test" builtin: return specified exit status,
;; which must be a non-empty string containing only decimal digits.
;;
;; As all builtins do, must return job status.
(define (builtin-test job prog-and-args options)
  (assert-string-list? 'builtin-test prog-and-args)
  (apply sh-test (cdr prog-and-args)))


;; the "true" builtin: return (void)
;;
;; As all builtins do, must return job status.
(define (builtin-true job prog-and-args options)
  (assert-string-list? 'builtin-true prog-and-args)
  (sh-true))


;; given a command line prog-and-args i.e. a list of strings,
;; extract the first string and return the corresponding builtin.
;; Return #f if no corresponding builtin is found.
;; Return builtin-true if prog-and-args is the empty list.
(define (sh-find-builtin prog-and-args)
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
    (hashtable-set! t "test"    builtin-test)
    (hashtable-set! t "true"    builtin-true)
    (lambda () t)))


;; function returning the global hashtable name -> help text for builtin.
;; Each help text must be a bytevector starting with byte 32 i.e. space and ending with byte 10 i.e. newline.
(define sh-builtins-help
  (let ((t (make-hashtable string-hash string=?)))

    (hashtable-set! t ":"       (string->utf8 " [arg ...]
    ignore arguments. return success i.e. (void).\n"))

    (hashtable-set! t "copyright" (string->utf8 "

schemesh, a fusion between interactive Unix shell and Chez Scheme REPL.
Copyright (C) 2023-2035 Massimiliano Ghilardi <https://github.com/cosmos72/schemesh>

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
    ignore arguments. return failure i.e. '(exited . 1).\n"))

    (hashtable-set! t "help"    (string->utf8 " [name]
    display available builtins, or help about builtin NAME.

    return success, unless NAME is not found.\n"))

    (hashtable-set! t "history" (string->utf8 " [arg ...]
    ignore arguments, write history to standard output.

    return success.\n"))

    (hashtable-set! t "test"   (string->utf8 " [int ...]
    return INT value specified as first argument, or failure i.e. '(exited . 1) if no arguments.\n"))

    (hashtable-set! t "true"    (hashtable-ref t ":" ""))

    (hashtable-set! t "warranty"       (string->utf8 "
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.\n"))


    (lambda () t)))

) ; close library
