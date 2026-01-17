;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh shell replacements (0 9 3))
  (export
      ;; the following functions *intentionally* conflict with R6RS and Chez Scheme
      ;; functions with the same names,
      ;;
      ;; because they are intended as replacements
      ;;
      getenv putenv

      get-bytevector-all get-bytevector-n get-bytevector-some get-u8
      put-bytevector                      put-bytevector-some put-u8

      with-input-from-file with-output-to-file)
  (import
    (rename (except (rnrs) with-input-from-file with-output-to-file)
        (get-bytevector-all   r6rs:get-bytevector-all)
        (get-bytevector-n     r6rs:get-bytevector-n)
        (get-bytevector-some  r6rs:get-bytevector-some)
        (get-u8               r6rs:get-u8)
        (put-bytevector       r6rs:put-bytevector)
        (put-u8               r6rs:put-u8))

    (prefix
      (only (chezscheme) put-bytevector-some current-input-port current-output-port)
      chez:)

    (scheme2k io stdio)
    (only (scheme2k posix io)     file->port)
    (only (schemesh shell job)    sh-env-set! sh-env-visibility-ref)
    (only (schemesh shell macros) with-parameterized-resource))

;;; key must be a string.
;;; Return the exported environment value associated with key in current job's environment,
;;    or in (sh-globals) environment if current job is not set.
;;; If no exported environment value is associated with key, return #f
(define (getenv key)
  (let-values (((value visibility) (sh-env-visibility-ref #f key)))
    (if (eq? 'export visibility)
      value
      #f)))


;;; key and value must be strings.
;;; Stores key and value in the exported environment of current job's,
;;;    or in (sh-globals) if current job is not set,
;;; where it is available to the current process (e.g., via getenv) and any spawned processes.
(define (putenv key value)
  (sh-env-set! #f key value 'export))


;;; If binary-input-port, which defaults to (sh-stdin), is at end of file, the eof object is returned.
;;; Otherwise, read (as if with get-u8) all of the bytes available before the port is at end of file
;;; and return a bytevector containing these characters.
;;; The port's position is advanced past the bytes read.
(define get-bytevector-all
  (case-lambda
    (()     (r6rs:get-bytevector-all (sh-stdin)))
    ((port) (r6rs:get-bytevector-all port))))


;;; n must be an exact nonnegative integer.
;;; If binary-input-port, which defaults to (sh-stdin), is at end of file, the eof object is returned.
;;; Otherwise, read (as if with get-u8) as many bytes, up to n, as are available before the port is at end of file,
;;; and returns a new (nonempty) bytevector containing these characters.
;;; The port's position is advanced past the bytes read.
(define get-bytevector-n
  (case-lambda
    ((n)      (r6rs:get-bytevector-n (sh-stdin) n))
    ((port n) (r6rs:get-bytevector-n port n))))


;;; If binary-input-port, which defaults to (sh-stdin), is at end of file, the eof object is returned.
;;; Otherwise, reads (as if with get-u8) at least one byte and possibly more,
;;; and returns a string containing these characters.
;;;
;;; The port's position is advanced past the bytes read.
;;;
;;; The maximum number of bytes read by this operation is implementation-dependent.
;;;
;;; An exception to the "at least one byte" guarantee occurs if the port is in nonblocking mode
;;; (see set-port-nonblocking!) and no input is ready. In this case, an empty bytevector is returned.
(define get-bytevector-some
  (case-lambda
    (()     (r6rs:get-bytevector-some (sh-stdin)))
    ((port) (r6rs:get-bytevector-some port))))


;;; read and return the next character from textual-input-port,
;;    which defaults to (sh-stdin),
;;; or the eof object
(define get-u8
  (case-lambda
    (()     (r6rs:get-u8 (sh-stdin)))
    ((port) (r6rs:get-u8 port))))


;;; If supplied, start and n must be nonnegative exact integers, and the sum of start and n must not exceed the length of bytevector.
;;; If not supplied, start defaults to zero and n defaults to the difference between the length of bytevector and start.
;;;
;;; Write the n bytes of bytevector starting at start to the port, which defaults to (sh-stdout).
;;; Port's position advances by n bytes.
;;;
;;; Return unspecified value.
(define put-bytevector
  (case-lambda
    ((bytevector)               (r6rs:put-bytevector (sh-stdout) bytevector))
    ((port bytevector)          (r6rs:put-bytevector port bytevector))
    ((port bytevector start)    (r6rs:put-bytevector port bytevector start))
    ((port bytevector start n)  (r6rs:put-bytevector port bytevector start n))))


;;; If supplied, start and n must be nonnegative exact integers, and the sum of start and n must not exceed the length of bytevector.
;;; If not supplied, start defaults to zero and n defaults to the difference between the length of bytevector and start.
;;;
;;; This procedure normally writes the n bytes of bytevector starting at start to the port, which defaults to (sh-stdout).
;;; If the port is in nonblocking mode (see set-port-nonblocking!), however,
;;; the number of characters written may be less than n, if the system would have to block to write more characters.
;;;
;;; Port's position advances by the number of bytes actually written.
;;;
;;; Return the number of bytes actually written.
(define put-bytevector-some
  (case-lambda
    ((bytevector)               (chez:put-bytevector-some (sh-stdout) bytevector))
    ((port bytevector)          (chez:put-bytevector-some port bytevector))
    ((port bytevector start)    (chez:put-bytevector-some port bytevector start))
    ((port bytevector start n)  (chez:put-bytevector-some port bytevector start n))))


;;; Write byte u8 to binary-output-port, which defaults to (sh-stdout).
;;; Port's position advances by one byte.
;;;
;;; Return unspecified value.
(define put-u8
  (case-lambda
    ((u8)      (r6rs:put-u8 (sh-stdout) u8))
    ((port u8) (r6rs:put-u8 port u8))))


;; evaluate body ... with (current-output-port) bound to expr,
;; then always restore (current-output-port) and call (close-port expr),
;; even if body ... raises a condition or calls a continuation
(define with-input-from-file
  (case-lambda
    ((path thunk options)
      (with-parameterized-resource ((chez:current-input-port (file->port path 'read options) close-port))
        (thunk)))
    ((path thunk)
      (with-input-from-file path thunk '()))))


;; evaluate body ... with (current-output-port) bound to expr,
;; then always restore (current-output-port) and call (close-port expr),
;; even if body ... raises a condition or calls a continuation
(define with-output-to-file
  (case-lambda
    ((path thunk options)
      (with-parameterized-resource ((chez:current-output-port (file->port path 'write options) close-port))
        (thunk)))
    ((path thunk)
      (with-output-to-file path thunk '()))))

) ; close library
