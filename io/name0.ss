;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs


(library (scheme2k io name0 (1 0 0))
  (export
    make-name0-reader name0-reader name0-reader-port)
  (import
    (rnrs)
    (only (chezscheme)                   fx1+ get-bytevector-some!)
    (only (scheme2k bootstrap)           assert*)
    (only (scheme2k io obj)              reader)
    (only (scheme2k containers bytespan) bytespan-clear! bytespan-delete-left! bytespan-index bytespan-length
                                         bytespan-peek-data bytespan-peek-end
                                         bytespan-reserve-right! bytespan-resize-right! make-bytespan)
    (only (scheme2k containers utf8b)    utf8b-bytespan->string))



;; Reader that wraps a binary input port, and reads nul-terminated names from it.
(define-record-type (name0-reader %make-name0-reader name0-reader?)
  (parent reader)
  (fields
    (mutable port)  ;; #f or binary input port
    rbuf)           ;; bytespan read buffer
  (protocol
    (lambda (args->new)
      (lambda (port close-port?)
         ;; there's no optimized mechanism to skip an element:
         ;; we must read one or more elements from wrapped reader,
         ;; then sequentially call pred on each one
         ;; until either (pred element cache) returns truish, or wrapped reader is exhausted
         ((args->new %name0-reader-get #f (and close-port? %name0-reader-port-close))
            port (make-bytespan 0)))))
  (nongenerative %name0-reader-7c46d04b-34f4-4046-b5c7-b63753c1be40))


;; Create and return a name0-reader that wraps a binary input port.
;;
;; At each call to (reader-get) reads a nul-terminated bytevector from the wrapped port,
;; removes the final \x0, converts the bytevector from UTF-8b to string, and returns a plist
;; ('name "PARSED-STRING")
;;
;; Note: as per reader contract, by default closing a filter-reader does NOT close
;; the wrapped port, because it is a pre-existing, borrowed resource passed to the constructor.
;;
;; If a name0-reader should take ownership of the wrapped port passed to the constructor,
;; then pass a truish value as the optional argument close-port?
(define make-name0-reader
  (case-lambda
    ((port close-port?)
      (assert* 'make-name0-reader (port? port))
      (assert* 'make-name0-reader (binary-port? port))
      (assert* 'make-name0-reader (input-port? port))
      (%make-name0-reader port close-port?))
    ((port)
      (make-name0-reader port #f))))



;; read more data from port, and return number of bytes read, or #f on EOF.
(define (%name0-reader-read-some rx port rbuf)
  (bytespan-reserve-right! rbuf (fxmax 4096 (fx* 2 (bytespan-length rbuf))))
  (let* ((len (bytespan-length rbuf))
         (bv  (bytespan-peek-data rbuf))
         (end (bytespan-peek-end rbuf))
         (got (get-bytevector-some! port bv end (fx- (bytevector-length bv) end))))
    (cond
      ((and (fixnum? got) (fx>? got 0))
        (bytespan-resize-right! rbuf (fx+ len got))
        got)
      (else
        (%name0-reader-port-close rx)
        #f))))


;; read more data from port until a #\x0 is found, and return its position.
;; return #f if EOF is reached without finding #\x0
(define (%name0-reader-read-until-nul-or-eof rx port rbuf minpos)
  (let ((len (bytespan-length rbuf)))
    (or (bytespan-index rbuf minpos len 0)
        (and port
             (%name0-reader-read-some rx port rbuf)
             (%name0-reader-read-until-nul-or-eof rx port rbuf len)))))


;; called by (reader-get) (reader-skip)
(define (%name0-reader-get rx)
  (let* ((rbuf (name0-reader-rbuf rx))
         (nul  (%name0-reader-read-until-nul-or-eof rx (name0-reader-port rx) rbuf 0))
         (len  (or nul (bytespan-length rbuf))))
    (cond
      (nul ; found nul-terminated fragment
        (let ((str (utf8b-bytespan->string rbuf 0 nul)))
          (bytespan-delete-left! rbuf (fx1+ nul))
          (values (list 'name str) #t)))
      ((fx>? len 0) ; last fragment is not nul-terminated
        (let ((str (utf8b-bytespan->string rbuf)))
          (bytespan-clear! rbuf)
          (values (list 'name str) #t)))
      (else ; EOF
        (values #f #f)))))


(define (%name0-reader-port-close rx)
  (let ((port (name0-reader-port rx)))
    (when port
      (name0-reader-port-set! rx #f)
      (close-port port))))


) ; close library