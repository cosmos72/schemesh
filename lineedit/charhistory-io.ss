;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit charhistory io (0 7 6))
  (export
    charhistory-load!           charhistory-save
    charhistory-load-from-path! charhistory-save-to-path
    charhistory-load-from-port! charhistory-save-to-port)
  (import
    (rnrs)
    (only (chezscheme)                 fx1+ void)
    (only (schemesh bootstrap)         try catch until)
    (only (schemesh containers string) string-replace/char!)
    (only (schemesh containers utf8b)  string->utf8b utf8b-bytespan->string)
    (schemesh containers bytespan)
    (schemesh containers gbuffer)
    (schemesh containers charline)
    (schemesh containers charlines)
    (only (schemesh posix pid)         pid-get)
    (only (schemesh posix dir)         file-delete file-rename ok?)
    (schemesh lineedit charhistory))


;; save charhistory to file (charhistory-path hist)
;; return #t if successful, otherwise return #f
(define (charhistory-save hist)
  (let ((path (charhistory-path hist)))
    (and path (charhistory-save-to-path hist path))))


;; save charhistory to file specified by path.
;; return #t if successful, otherwise return #f
(define (charhistory-save-to-path hist path)
  (call/cc
    (lambda (cont)
      (let ((temp-path (string-append path "." (number->string (pid-get))))
            (remove-temp-path? #f)
            (port #f)
            (success? #f))
        (dynamic-wind
          void
          (lambda ()
            ;; write to a temporary file "history.txt.PID"
            (set! port (open-file-output-port temp-path (file-options no-fail) (buffer-mode block)))
            (set! remove-temp-path? #t)
            (charhistory-save-to-port hist port)
            (close-port port)
            (set! port #f)
            ;; atomically rename "history.txt.PID" -> "history.txt"
            (when (ok? (file-rename temp-path path 'catch))
              (set! remove-temp-path? #f)
              (set! success? #t)))
          (lambda ()
            (when port
              (close-port port)
              (set! port #f))
            (when remove-temp-path?
              (file-delete temp-path 'catch)
              (set! remove-temp-path? #f))
            (cont success?)))))))


;; save charhistory to specified binary output port.
;; return #t if successful, otherwise return #f
(define (charhistory-save-to-port hist port)
  (charhistory-iterate hist
    (lambda (i lines)
      (charlines-save-to-port lines port))))


;; save charlines to specified binary output port.
;; return #t if successful, otherwise return #f
(define (charlines-save-to-port lines port)
  (and
    (charlines-iterate lines
      (lambda (i line)
        (charline-save-to-port line port)))
    (begin
      (put-u8 port 10) ; returns unspecified value, may be #f
      #t)))


;; save charline to specified binary output port.
;; return #t if successful, otherwise return #f
(define (charline-save-to-port line port)
  (put-bytevector port (string->utf8b (string-replace/char! (charline->string line) #\newline #\nul))))



;; load charhistory from file (charhistory-path hist)
;; return #t if successful, otherwise return #f
(define (charhistory-load! hist)
  (let ((path (charhistory-path hist)))
    (and path (charhistory-load-from-path! hist path))))


;; load charhistory from specified file path.
;; return #t if successful, otherwise return #f
(define (charhistory-load-from-path! hist path)
  (let ((port #f))
    (try
      (dynamic-wind
        (lambda ()
          ;; we do our own buffering in (charhistory-load-from-port)
          (set! port (open-file-input-port path (file-options) (buffer-mode none))))
        (lambda ()
          (charhistory-load-from-port! hist port))
        (lambda ()
          (when port
            (close-port port))))
      (catch (ex)
        #f))))


;; load charhistory from specified binary input port.
;; return #t if successful, otherwise return #f
(define (charhistory-load-from-port! hist port)
  (gbuffer-clear! hist)
  (let* ((bv    (make-bytevector #x10000))
         (start 0)
         (end   0)
         (done? #f))
    (until done?
      (let-values (((lines next-start next-end eof?) (charlines-load-from-port port bv start end)))
        (set! start next-start)
        (set! end next-end)
        (when lines
          (gbuffer-insert-at! hist (gbuffer-length hist) lines))
        (when (or eof? (not lines))
          (set! done? #t))))
    #t))


;; load a charlines from specified binary input port and return it.
;;
;; uses bytevector bv as input buffer, and consumes range [start, end)
;; of bv before reading more data from port.
;;
;; return the created charlines if successful, otherwise return #f
;; as second value, return updated start offset.
;; as third value, return updated end offset.
;; as fourth value, return #t if EOF was reached otherwise return #f
(define (charlines-load-from-port port bv start end)
  (let ((lines (charlines))
        (flag  #f))
    (until flag
      ; (debugf "->   charline-load-from-port start=~s end=~s" start end)
      ; (sleep (make-time 'time-duration 0 1))
      (let-values (((line next-start next-end next-flag) (charline-load-from-port port bv start end)))
        (set! start next-start)
        (set! end next-end)
        (set! flag next-flag)
        ; (debugf "<-  charline-load-from-port line=~s start=~s end=~s flag=~s" line start end flag)
        (when line
          (charlines-insert-at/cline! lines (charlines-length lines) line))))
    (values
      (if (and (eq? flag 'eof) (charlines-empty? lines)) #f lines)
      start
      end
      (eq? flag 'eof))))



;; load a charline from specified binary input port and return it.
;;
;; uses bytevector bv as input buffer, and consumes range [start, end)
;; of bv before reading more data from port.
;;
;; return the created charline if successful, otherwise return #f
;; as second value, return updated start offset.
;; as third value, return updated end offset.
;; as fourth value, return 'nl if newline was found and consumed (it marks the end of charlines),
;;   otherwise return 'eof if EOF was reached
;;   otherwise return #f
(define (charline-load-from-port port bv start end)
  (let ((bspan (bytespan))
        (done? #f)
        (eof?  #f)
        (nl?   #f))
    (until done?
      (when (fx>=? start end)
        ; refill bytevector
        (let ((n (get-bytevector-n! port bv 0 (bytevector-length bv))))
          (set! start 0)
          (set! end (if (fixnum? n) n 0))
          (unless (and (fixnum? n) (fx>? n 0))
            (set! eof? #t))
          ; (debugf "... charline-load-from-port n=~s start=~s end=~s eof?=~s" n start end eof?)
          ; (sleep (make-time 'time-duration 0 1))
          ))
      (until (or done? (fx>=? start end))
        (let ((b (bytevector-u8-ref bv start)))
          ; (debugf "... charline-load-from-port byte=~s" (make-string 1 (integer->char b)))
          (set! start (fx1+ start))
          (case b
            ((0) ; found an escaped newline, it marks the end charline
              (bytespan-insert-right/u8! bspan 10)
              (set! done? #t))
            ((10) ; found a newline, it marks the end of charlines
              (set! nl? #t)
              (set! done? #t))
            (else
              (bytespan-insert-right/u8! bspan b)))))
      (when eof?
        (set! done? #t))
      ; (debugf "... charline-load-from-port bspan=~s start=~s end=~s nl?=~s eof?=~s" (utf8b-bytespan->string bspan) start end nl? eof?)
      )

    (values
      (if (and eof? (bytespan-empty? bspan)) #f (string->charline* (utf8b-bytespan->string bspan)))
      start
      end
      (if nl? 'nl (if (and eof? (fx>=? start end)) 'eof #f)))))










) ; close library
