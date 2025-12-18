;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file vscreen/all.ss


;; save vhistory to file (vhistory-path hist)
;; return #t if successful, otherwise return #f
(define (vhistory-save hist)
  (let ((path (vhistory-path hist)))
    (and path (vhistory-save-to-path hist path))))


;; save vhistory to file specified by path.
;; return #t if successful, otherwise return #f
(define (vhistory-save-to-path hist path)
  (let ((temp-path (string-append path "." (number->string (pid-get))))
        (remove-temp-path? #f)
        (port #f)
        (success? #f))
    (try
        ;; write to a temporary file "history.txt.PID"
        (set! port (file->port temp-path 'write '(create) 'binary))
        (set! remove-temp-path? #t)
        (vhistory-save-to-port hist port)
        (close-port port)
        (set! port #f)
        ;; atomically rename "history.txt.PID" -> "history.txt"
        (when (eq? (void) (file-rename temp-path path 'catch))
          (set! remove-temp-path? #f)
          (set! success? #t))
      (catch (ex)
        #|
        (let ((port (current-error-port)))
          (put-string port "; error saving ")
          (put-string port temp-path)
          (display-condition ex port)
          (newline port)
          (flush-output-port port))
        |#
        #f))

    (when port
      (close-port port))
    (when remove-temp-path?
      (file-delete temp-path '(catch)))
    success?))


;; save vhistory to specified binary output port.
;; return #t if successful, otherwise return #f
(define (vhistory-save-to-port hist port)
  (vhistory-iterate hist
    (lambda (i lines)
      (vlines-save-to-port lines port))))


;; save vlines to specified binary output port.
;; return #t if successful, otherwise return #f
(define (vlines-save-to-port lines port)
  (and
    (vlines-iterate lines
      (lambda (i line)
        (vline-save-to-port line port)))
    (begin
      (put-u8 port 10) ; returns unspecified value, may be #f
      #t)))


;; save vline to specified binary output port.
;; return #t if successful, otherwise return #f
(define (vline-save-to-port line port)
  (put-bytevector port (string->utf8b (string-replace/char! (vline->string line) #\newline #\nul))))


;; load vhistory from file (vhistory-path hist)
;; return #t if successful, otherwise return #f
(define (vhistory-load! hist)
  (let ((path (vhistory-path hist)))
    (and path (vhistory-load-from-file! hist path))))


;; load vhistory from specified file path.
;; return #t if successful, otherwise return #f
(define (vhistory-load-from-file! hist path)
  (let ((port #f))
    (try
      (dynamic-wind
        (lambda ()
          ;; we do our own buffering in (vhistory-load-from-port)
          (set! port (file->port path 'read '() 'binary (buffer-mode none))))
        (lambda ()
          (vhistory-load-from-port! hist port))
        (lambda ()
          (when port
            (close-port port)
            (set! port #f))))
      (catch (ex)
        #|
        (let ((port (console-error-port)))
          (put-string port "; error loading ")
          (put-string port path)
          (display-condition ex port)
          (newline port)
          (flush-output-port port))
        |#
        #f))))


;; load vhistory from specified binary input port.
;; return #t if successful, otherwise return #f
(define (vhistory-load-from-port! hist port)
  (gbuffer-clear! hist)
  (let* ((bv    (make-bytevector #x10000))
         (start 0)
         (end   0)
         (done? #f))
    (until done?
      (let-values (((lines next-start next-end eof?) (vlines-load-from-port port bv start end)))
        (set! start next-start)
        (set! end next-end)
        (when lines
          (gbuffer-insert-at! hist (gbuffer-length hist) lines))
        (when (or eof? (not lines))
          (set! done? #t))))
    #t))


;; load a vlines from specified binary input port and return it.
;;
;; uses bytevector bv as input buffer, and consumes range [start, end)
;; of bv before reading more data from port.
;;
;; return the created vlines if successful, otherwise return #f
;; as second value, return updated start offset.
;; as third value, return updated end offset.
;; as fourth value, return #t if EOF was reached otherwise return #f
(define (vlines-load-from-port port bv start end)
  (let ((lines (vlines))
        (flag  #f))
    (until flag
      ; (debugf "->   vline-load-from-port start=~s end=~s" start end)
      ; (sleep (make-time 'time-duration 0 1))
      (let-values (((line next-start next-end next-flag) (vline-load-from-port port bv start end)))
        (set! start next-start)
        (set! end next-end)
        (set! flag next-flag)
        ; (debugf "<-  vline-load-from-port line=~s start=~s end=~s flag=~s" line start end flag)
        (when line
          (vlines-insert-at! lines (vlines-length lines) line))))
    (values
      (if (and (eq? flag 'eof) (vlines-empty? lines)) #f lines)
      start
      end
      (eq? flag 'eof))))



;; load a vline from specified binary input port and return it.
;;
;; uses bytevector bv as input buffer, and consumes range [start, end)
;; of bv before reading more data from port.
;;
;; return the created vline if successful, otherwise return #f
;; as second value, return updated start offset.
;; as third value, return updated end offset.
;; as fourth value, return 'nl if newline was found and consumed (it marks the end of vlines),
;;   otherwise return 'eof if EOF was reached
;;   otherwise return #f
(define (vline-load-from-port port bv start end)
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
          ; (debugf "... vline-load-from-port n=~s start=~s end=~s eof?=~s" n start end eof?)
          ; (sleep (make-time 'time-duration 0 1))
          ))
      (until (or done? (fx>=? start end))
        (let ((b (bytevector-u8-ref bv start)))
          ; (debugf "... vline-load-from-port byte=~s" (make-string 1 (integer->char b)))
          (set! start (fx1+ start))
          (case b
            ((0) ; found an escaped newline, it marks the end vline
              (bytespan-insert-right/u8! bspan 10)
              (set! done? #t))
            ((10) ; found a newline, it marks the end of vlines
              (set! nl? #t)
              (set! done? #t))
            (else
              (bytespan-insert-right/u8! bspan b)))))
      (when eof?
        (set! done? #t))
      ; (debugf "... vline-load-from-port bspan=~s start=~s end=~s nl?=~s eof?=~s" (utf8b-bytespan->string bspan) start end nl? eof?)
      )

    (values
      (if (and eof? (bytespan-empty? bspan))
        #f
        (vline (utf8b-bytespan->string bspan)))
      start
      end
      (if nl? 'nl (if (and eof? (fx>=? start end)) 'eof #f)))))
