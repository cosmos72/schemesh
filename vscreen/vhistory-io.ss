;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file vscreen/all.ss


;; save vhistory to file (vhistory-path hist) or to specified file path
;; return #t if successful, otherwise return #f
(define vhistory-file-save
  (case-lambda
    ((hist path)
      (let ((temp-path (string-append path "." (number->string (pid-get))))
            (remove-temp-path? #f)
            (port #f)
            (success? #f))
        (try
            ;; write to a temporary file "history.txt.PID"
            (set! port (file->port temp-path 'write '(create) 'binary))
            (set! remove-temp-path? #t)
            (vhistory-port-save hist port)
            (close-port port)
            (set! port #f)
            ;; atomically rename "history.txt.PID" -> "history.txt"
            (when (eq? (void) (file-rename temp-path path 'catch))
              (set! remove-temp-path? #f)
              (set! success? #t))
          (catch (ex)
            #|
            (catch-all
              (void)
              (let ((port (current-error-port)))
                (put-string out "\x1b;[1;31m; error saving ")
                (put-string port temp-path)
                (display-condition ex port)
                (put-string out "\x1b;[m\n" port)
                (flush-output-port port))
              (void))
            |#
            #f))
        (when port
          (close-port port))
        (when remove-temp-path?
          (file-delete temp-path '(catch)))
        success?))
    ((hist)
      (let ((path (vhistory-path hist)))
        (and path (vhistory-file-save hist path))))))


;; save vhistory to specified binary output port.
;; return #t if successful, otherwise return #f
(define (vhistory-port-save hist port)
  (vhistory-iterate hist
    (lambda (i lines)
      (vlines-port-save lines port))))


;; save vlines to specified binary output port.
;; return #t if successful, otherwise return #f
(define (vlines-port-save lines port)
  (and
    (vlines-iterate lines
      (lambda (i line)
        (vline-port-save line port)))
    (begin
      (put-u8 port 10) ; returns unspecified value, may be #f
      #t)))


;; save vline to specified binary output port.
;; return #t if successful, otherwise return #f
(define (vline-port-save line port)
  (put-bytevector port (string->utf8b (string-replace/char! (vline->string line) #\newline #\x0))))


;; load vhistory from file (vhistory-path hist) or from specified file path
;; return #t if successful, otherwise return #f
(define vhistory-file-load!
  (case-lambda
    ((hist path)
      (let ((port #f))
        (try
          (dynamic-wind
            (lambda ()
              ;; we do our own buffering in (vhistory-port-load!)
              (set! port (file->port path 'read '() 'binary (buffer-mode none))))
            (lambda ()
              (vhistory-port-load! hist port))
            (lambda ()
              (when port
                (close-port port)
                (set! port #f))))
          (catch (ex)
            #|
            (catch-all
              void
              (let ((port (console-error-port)))
                (put-string out "\x1b;[1;31m; error loading ")
                (put-string port path)
                (display-condition ex port)
                (put-string out "\x1b;[m\n")
                (flush-output-port port))
              (void))
            |#
            #f))))
    ((hist)
      (let ((path (vhistory-path hist)))
        (and path (vhistory-file-load! hist path))))))


;; load vhistory from specified binary input port.
;; return #t if successful, otherwise return #f
(define (vhistory-port-load! hist port)
  (gbuffer-clear! hist)
  (let* ((bv    (make-bytevector #x10000))
         (start 0)
         (end   0)
         (done? #f))
    (until done?
      (let-values (((lines next-start next-end eof?) (vlines-port-load port bv start end)))
        (set! start next-start)
        (set! end next-end)
        (when lines
          (let* ((hist-n (gbuffer-length hist))
                 (prev-lines (and (not (fxzero? hist-n)) (gbuffer-ref hist (fx1- hist-n)))))
            ;; remove duplicates
            (unless (and prev-lines (vlines-equal/chars? lines prev-lines))
              (gbuffer-insert-at! hist (gbuffer-length hist) lines))))
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
(define (vlines-port-load port bv start end)
  (let ((lines (vlines))
        (flag  #f))
    (until flag
      ; (debugf "->   vline-port-load start=~s end=~s" start end)
      ; (sleep (make-time 'time-duration 0 1))
      (let-values (((line next-start next-end next-flag) (vline-port-load port bv start end)))
        (set! start next-start)
        (set! end next-end)
        (set! flag next-flag)
        ; (debugf "<-  vline-port-load line=~s start=~s end=~s flag=~s" line start end flag)
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
(define (vline-port-load port bv start end)
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
          ; (debugf "... vline-port-load n=~s start=~s end=~s eof?=~s" n start end eof?)
          ; (sleep (make-time 'time-duration 0 1))
          ))
      (until (or done? (fx>=? start end))
        (let ((b (bytevector-u8-ref bv start)))
          ; (debugf "... vline-port-load byte=~s" (make-string 1 (integer->char b)))
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
      ; (debugf "... vline-port-load bspan=~s start=~s end=~s nl?=~s eof?=~s" (utf8b-bytespan->string bspan) start end nl? eof?)
      )

    (values
      (if (and eof? (bytespan-empty? bspan))
        #f
        (vline (utf8b-bytespan->string bspan)))
      start
      end
      (if nl? 'nl (if (and eof? (fx>=? start end)) 'eof #f)))))
