;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; scheme wrapper around libcurl

(library (scheme2k port http (0 9 3))
  (export
    http-init http-open http-error-string http-read http-close http->port http-url->port)
  (import
    (rnrs)
    (only (chezscheme)                foreign-procedure foreign-sizeof format load-shared-object
                                      make-continuation-condition record-writer)
    (only (scheme2k bootstrap)        assert* assert-not* check-interrupts)
    (only (scheme2k containers utf8b) utf8b->string)
    (only (scheme2k conversions)      text->bytevector0 text->string)
    (only (scheme2k posix io)         port->utf8b-port))


;; wrapper for C http* pointer
(define-record-type http
  (fields
    (mutable addr)   ; C http* pointer
    (immutable url)) ; string
  (nongenerative http-7c46d04b-34f4-4046-b5c7-b63753c1be40))


(define c-http-new      #f)
(define c-http-del      #f)
(define c-http-open     #f)
(define c-http-close    #f)
(define c-http-errcode  #f)
(define c-http-select   #f)
(define c-http-try-read #f)
(define c-http-sprint-error #f)
(define c-http-eof      (bitwise-not (bitwise-arithmetic-shift-left -1 (fx* 8 (foreign-sizeof 'size_t))))) ;; (size_t)-1


(define http-init
  (case-lambda
    (()
      (http-init "/usr/local/lib/scheme2k/libscheme2k_http_c_0.9.3.so"))
    ((path)
      (load-shared-object path)
      (let ((err ((foreign-procedure "http_global_init" () int))))
        (unless (zero? err)
          (let ((str (utf8b->string ((foreign-procedure __collect_safe "http_global_strerror" (int) u8*) err))))
            (raise (string->http-condition 'http-init str)))))
      (set! c-http-new      (foreign-procedure __collect_safe "http_new"  () void*))
      (set! c-http-del      (foreign-procedure __collect_safe "http_del" (void*) void))
      (set! c-http-open     (foreign-procedure                "http_open" (void* u8*) int))
      (set! c-http-close    (foreign-procedure                "http_close" (void*) void))
      (set! c-http-errcode  (foreign-procedure __collect_safe "http_errcode"  (void*) int))
      (set! c-http-select   (foreign-procedure __collect_safe "http_select"   (void* int) int))
      (set! c-http-try-read (foreign-procedure                "http_try_read" (void* u8* size_t size_t) size_t))
      (set! c-http-sprint-error (foreign-procedure            "http_sprint_error" (void* u8* size_t) size_t)))))


(define (http-new url)
  (unless c-http-sprint-error ; last foreign-procedure loaded by (http-init)
    (http-init))
  (let ((addr (c-http-new)))
    (assert-not* 'http-new (zero? addr))
    (make-http addr (text->string url))))


;; create and return an http context for reading from url
(define (http-open url)
  (let* ((url0 (text->bytevector0 url))
         (ctx  (http-new url))
         (addr (http-addr ctx))
         (err  (c-http-open addr url0)))
    (unless (zero? err)
      (let ((ex (make-http-condition 'http-open ctx)))
        (http-close ctx)
        (raise ex)))
    ctx))


;; clone an http context and deallocate its libcurl resources
(define (http-close ctx)
  (assert* 'http-close (http? ctx))
  ;; release libcurl resources, and also deallocate C http* addr
  (c-http-del (http-addr ctx))
  (http-addr-set! ctx 0))


;; read up to n bytes from an http context and write them into bytevector bv, starting at position start
(define (http-read ctx bv start n)
  (assert* 'http-read (http? ctx))
  (let %http-read-loop ((addr (http-addr ctx)))
    (check-interrupts)
    (let ((got (c-http-try-read addr bv start (+ start n))))
      (cond
        ((zero? got)
          (if (zero? (c-http-errcode addr))
            (begin
              ;; no available data. select() then try again
              (unless (zero? (c-http-select addr 500))
                (raise-http-condition 'http-read ctx))
              (%http-read-loop addr))
            (raise-http-condition 'http-read ctx)))
        ((= got c-http-eof) ; EOF
          (unless (zero? addr)
            ;; release libcurl resources.
            ;; do NOT deallocate C http* addr, it contains error codes.
            (c-http-close addr))
          0) ; EOF
        (else
          got)))))


(define validate-transcoder-sym
  (let ((allowed-transcoder-syms '(binary text utf8b)))
    (lambda (who transcoder-sym)
      (assert* who (memq transcoder-sym allowed-transcoder-syms)))))


;; create and return a binary or textual input port that wraps an http context
;; (which internally uses libcurl)
(define http->port
  (case-lambda
    ((ctx transcoder-sym)
      (assert* 'http->port (http? ctx))
      (validate-transcoder-sym 'http->port transcoder-sym)
      (let ((p (make-custom-binary-input-port
                (http-url ctx)
                (lambda (bv start n) (http-read ctx bv start n))
                #f ; cannot tell position
                #f ; cannot seek
                (lambda () (http-close ctx)))))
        (if (eq? transcoder-sym 'binary)
          p
          (port->utf8b-port p))))
    ((ctx)
      (http->port ctx 'text))))


;; create and return a binary or textual input port that connects to specified HTTP or HTTPS url and reads from it.
;; Internally uses libcurl.
;;
;; Arguments:
;;   mandatory url            must be a bytevector, string, bytespan or charspan
;;   optional transcoder-sym  must be one of: 'binary 'text 'utf8b and defaults to 'text
(define http-url->port
  (case-lambda
    ((url transcoder-sym)
      (validate-transcoder-sym 'http-url->port transcoder-sym)
      (http->port (http-open url) transcoder-sym))
    ((url)
      (http-url->port url 'text))))


(define (raise-http-condition who ctx)
  (raise (make-http-condition who ctx)))

(define (make-http-condition who ctx)
  (string->http-condition who (http-error-string ctx)))

;; return current error, converted to string
(define (http-error-string ctx)
  (let* ((maxlen 4000)
         (bv     (make-bytevector maxlen))
         (len    (c-http-sprint-error (http-addr ctx) bv maxlen)))
    (utf8b->string bv 0 len)))


(define (string->http-condition who str)
  (call/cc
    (lambda (k)
      (condition
        (make-error)
        (make-continuation-condition k)
        (make-non-continuable-violation)
        (make-who-condition (if (symbol? who) who (format #f "~s" who)))
        ;; (make-format-condition)
        (make-message-condition str)))))


(record-writer (record-type-descriptor http)
  (lambda (ctx port writer)
    (display "#<http " port)
    (display (http-url ctx) port)
    (display ">" port)))

)
