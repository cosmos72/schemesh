;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; scheme wrapper around libcurl

(library (schemesh port http (0 9 2))
  (export
    http-init http-open http-read http-close http-url->port)
  (import
    (rnrs)
    (only (chezscheme)                foreign-procedure foreign-sizeof format
                                      load-shared-object make-continuation-condition)
    (only (schemesh bootstrap)        assert* assert-not* check-interrupts)
    (only (schemesh containers utf8b) utf8b->string)
    (only (schemesh conversions)      text->bytevector0))


;; wrapper for C http* pointer
(define-record-type http
  (fields
    (mutable addr)) ; C http* pointer
  (nongenerative http-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define c-http-new      #f)
(define c-http-del      #f)
(define c-http-open     #f)
(define c-http-errcode  #f)
(define c-http-select   #f)
(define c-http-try-read #f)
(define c-http-sprint-error #f)
(define c-size_t-max    (bitwise-not (bitwise-arithmetic-shift-left -1 (fx* 8 (foreign-sizeof 'size_t)))))


(define http-init
  (case-lambda
    (()
      (http-init "/usr/local/lib/libchez_curl.so.0"))
    ((path)
      (load-shared-object path)
      (let ((err ((foreign-procedure "http_global_init" () int))))
        (unless (zero? err)
          (let ((str (utf8b->string ((foreign-procedure __collect_safe "http_global_strerror" (int) u8*) err))))
            (raise (string->http-condition 'http-init str)))))
      (set! c-http-new      (foreign-procedure __collect_safe "http_new"  () void*))
      (set! c-http-del      (foreign-procedure __collect_safe "http_del" (void*) void))
      (set! c-http-open     (foreign-procedure                "http_open" (void* u8*) int))
      (set! c-http-errcode  (foreign-procedure __collect_safe "http_errcode"  (void*) int))
      (set! c-http-select   (foreign-procedure __collect_safe "http_select"   (void* int) int))
      (set! c-http-try-read (foreign-procedure                "http_try_read" (void* u8* size_t size_t) size_t))
      (set! c-http-sprint-error (foreign-procedure            "http_sprint_error" (void* u8* size_t) size_t)))))


(define (http-new)
  (unless c-http-new
    (http-init))
  (let ((addr (c-http-new)))
    (assert-not* 'http-new (zero? addr))
    (make-http addr)))


(define (http-open url)
  (let* ((url0 (text->bytevector0 url))
         (ctx  (http-new))
         (addr (http-addr ctx))
         (err  (c-http-open addr url0)))
    (unless (zero? err)
      (let ((ex (make-http-condition 'http-open ctx)))
        (http-close ctx)
        (raise ex)))
    ctx))


(define (http-close ctx)
  (assert* 'http-close (http? ctx))
  (c-http-del (http-addr ctx))
  (http-addr-set! ctx 0))


(define (http-read ctx bv start n)
  (assert* 'http-read (http? ctx))
  (let %http-read-loop ((addr (http-addr ctx)))
    (check-interrupts)
    (unless (zero? (c-http-select addr 500))
      (raise-http-condition 'http-read ctx))
    (check-interrupts)
    (let ((got (c-http-try-read addr bv start (+ start n))))
      (cond
        ((zero? got)
          (if (zero? (c-http-errcode addr))
            ;; timeout waiting for data. try again
            (%http-read-loop addr)
            (raise-http-condition 'http-read ctx)))
        ((= got c-size_t-max)
          0) ; EOF
        (else
          got)))))


(define (http-url->port url)
  (let ((ctx (http-open url)))
    (make-custom-binary-input-port
      url
      (lambda (bv start n) (http-read ctx bv start n))
      #f ; cannot tell position
      #f ; cannot seek
      (lambda () (http-close ctx)))))


(define (raise-http-condition who ctx)
  (raise (make-http-condition who ctx)))

(define (make-http-condition who ctx)
  (string->http-condition who (http-error->string ctx)))


(define (http-error->string ctx)
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

)
