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
    http-url->port)
  (import
    (rnrs)
    (only (chezscheme)                foreign-procedure foreign-sizeof format load-shared-object make-continuation-condition)
    (only (schemesh bootstrap)        assert* assert-not*)
    (only (schemesh containers utf8b) utf8b->string)
    (only (schemesh conversions)      text->bytevector0))


;; wrapper for C http* pointer
(define-record-type http-port
  (fields
    (mutable ctx)) ; C http* pointer
  ;; (nongenerative http-port-7c46d04b-34f4-4046-b5c7-b63753c1be39)
)


(define (http-init solib-path)
  (load-shared-object solib-path)
  (let ((err ((foreign-procedure "http_global_init" () int))))
    (unless (zero? err)
      (let ((str (utf8b->string ((foreign-procedure __collect_safe "http_global_strerror" (int) u8*) err))))
        (raise (string->http-condition 'http-init str))))))


(define http-new
  (let ((dummy       (http-init "/usr/local/lib/libchez_curl.so.0.0.1"))
        (c-http-new  (foreign-procedure __collect_safe "http_new"  () void*)))
    (lambda ()
      (let ((ctx (c-http-new)))
        (assert-not* 'http-new (zero? ctx))
        (make-http-port ctx)))))


(define c-http-del (foreign-procedure __collect_safe "http_del" (void*) void))

(define http-open
  (let ((c-http-open (foreign-procedure "http_open" (void* u8*) int)))
    (lambda (url)
      (let* ((url0 (text->bytevector0 url))
             (p    (http-new))
             (ctx  (http-port-ctx p))
             (err  (c-http-open ctx url0)))
        (unless (zero? err)
          (let ((ex (make-http-condition 'http-open p)))
            (c-http-del ctx)
            (raise ex)))
        p))))


(define http-close
  (let ((c-http-close (foreign-procedure "http_close"  (void*) int)))
    (lambda (p)
      (assert* 'http-close (http-port? p))
      (c-http-close (http-port-ctx p))
      (c-http-del   (http-port-ctx p))
      (http-port-ctx-set! p 0))))


(define http-read
  (let ((c-http-errcode  (foreign-procedure __collect_safe "http_errcode"  (void*) int))
        (c-http-select   (foreign-procedure __collect_safe "http_select"   (void* int) int))
        (c-http-try-read (foreign-procedure                "http_try_read" (void* u8* size_t size_t) size_t))
        (c-size_t-max    (bitwise-not (bitwise-arithmetic-shift-left -1 (fx* 8 (foreign-sizeof 'size_t))))))
    (lambda (p bv start n)
      (assert* 'http-read (http-port? p))
      (let %http-read-loop ((ctx (http-port-ctx p)))
        (unless (zero? (c-http-select ctx 1000))
          (raise-http-condition 'http-read ctx))
        (let ((got (c-http-try-read ctx bv start (+ start n))))
          (cond
            ((zero? got)
              (if (zero? (c-http-errcode ctx))
                ;; timeout waiting for data. try again
                (%http-read-loop ctx)
                (raise-http-condition 'http-read ctx)))
            ((= got c-size_t-max)
              0) ; EOF
            (else
              got)))))))


(define (http-url->port url)
  (let ((p (http-open url)))
    (make-custom-binary-input-port
      url
      (lambda (bv start n) (http-read p bv start n))
      #f ; cannot tell position
      #f ; cannot seek
      (lambda () (http-close p)))))


(define (raise-http-condition who p)
  (raise (make-http-condition who p)))

(define (make-http-condition who p)
  (string->http-condition who (http-error->string p)))

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

        
(define http-error->string
  (let ((c-http-sprint-error (foreign-procedure "http_sprint_error" (void* u8* size_t) size_t)))
    (lambda (p)
      (let* ((maxlen 4000)
             (bv     (make-bytevector maxlen))
             (len    (c-http-sprint-error (http-port-ctx p) bv maxlen)))
        (utf8b->string bv 0 len)))))


)
