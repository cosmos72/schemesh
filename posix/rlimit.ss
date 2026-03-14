;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k posix rlimit (1 0 0))
  (export rlimit-keys rlimit-all rlimit-ref rlimit-set!
          (rename (make-rlimit rlimit) (rlimit rlimit-type))
          rlimit? rlimit-name rlimit-soft rlimit-hard rlimit-name-set! rlimit-soft-set! rlimit-hard-set!)
  (import
    (rnrs)
    (only (chezscheme)                 foreign-procedure fx1+ record-writer vector->immutable-vector void)
    (only (scheme2k bootstrap)         assert* raise-errorf)
    (only (scheme2k containers span)   make-span span-set!)
    (only (scheme2k posix fd)          raise-c-errno))


(define c-errno-einval ((foreign-procedure "c_errno_einval" () int)))


(define-record-type rlimit
  (fields
    (mutable name)
    (mutable soft)
    (mutable hard))
  (nongenerative rlimit-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define rlimit-keys
  (let ((keys (vector->immutable-vector ;; keys order must match C rlimit_keys[]
                '#(coredump-size data-size nice file-size pending-signals
                   locked-memory-size memory-size open-files pipe-size msgqueue-size
                   realtime-priority stack-size cpu-time user-processes
                   virtual-memory-size file-locks realtime-nonblocking-time))))
    (lambda ()
      keys)))


(define rlimit-key->ckey
  (let ((htable (make-eq-hashtable))
        (keys   (rlimit-keys))
        (ckeys  ((foreign-procedure "c_rlimit_keys" () ptr))))
    (assert* 'rlimit-key->ckey (fx=? (vector-length keys) (vector-length ckeys)))
    (do ((i 0 (fx1+ i))
         (n (vector-length keys)))
        ((fx>=? i n))
      (hashtable-set! htable (vector-ref keys i) (vector-ref ckeys i)))
    (lambda (who key)
      (let ((ckey (hashtable-ref htable key #t)))
        (when (eq? #t ckey)
          ;; throw if key is not among (rlimit-keys)
          (raise-errorf who "~s is not one of the known rlimit resources keys ~s" key htable))
        ckey))))


;; get soft and hard limits for specified resource with getrlimit()
;; and return rlimit object containing them:
;; hard and soft fields will be #f if specified resource is not supported on current OS
;; or will be 'unlimited if resource has no limit
(define rlimit-ref
  (let ((c-rlimit-get (foreign-procedure "c_rlimit_get" (int int) ptr)))
    (lambda (key)
      ;; (debugf "(rlimit-ref ~s ~s)" hard-soft key)
      (let* ((ckey (rlimit-key->ckey 'rlimit-ref key))
             (soft (and ckey (c-rlimit-get 0 ckey)))
             (hard (and ckey (c-rlimit-get 1 ckey))))
        ;; (debugf "(rlimit-ref ~s) -> soft = ~s, hard = ~s" key soft hard)
        (cond
          ((and (not (boolean? soft)) (< soft 0))
            (raise-c-errno 'rlimit-ref 'getrlimit soft 'soft key))
          ((and (not (boolean? hard)) (< hard 0))
            (raise-c-errno 'rlimit-ref 'getrlimit hard 'hard key))
          (else
            (make-rlimit key (if (eq? #t soft) 'unlimited soft)
                             (if (eq? #t hard) 'unlimited hard))))))))


;; get all soft and hard resource limits with getrlimit()
;; and return them as a span of rlimit objects
(define (rlimit-all)
  (let* ((keys (rlimit-keys))
         (n    (vector-length keys))
         (ret  (make-span n)))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i n) ret)
        (let ((key (vector-ref keys i)))
          (span-set! ret i (rlimit-ref key))))))


;; set resource limits with setrlimit()
;;
;; arguments must be either:
;;   a single rlimit object, where fields soft and hard must be one of:
;;     an exact unsigned-64 number
;;     the symbol 'unlimited
;;     #f, indicating that the corresponding soft or hard limit must NOT be set
;;
;;   three arguments
;;     key: a symbol among the ones returned by (rlimit-keys)
;;     soft: the new soft limit to set, must be one of:
;;       an exact unsigned-64 number
;;       the symbol 'unlimited
;;       #f, indicating that the corresponding soft limit must NOT be set
;;     hard: the new hard limit to set, must be one of:
;;       an exact unsigned-64 number
;;       the symbol 'unlimited
;;       #f, indicating that the corresponding soft hard must NOT be set
;;
;; returns (void), or #f if specified resource is not supported on current OS
(define rlimit-set!
  (case-lambda
    ((key soft hard)
      (let* ((ckey (rlimit-key->ckey 'rlimit-set! key))
             (ret1 (and ckey (if soft (%rlimit-set! 'soft key ckey soft) (void))))
             (ret2 (and ckey (if hard (%rlimit-set! 'hard key ckey hard) (void)))))
        (and ret1 ret2)))
    ((rl)
      (assert* 'rlimit-set! (rlimit? rl))
      (rlimit-set! (rlimit-name rl) (rlimit-soft rl) (rlimit-hard rl)))))


;; implementation of (rlimit-set!)
(define %rlimit-set!
  (let ((c-rlimit-set (foreign-procedure "c_rlimit_set" (int int ptr) int)))
    (lambda (hard-soft key ckey value)
      (unless (eq? 'unlimited value)
        (assert* 'rlimit-set! (exact? value))
        (assert* 'rlimit-set! (integer? value))
        (assert* 'rlimit-set! (<= 0 value #xFFFFFFFFFFFFFFFF)))
      (let ((ret (and
                   ckey
                   (c-rlimit-set
                     (if (eq? hard-soft 'hard) 1 0)
                     ckey
                     (if (eq? 'unlimited value) #t value)))))
        ;; return (void) if resource was set successfully,
        ;; or #f if resource is not supported on current OS
        (if ret
          (when (< ret 0)
            (raise-c-errno 'rlimit-set! 'setrlimit ret hard-soft key value))
          ret)))))


(record-writer (record-type-descriptor rlimit)
  (lambda (rl port writer)
    (put-string port "(rlimit ")
    (writer (rlimit-name rl) port)
    (put-char port #\space)
    (writer (rlimit-soft rl) port)
    (put-char port #\space)
    (writer (rlimit-hard rl) port)
    (put-string port ")")))

) ; close library
