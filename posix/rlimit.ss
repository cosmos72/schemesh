;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (schemesh posix rlimit (0 9 2))
  (export rlimit-keys rlimit-ref rlimit-set!)
  (import
    (rnrs)
    (only (chezscheme)                foreign-procedure)
    (only (schemesh bootstrap)        assert*) ; debugf
    (only (schemesh containers list)  for-list)
    (only (schemesh posix fd)         raise-c-errno))


(define c-errno-einval ((foreign-procedure "c_errno_einval" () int)))


(define rlimit-keys
  (lambda ()
    '(coredump-size data-size nice file-size pending-signals
     locked-memory-size memory-size open-files pipe-size msgqueue-size
     realtime-priority stack-size cpu-time user-processes
     virtual-memory-size file-locks realtime-nonblocking-time)))


(define rlimit-key->ckey
  (let ((htable (make-eq-hashtable)))
    (for-list ((key  (rlimit-keys))
               (ckey ((foreign-procedure "c_rlimit_keys" () ptr))))
      (hashtable-set! htable key ckey))
    (lambda (who resource)
      (let ((ckey (hashtable-ref htable resource #f)))
        (unless ckey
          ;; throw if resource is not among (rlimit-keys)
          (assert* who (memq resource (rlimit-keys))))
        ;; ckey can be #f if resource is not among C rlimit_keys[]
        ckey))))


;; return unsigned per-process hard or soft resource limit, i.e. getrlimit()
;; returns #f if specified resource is not supported on current OS
;; returns 'unlimited if resource has no limit
(define rlimit-ref
  (let ((c-rlimit-get (foreign-procedure "c_rlimit_get" (int int) ptr)))
    (lambda (hard-soft resource)
      ;; (debugf "(rlimit-ref ~s ~s)" hard-soft resource)
      (assert* 'rlimit-ref (memq hard-soft '(hard soft)))
      (let* ((ckey (rlimit-key->ckey 'rlimit-ref resource))
             (ret  (and ckey (c-rlimit-get (if (eq? hard-soft 'hard) 1 0) ckey))))
        ;; (debugf "(rlimit-ref ~s ~s) -> ret = ~s" hard-soft resource ret)
        (cond
          ((eq? #t ret)
            'unlimited)
          ((and ret (< ret 0))
            (raise-c-errno 'rlimit-ref 'getrlimit ret hard-soft resource))
          (else
            ret))))))


;; set per-process hard or soft resource limit, i.e. setrlimit().
;; limit to set must be an unsigned-64 or symbol 'unlimited
;; returns (void), or #f if specified resource is not supported on current OS
(define rlimit-set!
  (let ((c-rlimit-set (foreign-procedure "c_rlimit_set" (int int ptr) int)))
    (lambda (hard-soft resource value)
      (assert* 'rlimit-set! (memq hard-soft '(hard soft)))
      (unless (eq? 'unlimited value)
        (assert* 'rlimit-set! (exact? value))
        (assert* 'rlimit-set! (integer? value))
        (assert* 'rlimit-set! (<= 0 value #xFFFFFFFFFFFFFFFF)))
      (let* ((ckey (rlimit-key->ckey 'rlimit-set! resource))
             (ret  (and
                     ckey
                     (c-rlimit-set
                       (if (eq? hard-soft 'hard) 1 0)
                       ckey
                       (if (eq? 'unlimited value) #t value)))))
        ;; return (void) if resource was set successfully,
        ;; or #f if resource is not supported on current OS
        (if ret
          (when (< ret 0)
            (raise-c-errno 'rlimit-set! 'setrlimit ret hard-soft resource value))
          ret)))))

) ; close library
