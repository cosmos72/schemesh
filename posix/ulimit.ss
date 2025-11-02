;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh posix ulimit (0 9 2))
  (export ulimit-keys ulimit-ref ulimit-set!)
  (import
    (rnrs)
    (only (chezscheme)                    foreign-procedure)
    (only (schemesh bootstrap)            assert*)
    (only (schemesh containers list)      for-list))


(define c-errno-einval ((foreign-procedure "c_errno_einval" () int)))


(define ulimit-keys
  (lambda ()
    '(coredump-size data-size nice file-size pending-signals
     locked-memory-size memory-size open-files pipe-size msgqueue-size
     real-time-priority stack-size cpu-time user-processes
     virtual-memory-size file-locks real-time-nonblocking-time)))


(define ulimit-key->ckey
  (let ((htable (make-eq-hashtable)))
    (for-list ((key  (ulimit-keys))
               (ckey ((foreign-procedure "c_rlimit_keys" () ptr))))
      (hashtable-set! htable key ckey))
    (lambda (sym)
      (hashtable-ref htable sym #f))))

  
(define ulimit-ref
  (let ((c-rlimit-get (foreign-procedure "c_rlimit_get" (int int) ptr)))
    (lambda (hard-soft resource)
      (let ((ckey (ulimit-key->ckey resource)))
        (if (and ckey (memq hard-soft '(hard soft)))
          (let ((c-value (c-rlimit-get (if (eq? hard-soft 'hard) 1 0) ckey)))
            (if (eq? #t c-value) 'unlimited c-value))
          c-errno-einval)))))


(define ulimit-set!
  (let ((c-rlimit-set (foreign-procedure "c_rlimit_set" (int int ptr) int)))
    (lambda (hard-soft resource value)
      (let ((ckey (ulimit-key->ckey resource)))
        (if (and (memq hard-soft '(hard soft))
                 ckey
                 (or (eq? 'unlimited value) 
                     (and (exact? value) (integer? value) (<= 0 value #xFFFFFFFFFFFFFFFF))))
          (c-rlimit-set (if (eq? hard-soft 'hard) 1 0)
                        ckey
                        (if (eq? 'unlimited value) #t value))
          c-errno-einval)))))

) ; close library
