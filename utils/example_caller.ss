(library (schemesh example caller (0 7 7))
  (export call^ apply^)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) eval-when #| format make-time sleep |# ))

(eval-when (compile) (optimize-level 3) (debug-level 0))

(define resume&yield (cons #f #f))


(define (caller-loop proc-and-args)
  (do () (#f)
    ;; (format #t "; caller-loop proc-and-args=~s\n" proc-and-args)
    ;; (sleep (make-time 'time-duration 0 1))
    (let* ((proc (car proc-and-args))
           (args (cdr proc-and-args))
           (rets (call-with-values
                    (lambda () (apply proc args))
                    list))
           (yield  (cdr resume&yield)))
      (set-cdr! resume&yield #f)
      ;; (format #t "; proc rets=~s\n" rets)
      (set! proc-and-args
        (if (car resume&yield)
          (apply yield rets)
          (call/cc
            (lambda (resume)
              ;; (format #t "; installing caller-resume-proc=~s\n" resume)
              (set-car! resume&yield resume)
              (apply yield rets))))))))


(define (apply^ proc args)
  (call/cc
    (lambda (yield)
      ;; (format #t "; installing caller-yield-proc=~s\n" yield)
      (set-cdr! resume&yield yield)
      (let ((resume (or (car resume&yield) caller-loop)))
        ;; (format #t "; resuming caller-resume-proc=~s\n" resume)
        ;; (set-car! resume&yield #f)
        (resume (cons proc args))))))


;; call a procedure from a continuation and return its value(s).
;; slow, its main purpose is that procedure is called from a different
;; dynamic context with fixed depth.
(define call^
  (case-lambda
    ((proc)        (apply^ proc '()))
    ((proc . args) (apply^ proc args))))

) ; close library


(import (schemesh example caller))
