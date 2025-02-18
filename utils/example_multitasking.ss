

(library (schemesh example multitasking (0 7 5))
  (export
    tasks make-task task-yield task-resume)

  (import
    (rnrs)
    (only (chezscheme)          procedure-arity-mask void)
    (only (schemesh bootstrap)  assert*)
    (schemesh containers span))


(define tasks
  (let ((sp (span)))
    (lambda () sp)))


(define-record-type
  (task %make-task task?)
  (fields
     id
     (mutable status) ; one of: 'new 'running 'failed
     (mutable result)
     start-proc
     (mutable resume-proc)
     (mutable suspend-proc))
  (nongenerative #{task hvcyofcpj596hi922bdsnfbh9-2}))


(define (task-find task-or-id)
  (let ((all (tasks))
        (x task-or-id))
    (cond
      ((task? x)
        x)
      ((and (fixnum? x) (fx<? -1 x (span-length all)))
        (span-ref all x))
      (else
        #f))))


(define (task-end task result)
  (task-status-set! task 'failed)
  (task-result-set! task result)
  result)


(define (make-task start-proc)
  (assert* 'make-task (procedure? start-proc))
  (assert* 'make-task (logbit? 1 (procedure-arity-mask start-proc)))
  (let* ((all (tasks))
         (id (span-length all))
         (wrapper-proc
           (lambda (task)
             (task-end task (start-proc task))))
         (task (%make-task id 'new #f wrapper-proc #f #f)))
    (span-insert-back! all task)
    task))


;; resume running task. returns when task calls (task-end) or (task-yield)
(define (task-resume task-or-id)
  (let ((task (task-find task-or-id)))
    (assert* 'task-resume task)
    (call/cc
      ;; Capture the continuation representing THIS call to task-resume
      (lambda (susp)
        (let ((proc (case (task-status task)
                      ((new failed) (task-start-proc task))
                      ((running)    (task-resume-proc task))
                      (else     #f))))
          (when proc
            (task-status-set! task 'running)
            (task-result-set! task #f)
            (task-suspend-proc-set! task susp)
            (proc task)))))))


;; suspend this task and return intermediate-result to whoever called (task-resume task)
(define (task-yield task intermediate-result)
  (assert* 'task-yield (task-suspend-proc task))
  (call/cc
    ;; Capture the continuation representing THIS call to task-yield
    (lambda (cont)
      ;; store it as task's resume-proc
      (task-resume-proc-set! task (lambda (task) (cont)))
      ;; suspend task, i.e. call its suspend-proc, and also unset suspend-proc
      (let ((susp (task-suspend-proc task)))
        (task-suspend-proc-set! task #f)
        (susp intermediate-result)))))


) ; close library

(import (schemesh example multitasking))


(make-task
  (lambda (task)
    (format #t "I am task ~s\n" task)
    (task-yield task 42)
    (format #t "I am still task ~s\n" task)
    (task-yield task 43)
    (format #t "I am really task ~s\n" task)
    ; (task-end task 44)
    44))

(void)
