(library (schemesh example signal handler (0 9 0))
  (export check-interrupts init-signal-handlers)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) $primitive console-error-port format register-signal-handler top-level-bound?))


(define check-interrupts ($primitive 3 $event))


(define (handle-signal int)
  (let ((port (console-error-port)))
    (format port "received signal: ~a\n" int)
    (flush-output-port port)))


(define (init-signal-handlers)
  (register-signal-handler  3 handle-signal)  ; SIGQUIT on Linux
  (register-signal-handler 17 handle-signal)  ; SIGCHLD on Linux
  (register-signal-handler 20 handle-signal)) ; SIGTSTP on Linux


) ; close library

(when (top-level-bound? 'sh-version)
  (display (console-error-port)
          "; warning: this signal handler example should be run from vanilla Chez Scheme.\n\
           ;          running it from schemesh will interfere with signal handlers installed by schemesh.\n"))

(import (schemesh example signal handler))
