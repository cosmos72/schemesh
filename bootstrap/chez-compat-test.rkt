#!r6rs

;; this file can be executed with "plt-r6rs chez-compat-test.rkt"
;;
;; after installing (chez compat) library as described in sibling file "chez-compat.rkt"
;;
(import (rnrs)
        (chez compat))

(for-each
    (lambda (p) (display p) (newline))
  (list append!
          check-interrupts current-time
          chez:car chez:cdr chez:cons chez:list chez:pair? 
          fx1+ fx1- fx/ #|foreign-procedure|# format
          keyboard-interrupt-handler
          list-copy #|list-head|# load-shared-object lock-object
          #|pariah|#
          read-token register-signal-handler reverse!
          top-level-syntax unlock-object unread-char
          time-second time-nanosecond tree->chez:tree
          void))

(call/cc
 (lambda (k)
   (with-exception-handler
       (lambda (ex)
         (display ex)
         (k))
     (lambda ()
       (display (foreign-procedure "Sbox" (ptr) ptr))))))
(newline)

