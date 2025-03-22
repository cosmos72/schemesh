;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this library needs to be installed with "plt-r6rs --install --force chez-compat.rkt"
;; see https://docs.racket-lang.org/r6rs/index.html
;;
;; then it can be imported from Racket #!r6rs files with the usual (import (chez compat))
;;
(library (chez compat (0 8 1))
  (export append!
          check-interrupts current-time
          chez:car chez:cdr chez:cons chez:list chez:pair? 
          fx1+ fx1- fx/ foreign-procedure format
          keyboard-interrupt-handler
          list-copy #|list-head|# load-shared-object lock-object
          #|pariah|#
          read-token register-signal-handler reverse!
          top-level-syntax unlock-object unread-char
          time-second time-nanosecond tree->chez:tree
          void)
  (import (ffi unsafe vm)
          (rnrs)
          (only (srfi :1) append! list-copy reverse!)
          (only (srfi :19) current-time time-second time-nanosecond)
          )

;; create and access Chez Scheme cons objects, not Racket mpair objects
(define chez:car                   (vm-primitive 'car))
(define chez:cdr                   (vm-primitive 'cdr))
(define chez:cons                  (vm-primitive 'cons))
(define chez:list                  (vm-primitive 'list))
(define chez:pair?                 (vm-primitive 'pair?))

;; convert a tree of Racket mpair objects to a tree of Chez Scheme cons objects
(define (tree->chez:tree obj)
  (cond
    ((pair? obj)
      (chez:cons (tree->chez:tree (car obj))
                 (tree->chez:tree (cdr obj))))
    ((chez:pair? obj)
      (chez:cons (tree->chez:tree (chez:car obj))
                 (tree->chez:tree (chez:cdr obj))))
    (else
      obj)))

(define check-interrupts           (vm-eval (chez:list '$primitive 3 '$event)))
(define keyboard-interrupt-handler (vm-primitive 'keyboard-interrupt-handler))
(define format                     (vm-primitive 'format))
(define fx1+                       (vm-primitive 'fx1+))
(define fx1-                       (vm-primitive 'fx1-))
(define fx/                        (vm-primitive 'fx/))
(define load-shared-object         (vm-primitive 'load-shared-object))
(define lock-object                (vm-primitive 'lock-object))
;(define pariah                     (vm-primitive 'pariah))
(define read-token                 (vm-primitive 'read-token))
(define register-signal-handler    (vm-primitive 'register-signal-handler))
(define top-level-syntax           (vm-primitive 'top-level-syntax))
(define unlock-object              (vm-primitive 'unlock-object))
(define unread-char                (vm-primitive 'unread-char))
(define void                       (vm-primitive 'void))

(define-syntax foreign-procedure
  (lambda (stx)
    (syntax-case stx ()
      ;; warning: (syntax->datum #'args) returns Racket mpair objects,
      ;; not Chez Scheme cons objects needed by (vm-eval)
      ;; => convert them
      ((_ . args) #'(vm-eval (chez:cons 'foreign-procedure (tree->chez:tree (syntax->datum #'args))))))))


) ; close library
