;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh shell builtins (0 1))
  (export sh-true sh-false sh-cd sh-pwd)
  (import
    (rnrs)
    (schemesh shell jobs))

(define (sh-true . ignored-args)
  (error 'sh-true "unimplemented"))  ; TODO: implement, must return a job

(define (sh-false . ignored-args)
  (error 'sh-false "unimplemented")) ; TODO: implement, must return a job

(define (sh-cd path)
  (error 'sh-cd "unimplemented"))    ; TODO: implement, must return a job

(define (sh-pwd . ignored-args)
  (error 'sh-pwd "unimplemented"))   ; TODO: implement, must return a job

) ; close library
