;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit parenmatcher (0 1))
  (export
    parenmatcher? make-custom-parenmatcher)
  (import
    (rnrs)
    (only (chezscheme) record-writer))

;; type parenmatcher contains bookkeeping information,
;; to be filled by an actual function that matches parenthesis
;; - as for example sh-parenmatcher

(define-record-type
  (parenmatcher %make-parenmatcher parenmatcher?)
  (fields
    update-func       ; procedure
    lookup-func       ; procedure
    (mutable state)) ; #f or some state indexable by x and y
  (nongenerative #{parenmatcher mjy8nva9hgl7vh7srl6427u98-521}))

;; Create a parenmatcher containing user-specified procedures.
;;
;; update-func must be a procedure accepting two argument: textual-input-port enabled-parsers
;; and returning one value: an opaque state.
;; it should parse textual-input-port, find matching parenthesis or grouping tokens,
;; and return an opaque state representing them.
;;
;; opaque state will be stored in parenmatcher-state to avoid calling update-func
;; multiple times on the same input.
;;
;; lookup-func must be a procedure accepting three arguments: state x y
;; and returning two values: match-x match-y
;; or -1 -1 if no matching parenthesis or grouping token was found
(define (make-custom-parenmatcher update-func lookup-func state)
  (assert (procedure? update-func))
  (assert (procedure? lookup-func))
  (%make-parenmatcher update-func lookup-func state))


;; Find parenthesis or grouping token matching position x y.
;;
;; In detail:
;;
;; first, if needed (i.e. if (parenmatcher-state pm) is #f) then parse textual-input-port
;; by calling (parenmatcher-update-func pm) to produce an opaque state,
;;
;; then call (parenmatcher-lookup-func pm) to find parenthesis or grouping token
;; matching position x y and return such match-x match-y
;; or -1 -1 if no matching parenthesis or grouping token was found
(define (parenmatcher-lookup pm in enabled-parsers x y)
  (if pm
    (let ((state (parenmatcher-state pm)))
      (unless state
        (set! state ((parenmatcher-update-func pm) in enabled-parsers))
        (parenmatcher-state-set! pm state))
      ((parenmatcher-lookup-func pm) state x y))
    (values -1 -1)))

(define (parenmatcher-clear! pm)
  (when pm
    (parenmatcher-state-set! pm #f)))


; customize how "parenmatcher" objects are printed
(record-writer (record-type-descriptor parenmatcher)
  (lambda (pm port writer)
    (display "#<parenmatcher " port)
    (display (parenmatcher-update-func pm) port)
    (display " " port)
    (display (parenmatcher-lookup-func pm) port)
    (display " " port)
    (display (parenmatcher-state pm) port)
    (display ">" port)))

) ; close library
