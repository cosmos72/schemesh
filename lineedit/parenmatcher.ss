;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit parenmatcher (0 1))
  (export
    parenmatcher? make-custom-parenmatcher parenmatcher-clear!
    parenmatcher-parens parenmatcher-maybe-update! parenmatcher-find-match)
  (import
    (rnrs)
    (only (chezscheme) record-writer)
    (schemesh lineedit parens))

;; type parenmatcher contains bookkeeping information,
;; to be filled by an actual function that matches parenthesis
;; - as for example sh-parenmatcher

(define-record-type
  (parenmatcher %make-parenmatcher parenmatcher?)
  (fields
    update-func       ; procedure (parsectx initial-parser) -> state
    (mutable parens)  ; #f or outermost parens object
    (mutable htable)) ; #f or hashtable (+ x (* y 65536)) -> parens
  (nongenerative #{parenmatcher oy6xm1zt7ltnfh0bwpj3pu0mh-559}))

;; Create a parenmatcher containing user-specified procedure.
;;
;; update-func must be a procedure accepting two argument: parsectx initial-parsers
;; and returning one value: the outermost parens object.
;; it should parse the textual input port (parsectx-in parsectx),
;; find matching parenthesis or grouping tokens,
;; and return the corresponding parens object,
;; which will be stored in parenmatcher-parens
;; to avoid calling update-func multiple times on the same input.
(define (make-custom-parenmatcher update-func)
  (assert (procedure? update-func))
  (%make-parenmatcher update-func #f #f))


;; if (parenmatcher-htable pm) is #f then parse (parsectx-in pctx)
;; by calling (parenmatcher-update-func pm) and store the created parens and hashtable
;; into parenmatcher pm
(define (parenmatcher-maybe-update! pm pctx-or-func initial-parser)
  (unless (parenmatcher-htable pm)
    (let* ((pctx   (if (procedure? pctx-or-func) (pctx-or-func) pctx-or-func))
           (parens ((parenmatcher-update-func pm) pctx initial-parser)))
      (parenmatcher-parens-set! pm parens)
      (parenmatcher-htable-set! pm (parens->hashtable parens)))))


;; Find parenthesis or grouping token matching position x y.
;;
;; In detail:
;;
;; first, call (parenmatcher-maybe-update!) to update parenmatcher if needed,
;; then call (parens-hashtable-ref) to find parenthesis or grouping token
;; matching position x y.
;;
;; Return such matching parens,
;; or #f no parenthesis or grouping token matches position x y
(define (parenmatcher-find-match pm pctx-or-func initial-parser x y)
  (if pm
    (begin
      (parenmatcher-maybe-update! pm pctx-or-func initial-parser)
      (parens-hashtable-ref (parenmatcher-htable pm) x y))
    #f))


(define (parenmatcher-clear! pm)
  (when pm
    (parenmatcher-parens-set! pm #f)
    (parenmatcher-htable-set! pm #f)))


; customize how "parenmatcher" objects are printed
(record-writer (record-type-descriptor parenmatcher)
  (lambda (pm port writer)
    (display "#<parenmatcher>" port)))

) ; close library
