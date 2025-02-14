;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit parenmatcher (0 7 5))
  (export
    parenmatcher? make-custom-parenmatcher parenmatcher-clear!
    parenmatcher-paren parenmatcher-maybe-update!
    parenmatcher-find/at parenmatcher-find/surrounds)
  (import
    (rnrs)
    (only (chezscheme)         record-writer)
    (only (schemesh bootstrap) assert*)
    (schemesh lineedit paren))

;; type parenmatcher contains bookkeeping information,
;; to be filled by an actual function that matches parenthesis
;; - as for example sh-parenmatcher

(define-record-type
  (parenmatcher %make-parenmatcher parenmatcher?)
  (fields
    update-func       ; procedure (parsectx initial-parser) -> state
    (mutable paren)   ; #f or outermost paren object
    (mutable htable)) ; #f or hashtable (+ x (* y 65536)) -> paren
  (nongenerative #{parenmatcher oy6xm1zt7ltnfh0bwpj3pu0mh-559}))

;; Create a parenmatcher containing user-specified procedure.
;;
;; update-func must be a procedure accepting two argument: parsectx initial-parsers
;; and returning one value: the outermost paren object.
;; it should parse the textual input port (parsectx-in parsectx),
;; find matching parenthesis or grouping tokens,
;; and return the corresponding paren object,
;; which will be stored in parenmatcher-paren
;; to avoid calling update-func multiple times on the same input.
(define (make-custom-parenmatcher update-func)
  (assert* 'make-custom-parenmatcher (procedure? update-func))
  (%make-parenmatcher update-func #f #f))


;; if (parenmatcher-htable pm) is #f then parse (parsectx-in pctx)
;; by calling (parenmatcher-update-func pm) and store the created paren and hashtable
;; into parenmatcher pm
(define (parenmatcher-maybe-update! pm pctx-or-func initial-parser)
  (unless (parenmatcher-htable pm)
    (let* ((pctx  (if (procedure? pctx-or-func) (pctx-or-func) pctx-or-func))
           (paren ((parenmatcher-update-func pm) pctx initial-parser)))
      ; (debugf-paren paren)
      (parenmatcher-paren-set! pm paren)
      (parenmatcher-htable-set! pm (paren->hashtable paren)))))


;; Find parenthesis or grouping token starting or ending at position x y.
;;
;; In detail:
;;
;; first, call (parenmatcher-maybe-update!) to update parenmatcher if needed,
;; then call (paren-hashtable-ref) to find parenthesis or grouping token
;; starting or ending at position x y.
;;
;; Return such matching paren,
;; or #f no parenthesis or grouping token starts or ends at position x y
(define (parenmatcher-find/at pm pctx-or-func initial-parser x y)
  (if pm
    (begin
      (parenmatcher-maybe-update! pm pctx-or-func initial-parser)
      (paren-hashtable-ref (parenmatcher-htable pm) x y))
    #f))


;; Find innermost parenthesis or grouping token surrounding position x y.
;;
;; In detail:
;;
;; first, call (parenmatcher-maybe-update!) to update parenmatcher if needed,
;; then call (paren-hashtable-ref) to find parenthesis or grouping token
;; starting or ending at position x y.
;;
;; Return such matching paren,
;; or #f no parenthesis or grouping token surrounds position x y
(define (parenmatcher-find/surrounds pm pctx-or-func initial-parser x y)
  (if pm
    (begin
      (parenmatcher-maybe-update! pm pctx-or-func initial-parser)
      (let ((paren (parenmatcher-paren pm)))
        (and paren (paren-find/surrounds paren x y))))
    #f))


(define (parenmatcher-clear! pm)
  (when pm
    (parenmatcher-paren-set! pm #f)
    (parenmatcher-htable-set! pm #f)))


; customize how "parenmatcher" objects are printed
(record-writer (record-type-descriptor parenmatcher)
  (lambda (pm port writer)
    (display "#<parenmatcher>" port)))

) ; close library
