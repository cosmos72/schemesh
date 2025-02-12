;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers misc (0 7 4))
  (export
    list->bytevector subbytevector
    bytevector-fill-range! bytevector-find/u8 bytevector-iterate bytevector-compare
    bytevector<=? bytevector<? bytevector>=? bytevector>?

    list-iterate list-quoteq! list-reverse*! list-remove-consecutive-duplicates! with-list-elements

    vector-copy! subvector vector-fill-range! vector-iterate vector->hashtable vector-range->list)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) bytevector foreign-procedure fx1+ fx1- include)
    (only (schemesh bootstrap) assert*))


(include "containers/bytevector.ss")
(include "containers/list.ss")
(include "containers/vector.ss")


) ; close library
