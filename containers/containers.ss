;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers (0 1))
  (export
    ; misc.ss
    list-iterate reverse*!
    vector-copy! subvector vector-fill-range! vector-iterate vector->hashtable
    list->bytevector list-quoteq!
    subbytevector bytevector-fill-range! bytevector-iterate bytevector-compare
    bytevector<=? bytevector<? bytevector>=? bytevector>?
    string-fill-range! string-range=? string-iterate

    ; hashtable.ss
    make-hash-iterator hash-iterator? hash-iterator-copy hash-iterator-cell hash-iterator-next!
    hashtable-iterate hashtable-transpose eq-hashtable eqv-hashtable hashtable

    ; span.ss
    list->span vector->span vector->span* make-span span->vector span span?
    span-length span-empty? span-clear! span-capacity span-capacity-front span-capacity-back
    span-ref span-back span-set! span-fill! span-fill-range! span-copy span-copy!
    span-reserve-front! span-reserve-back! span-resize-front! span-resize-back!
    span-insert-front! span-insert-back! span-sp-insert-front! span-sp-insert-back!
    span-erase-front! span-erase-back! span-iterate span-find
    span-peek-beg span-peek-end span-peek-data

    ; bytespan.ss
    list->bytespan bytevector->bytespan bytevector->bytespan* make-bytespan bytespan->bytevector
    bytespan bytespan? bytespan-length bytespan-empty? bytespan-clear!
    bytespan-capacity bytespan-capacity-front bytespan-capacity-back
    bytespan-u8-ref bytespan-u8-back bytespan-u8-set!
    bytespan-fill! bytespan-fill-range! bytespan-copy bytespan-copy! bytespan=?
    bytespan-reserve-front! bytespan-reserve-back! bytespan-resize-front! bytespan-resize-back!
    bytespan-u8-insert-front! bytespan-u8-insert-back!
    bytespan-bsp-insert-front! bytespan-bsp-insert-back!
    bytespan-bv-insert-front! bytespan-bv-insert-back!
    bytespan-erase-front! bytespan-erase-back! bytespan-iterate bytespan-u8-find
    bytespan-peek-beg bytespan-peek-end bytespan-peek-data

    ; charspan.ss
    list->charspan string->charspan string->charspan* make-charspan charspan->string
    charspan charspan? charspan-length charspan-empty? charspan-clear! charspan-capacity
    charspan-capacity-front charspan-capacity-back charspan-ref charspan-back charspan-set!
    charspan-fill! charspan-fill-range! charspan-copy charspan-copy! charspan=? charspan-range=?
    charspan-reserve-front! charspan-reserve-back! charspan-resize-front! charspan-resize-back!
    charspan-insert-front! charspan-insert-back!
    charspan-csp-insert-front! charspan-csp-insert-back!
    charspan-erase-front! charspan-erase-back! charspan-iterate charspan-find
    charspan-peek-data charspan-peek-beg charspan-peek-end

    ; gbuffer.ss
    list->gbuffer vector->gbuffer vector->gbuffer* span->gbuffer span->gbuffer*
    make-gbuffer gbuffer gbuffer? gbuffer->vector gbuffer->span
    gbuffer-length gbuffer-empty? gbuffer-ref gbuffer-set! gbuffer-clear! gbuffer-split-at!
    gbuffer-insert-at! gbuffer-erase-at! gbuffer-iterate

    ; chargbuffer.ss
    list->chargbuffer string->chargbuffer string->chargbuffer* charspan->chargbuffer charspan->chargbuffer*
    make-chargbuffer chargbuffer chargbuffer? chargbuffer->charspan chargbuffer->string
    chargbuffer-length chargbuffer-empty?
    chargbuffer-ref chargbuffer-set! chargbuffer-clear! chargbuffer-split-at!
    chargbuffer-insert-at! chargbuffer-erase-at! chargbuffer-iterate

    ; utils.ss
    bytevector-utf8-ref bytevector-utf8-set! char->utf8-length
    bytespan-utf8-ref bytespan-utf8-set! bytespan-utf8-insert-front! bytespan-utf8-insert-back!
    bytespan-csp-insert-back! bytespan-fixnum-display-back!
    charspan->utf8)

  (import (schemesh containers misc)
          (schemesh containers hashtable)
          (schemesh containers span)
          (schemesh containers bytespan)
          (schemesh containers charspan)
          (schemesh containers gbuffer)
          (schemesh containers chargbuffer)
          (schemesh containers utils))

) ; close library