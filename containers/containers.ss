;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers (0 1))
  (export
    ; misc.ss
    list-iterate list-quoteq! list-nth list-reverse*! list-remove-consecutive-duplicates!
    string-list? assert-string-list? string-contains-only-decimal-digits?
    vector-copy! subvector vector-fill-range! vector-iterate vector->hashtable vector-range->list
    list->bytevector subbytevector
    bytevector-fill-range! bytevector-iterate bytevector-compare
    bytevector<=? bytevector<? bytevector>=? bytevector>?
    string-fill-range! string-range-count= string-range=? string-range<?
    string-find/char string-rfind/char string-split string-iterate
    string-starts-with/char? string-ends-with/char?

    ; utf8b.ss
    integer->char* string->utf8b string->utf8b/0 utf8b->string utf8b-range->string

    ; hashtable.ss
    make-hash-iterator hash-iterator? hash-iterator-copy hash-iterator-cell hash-iterator-next!
    hashtable-iterate hashtable-transpose eq-hashtable eqv-hashtable hashtable

    ; bitmap.ss
    bitmap make-bitmap bitmap? bitmap-length bitmap-ref bitmap-set! bitmap-last-zero

    ; span.ss
    list->span vector->span vector->span* make-span span->list span->vector span span?
    span-length span-empty? span-clear! span-capacity span-capacity-front span-capacity-back
    span-ref span-back span-set! span-fill! span-fill-range! span-range->span* span-copy span-copy!
    span-reserve-front! span-reserve-back! span-resize-front! span-resize-back!
    span-insert-front! span-insert-back! span-insert-front/span! span-insert-back/span!
    span-erase-front! span-erase-back! span-iterate span-find
    span-peek-beg span-peek-end span-peek-data

    ; bytespan.ss
    list->bytespan bytevector->bytespan bytevector->bytespan* make-bytespan bytespan->bytevector
    bytespan bytespan? bytespan-length bytespan-empty? bytespan-clear!
    bytespan-capacity bytespan-capacity-front bytespan-capacity-back
    bytespan-ref/u8 bytespan-back/u8 bytespan-set/u8!
    bytespan-fill! bytespan-fill-range! bytespan-copy bytespan-copy! bytespan=?
    bytespan-reserve-front! bytespan-reserve-back! bytespan-resize-front! bytespan-resize-back!
    bytespan-insert-front/u8! bytespan-insert-back/u8!
    bytespan-insert-front/bspan! bytespan-insert-back/bspan!
    bytespan-insert-front/bvector! bytespan-insert-back/bvector!
    bytespan-erase-front! bytespan-erase-back! bytespan-iterate bytespan-find/u8
    bytespan-peek-beg bytespan-peek-end bytespan-peek-data

    ; charspan.ss
    list->charspan string->charspan string->charspan* make-charspan charspan->string charspan-range->string
    charspan charspan? assert-charspan? charspan-length charspan-empty? charspan-clear!
    charspan-capacity charspan-capacity-front charspan-capacity-back charspan-ref
    charspan-front charspan-back
    charspan-set! charspan-fill! charspan-fill-range! charspan-copy charspan-copy!
    charspan=? charspan-range-count= charspan-range=? charspan-range/string=?
    charspan-reserve-front! charspan-reserve-back! charspan-resize-front! charspan-resize-back!
    charspan-insert-front!        charspan-insert-back!
    charspan-insert-front/cspan!  charspan-insert-back/cspan!
    charspan-insert-front/string! charspan-insert-back/string!
    charspan-erase-front! charspan-erase-back! charspan-iterate
    charspan-find charspan-rfind charspan-find/ch charspan-rfind/ch
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

    ; charline.ss
    charline charline? string->charline string->charline* charline->string
    assert-charline? charline-nl? charline-copy-on-write charline-empty?
    charline-length charline-ref charline-at charline-equal? charline-set! charline-clear!
    charline-erase-at! charline-insert-at! charline-insert-at/cspan! charline-insert-at/cbuf!
    charline-find/left charline-find/right charline-count/left charline-count/right
    charline-dirty-start-x charline-dirty-end-x charline-dirty-x-add! charline-dirty-x-unset!

    ; charlines.ss
    charlines charlines? strings->charlines strings->charlines*
    assert-charlines? charlines-shallow-copy charlines-copy-on-write charlines-iterate
    charlines-empty? charlines-length charlines-equal? charlines-ref charlines-set/cline!
    charlines-clear! charlines-find/left charlines-find/right charlines-count/left charlines-count/right
    charlines-dirty-start-y charlines-dirty-end-y charlines-dirty-y-add! charlines-dirty-xy-unset!
    charlines-erase-at/cline! charlines-insert-at/cline!
    write-charlines

    ; sort.ss
    span-range-sort! vector-range-sort!

    ; utils.ss
    bytevector-ref/utf8b bytevector-set/utf8b! char->utf8b-length
    bytespan-ref/char bytespan-set/char! bytespan-insert-front/char! bytespan-insert-back/char!
    bytespan-insert-back/cspan! bytespan-insert-back/cbuffer! bytespan-display-back/fixnum!
    charspan->utf8b charspan->utf8b/0)

  (import (schemesh containers misc)
          (schemesh containers utf8b)
          (schemesh containers hashtable)
          (schemesh containers bitmap)
          (schemesh containers span)
          (schemesh containers bytespan)
          (schemesh containers charspan)
          (schemesh containers gbuffer)
          (schemesh containers chargbuffer)
          (schemesh containers charline)
          (schemesh containers charlines)
          (schemesh containers sort)
          (schemesh containers utils))

) ; close library
