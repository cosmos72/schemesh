;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k vscreen (0 9 2))
  (export
    ;; vcell.ss
    vcell vcell? vcell->char vcell->vpalette vcell->vcolors vcell-write vcell-display/bytespan
    vcolor? vrgb/24 vrgb/8 vrgb/4 vgray/5 symbol->vrgb/4
    vcolors vcolors? vcolors->fg vcolors->bg vcolors->vpalette
    vpalette? vpalette->vcolors
    vcolor-fg-display vcolor-bg-display vcolors-display vpalette-display
    vcolor-fg-display/bytespan vcolor-bg-display/bytespan  vcolors-display/bytespan vpalette-display/bytespan

    ;; vcellcector.ss
    make-vcellvector list->vcellvector string->vcellvector
    vcellvector-length vcellvector-empty? vcellvector-ref
    vcellvector-set! vcellvector-update/char! vcellvector-update/colors! vcellvector-update/palette!
    vcellvector-fill! vcellvector-copy! vcellvector-copy/string!
    vcellvector-display vcellvector-write

    ;; vcellspan.ss
    list->vcellspan string->vcellspan make-vcellspan
    vcellspan vcellspan? assert-vcellspan? vcellspan-length vcellspan-empty? vcellspan-clear!
    vcellspan-capacity vcellspan-capacity-left vcellspan-capacity-right
    vcellspan-ref vcellspan-ref-right
    vcellspan-set! vcellspan-fill!  vcellspan-copy vcellspan-copy!
    vcellspan-reserve-left! vcellspan-reserve-right! vcellspan-resize-left! vcellspan-resize-right!
    vcellspan-insert-left!         vcellspan-insert-right!
    vcellspan-insert-left/vcellspan!  vcellspan-insert-right/vcellspan!
    vcellspan-delete-left!         vcellspan-delete-right! vcellspan-index vcellspan-index/char
    vcellspan-iterate in-vcellspan  vcellspan-write

    ;; vbuffer.ss
    in-vbuffer list->vbuffer string->vbuffer
    vcellspan->vbuffer vcellspan->vbuffer*
    make-vbuffer vbuffer vbuffer?
    vbuffer-length vbuffer-empty?
    vbuffer-ref vbuffer-set! vbuffer-clear! vbuffer-split-at!
    vbuffer-insert-at! vbuffer-insert-at/vcellspan! vbuffer-insert-at/vbuffer!
    vbuffer-delete! vbuffer-iterate
    vbuffer-display/bytespan vbuffer-write

    ;; vline.ss
    vline vline? assert-vline? vline->string
    vline-nl? vline-copy-on-write vline-empty?
    vline-length vline-ref vline-ref/char vline-at vline-at/char
    vline-equal/chars? vline-set! vline-clear!
    vline-delete! vline-insert-at! vline-insert-at/vcellspan! vline-insert-at/vbuffer!
    vline-index vline-index-right vline-index/char vline-count vline-count-right
    vline-dirty-start-x vline-dirty-end-x vline-dirty-x-add! vline-dirty-x-unset!
    in-vline vline-iterate vline-display/bytespan vline-write

    ;; vlines.ss
    vlines vlines? assert-vlines? vlines->string
    vlines-shallow-copy vlines-copy-on-write vlines-iterate
    vlines-empty? vlines-length vlines-equal/chars?
    vlines-cell-count vlines-cell-count<=? vlines-ref vlines-set! vlines-clear!
    vlines-index vlines-index-right vlines-count vlines-count-right
    vlines-dirty-start-y vlines-dirty-end-y vlines-dirty-y-add! vlines-dirty-xy-unset!
    vlines-delete-at! vlines-insert-at! vlines-starts-with?
    vlines-next-xy vlines-prev-xy vlines-cell-at-xy vlines-char-at-xy
    vlines-char-before-xy vlines-char-after-xy
    in-vlines open-vlines-input-port

    ;; vscreen.ss
    make-vscreen  (rename (%vscreen vscreen))  vscreen?  assert-vscreen?
    vscreen-width        vscreen-height     vscreen-width-at-y  vscreen-resize!
    vscreen-dirty?       vscreen-dirty-set!
    vscreen-cursor-ix    vscreen-cursor-iy  vscreen-cursor-ixy  vscreen-cursor-ixy-set!
    vscreen-cursor-vx    vscreen-cursor-vy  vscreen-cursor-vxy  vscreen-cursor-vxy-set!
    vscreen-prompt-end-x vscreen-prompt-end-y vscreen-prompt-length  vscreen-prompt-length-set!
    vscreen-length-at-y  vscreen-length       vscreen-cell-count     vscreen-cell-count<=?
    vscreen-cell-at-xy   vscreen-char-at-xy   vscreen-char-before-xy  vscreen-char-after-xy
    vscreen-next-xy      vscreen-prev-xy   vscreen-next-xy/or-self  vscreen-prev-xy/or-self
    vscreen-count-before-xy/left  vscreen-count-at-xy/right
    vscreen-clear!       vscreen-empty?
    vscreen-cursor-move/left! vscreen-cursor-move/right!  vscreen-cursor-move/up!  vscreen-cursor-move/down!
    vscreen-delete-left/n!     vscreen-delete-right/n!      vscreen-delete-at-xy!
    vscreen-delete-left/vline!  vscreen-delete-right/vline!
    vscreen-insert/c!            vscreen-insert-at-xy/c!
    vscreen-insert-at-xy/newline! vscreen-insert-at-xy/vcellspan!
    vscreen-assign*!  vscreen-reflow  vscreen-write

    ;; vhistory.ss
    vhistory vhistory? make-vhistory
    vhistory-empty? vhistory-length vhistory-ref/cow vhistory-iterate
    vhistory-index/starts-with vhistory-index-right/starts-with
    vhistory-delete-empty-lines!
    vhistory-set*! vhistory-path vhistory-path-set!

    ;; vhistory-io.ss
    vhistory-load!           vhistory-save
    vhistory-load-from-file! vhistory-save-to-path
    vhistory-load-from-port! vhistory-save-to-port)

  (import
    (rnrs)
    (rnrs mutable-pairs)
    (rnrs mutable-strings)
    (only (chezscheme)                      bytevector-truncate! format fx1+ fx1- fx1- fx/
                                            include meta-cond record-writer string-copy! void)
    (only (scheme2k bootstrap)              assert* assert-not* catch fx<=?* raise-assertf try until while)
    (only (scheme2k containers bytevector)  subbytevector-fill!)
    (only (scheme2k containers string)      string-replace/char!)
    (scheme2k containers bytespan)
    (scheme2k containers charspan)
    (only (scheme2k containers list)        for-list list-index)
    (only (scheme2k containers hashtable)   eqv-hashtable)
    (scheme2k containers span)
    (scheme2k containers gbuffer)
    (only (scheme2k containers utf8b)       integer->char* bytespan-display-right/fixnum! bytespan-insert-right/char!
                                            string->utf8b utf8b-bytespan->string)
    (only (scheme2k posix pid)              pid-get)
    (only (scheme2k posix dir)              file-delete file-rename)
    (only (scheme2k posix io)               file->port))


(include "vscreen/vcell.ss")
(include "vscreen/vcellvector.ss")
(include "vscreen/vcellspan.ss")
(include "vscreen/vbuffer.ss")
(include "vscreen/vline.ss")
(include "vscreen/vlines.ss")
(include "vscreen/vscreen.ss")
(include "vscreen/vhistory.ss")
(include "vscreen/vhistory-io.ss")
(include "vscreen/writer.ss")


) ; close library
