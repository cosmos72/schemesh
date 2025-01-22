;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit (0 7 0))
  (export
    ; charlines-io.ss
    open-charline-input-port open-charlines-input-port

    ; charhistory
    charhistory charhistory? make-charhistory
    charhistory-empty? charhistory-length charhistory-ref/cow charhistory-iterate
    charhistory-find/starts-with charhistory-rfind/starts-with
    charhistory-erase-consecutive-empty-charlines-before!
    charhistory-set*! charhistory-path charhistory-path-set!

    ; charhistory-io.ss
    charhistory-load!           charhistory-save
    charhistory-load-from-path! charhistory-save-to-path
    charhistory-load-from-port! charhistory-save-to-port

    ; linectx.ss
    make-linectx make-linectx* linectx? linectx-rbuf linectx-wbuf
    linectx-vscreen linectx-width linectx-height linectx-end-y
    linectx-ix     linectx-iy     linectx-ixy  linectx-ixy-set!
    linectx-vx     linectx-vy
    linectx-term-x linectx-term-y linectx-term-xy-set!
    linectx-stdin  linectx-stdin-set! linectx-stdout linectx-stdout-set!
    linectx-prompt      linectx-prompt-end-x  linectx-prompt-end-y
    linectx-prompt-func linectx-prompt-length linectx-prompt-length-set!
    linectx-parenmatcher linectx-ktable
    linectx-paren linectx-paren-set!
    linectx-completions linectx-completion-stem linectx-completion-func
    linectx-parser-name linectx-parser-name-set!
    linectx-parsers linectx-parsers-set!
    linectx-history linectx-history-index linectx-history-index-set! linectx-to-history*
    linectx-load-history! linectx-save-history
    linectx-clear!  linectx-eof? linectx-eof-set! linectx-redraw? linectx-redraw-set!
    linectx-return? linectx-return-set!
    linectx-default-keytable linectx-keytable-set! linectx-keytable-find

    ; lineedit.ss
    lineedit-clear!
    lineedit-lines-set! lineedit-insert/rbuf!
    lineedit-key-nop lineedit-key-left lineedit-key-right lineedit-key-up lineedit-key-down
    lineedit-key-word-left lineedit-key-word-right lineedit-key-bol lineedit-key-eol
    lineedit-key-break lineedit-key-ctrl-d lineedit-key-transpose-char
    lineedit-key-del-left lineedit-key-del-right
    lineedit-key-del-word-left lineedit-key-del-word-right
    lineedit-key-del-line lineedit-key-del-line-left lineedit-key-del-line-right
    lineedit-key-enter lineedit-key-newline-left lineedit-key-newline-right
    lineedit-key-history-next lineedit-key-history-prev
    lineedit-key-redraw lineedit-key-tab lineedit-key-toggle-insert
    lineedit-paren-find/before-cursor lineedit-paren-find/surrounds-cursor
    lineedit-read lineedit-read-confirm-y-or-n? lineedit-flush lineedit-finish

    ; lineterm.ss
    lineterm-write/u8 lineterm-write/bvector lineterm-write/bspan lineterm-write/cspan lineterm-write/cbuffer
    lineterm-move-dx lineterm-move-dy lineterm-move-to-bol lineterm-clear-to-eol lineterm-clear-to-eos
    lineterm-move lineterm-move-from lineterm-move-to

    ; paren.ss
    make-paren      paren?           paren-name
    paren-start-token paren-end-token paren-end-token-set!
    paren-start-x   paren-start-y    paren-start-xy-set!
    paren-end-x     paren-end-y      paren-end-xy-set!
    paren-ok?       paren-recursive-ok?  paren-valid?
    paren-inner     paren-inner-empty?
    paren-inner-ref paren-inner-ref* paren-inner-append!
    paren->values   paren->hashtable paren-hashtable-ref
    paren-find/surrounds is-paren-char? debugf-paren

    ; parenmatcher.ss
    parenmatcher? make-custom-parenmatcher parenmatcher-clear!
    parenmatcher-paren parenmatcher-maybe-update!
    parenmatcher-find/at parenmatcher-find/surrounds

    ; parser.ss
    make-parsectx make-parsectx* parsectx? string->parsectx
    parsectx-in parsectx-current-pos parsectx-previous-pos parsectx-enabled-parsers

    make-parser parser?
    parser-name parser-parse-forms parser-parse-paren parser-autocomplete
    get-parser-or-false get-parser to-parser

    parsectx-peek-char parsectx-read-char parsectx-unread-char parsectx-skip-whitespace
    parsectx-skip-line parsectx-skip-until-char parsectx-read-simple-identifier
    try-read-parser-directive

    syntax-errorf

    ; vscreen.ss
    make-vscreen  vscreen*  vscreen?  assert-vscreen?
    vscreen-width        vscreen-height     vscreen-width-at-y  vscreen-resize!
    vscreen-dirty?       vscreen-dirty-set!
    vscreen-cursor-ix    vscreen-cursor-iy  vscreen-cursor-ixy  vscreen-cursor-ixy-set!
    vscreen-cursor-vx    vscreen-cursor-vy  vscreen-cursor-vxy  vscreen-cursor-vxy-set!
    vscreen-prompt-end-x vscreen-prompt-end-y vscreen-prompt-length  vscreen-prompt-length-set!
    vscreen-length-at-y  vscreen-length
    vscreen-char-at-xy   vscreen-char-before-xy  vscreen-char-after-xy
    vscreen-next-xy      vscreen-prev-xy   vscreen-next-xy/or-self  vscreen-prev-xy/or-self
    vscreen-count-before-xy/left  vscreen-count-at-xy/right
    vscreen-clear!       vscreen-empty?
    vscreen-cursor-move/left! vscreen-cursor-move/right!  vscreen-cursor-move/up!  vscreen-cursor-move/down!
    vscreen-erase-left/n!     vscreen-erase-right/n!      vscreen-erase-at-xy!
    vscreen-erase-left/line!  vscreen-erase-right/line!
    vscreen-insert-at-xy/ch!  vscreen-insert-at-xy/newline! vscreen-insert-at-xy/cspan!
    vscreen-insert/ch!        vscreen-insert/cspan!         vscreen-assign*!
    write-vscreen)

  (import
    (schemesh lineedit charlines io)
    (schemesh lineedit charhistory)
    (schemesh lineedit charhistory io)
    (schemesh lineedit linectx)
    (schemesh lineedit lineterm)
    (schemesh lineedit lineedit)
    (schemesh lineedit paren)
    (schemesh lineedit parenmatcher)
    (schemesh lineedit parser)
    (schemesh lineedit vscreen))

) ; close library
