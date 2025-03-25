;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file contains tests and should be loaded with (sh-read-file)
;;
;; odd elements are Scheme form to evaluate, even elements are expected result
#(


  ;; ----------------------- bytespan -------------------------------------
  (bytespan 1 2 3)                                 ,(bytespan 1 2 3)
  (list->bytespan '(56 12 0 46))                   ,(bytespan 56 12 0 46)
  (bytevector->bytespan #vu8(7 19 88 255))         ,(bytespan 7 19 88 255)
  (bytespan->bytevector (bytespan 65 66 67))       #vu8(65 66 67)
  (bytespan-length (bytespan 1 2 3))               3
  (bytespan-capacity-right (bytespan 1 2 3))        3
  (bytespan-empty? (bytespan))                     #t
  (bytespan-empty? (bytespan 250))                 #f
  (bytespan-ref-right/u8 (bytespan 251 252))            252
  (bytespan-ref/u8 (bytespan 252 253 254 255) 2)   254
  (let* ((v (bytevector 1 2 3))
         (sp (bytevector->bytespan v)))
     ;; set! does NOT propagate to the bytespan
     (bytevector-u8-set! v 1 7) sp)                ,(bytespan 1 2 3)
  (let* ((v (bytevector 1 2 3))
         (sp (bytevector->bytespan* v)))
    ;; set! propagates to the bytespan
    (bytevector-u8-set! v 1 7)  sp)                ,(bytespan 1 7 3)
  (let ((sp (bytespan 4 5 6)))
    (bytespan-insert-right/u8! sp 7 8) sp)          ,(bytespan 4 5 6 7 8)
  (let ((bsp (bytespan 9 10 11 12)))
    (bytespan-index/u8 bsp
      (lambda (elem) (fx=? 11 elem))))             2
  ;; ----------------------- charspan -------------------------------------
  (charspan #\1 #\2 #\3)                           ,(string->charspan* "123")
  (list->charspan '(#\i #\j #\k #\l))              ,(string->charspan* "ijkl")
  (string->charspan "pqrst")                       ,(string->charspan* "pqrst")
  (string->charspan* "ouh[()&*U")                  ,(string->charspan* "ouh[()&*U")
  (charspan->string (string->charspan "pqrst"))    "pqrst"
  (charspan-length (charspan #\a #\b #\c))         3
  (charspan-capacity-right (charspan #\a #\b #\c))  3
  (charspan-empty? (charspan))                     #t
  (charspan-empty? (charspan #\~))                 #f
  (charspan-ref-right (charspan #\{ #\\))               #\\
  (charspan-ref (charspan #\x #\y #\z) 2)          #\z
  (charspan-range-count=
    (string->charspan* "abcdef") 2
    (string->charspan* "1cde34") 1 4)              3
  (charspan-range=?
    (string->charspan* "abcdef") 2
    (string->charspan* "1cde34") 1 3)              #t
  (let* ((s "abc")
         (sp (string->charspan s)))
    ;; set! does NOT propagate to the charspan
    (string-set! s 1 #\^)  sp)                     ,(string->charspan* "abc")
  (let* ((s "abc")
         (sp (string->charspan* s)))
    ;; set! propagates to the charspan
    (string-set! s 1 #\^) sp)                      ,(string->charspan* "a^c")
  (let ((sp (charspan #\A #\B)))
    (charspan-insert-left! sp #\{ #\~) sp)        ,(string->charspan* "{~AB")
  (let ((sp (charspan #\4 #\5 #\6)))
    (charspan-insert-right! sp #\7 #\8) sp)         ,(string->charspan* "45678")
  (let ((sp (string->charspan "qwerty")))
    (charspan-erase-left! sp 1) sp)               ,(string->charspan* "werty")
  (let ((sp (string->charspan "asdfuiop")))
    (charspan-erase-right! sp 3) sp)                ,(string->charspan* "asdfu")
  (let ((sp (charspan #\@ #\a #\b #\c)))
    (charspan-index sp
      (lambda (elem) (eq? #\b elem))))             2
  (charspan->utf8b (string->charspan*
    "\x7c; \xdcce;\xdc98; \xdce0;\xdca4;\xdcb9; \xdcf0;\xdc90;\xdc8d;\xdc88;"))
                                                   ,(bytespan 124 32 206 152 32 224 164 185 32 240 144 141 136)
  (bytevector->bytespan (text->bytevector0
    (string->charspan* "123\x0;")))                ,(bytespan 49 50 51 0)
  ;; ----------------------- gbuffer ---------------------------
  (gbuffer 'a 2 3.7)                               ,(gbuffer a 2 3.7)
  (vector->gbuffer* (vector 0 1 2))                ,(gbuffer 0 1 2)
  (span->gbuffer* (span 0 1 2))                    ,(gbuffer 0 1 2)
  (let ((gb (make-gbuffer 5 #f)))
    (gbuffer-iterate gb
      (lambda (i elem)
        (gbuffer-set! gb i (fx- i)))) gb)          ,(gbuffer 0 -1 -2 -3 -4)
  (let ((gb (gbuffer 'a 'b 'c 'd 'e)))
    (gbuffer-erase-range! gb 2 4)
    (gbuffer-insert-at! gb 1 'x) gb)               ,(gbuffer a x b e)
  ;; --------------------- chargbuffer ------------------------------------
  (chargbuffer #\X #\Y #\Z)                        ,(string->chargbuffer* "XYZ")
  (string->chargbuffer* "qwerty")                  ,(string->chargbuffer* "qwerty")
  (charspan->chargbuffer* (string->charspan* "abcdef"))
                                                   ,(string->chargbuffer* "abcdef")
  (let ((gb (make-chargbuffer 5 #\@)))
    (chargbuffer-iterate gb
      (lambda (i elem)
        (chargbuffer-set! gb i (integer->char (fx+ i 64)))))
    gb)                                            ,(string->chargbuffer* "@ABCD")
  (let ((gb (chargbuffer #\a #\b #\c #\d #\e)))
    (chargbuffer-erase-range! gb 2 4)
    (chargbuffer-insert-at! gb 1 #\x)
    gb)                                            ,(string->chargbuffer* "axbe")
  ;; ------------------------ charline ------------------------------------
  (string->charline "abc 123")                     ,(string->charline* "abc 123")
  (string->charline* "echo \n")                    ,(string->charline* "echo \n")
  (charline-nl? (string->charline "echo \n"))      #t
  (charline-length (string->charline "echo \n"))   6
  (charline-index
    (string->charline* "qwerty=<>")
    9
    (lambda (ch) (char=? ch #\=)))                 6
  (let ((line (string->charline* "foo/bar"))
        (sp   (span))
        (pred (lambda (ch) (char=? ch #\b))))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i 10) sp)
      (span-insert-right! sp (charline-index-right line i pred))))
                                                   ,(span 4 4 4 4 4 #f #f #f #f #f)
  (let ((line (string->charline* "qwerty==="))
        (sp   (span))
        (pred (lambda (ch) (char=? ch #\=))))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i 12) sp)
      (span-insert-right! sp (charline-count line i pred))))
                                                   ,(span 0 0 0 0 0 0 0 1 2 3 3 3)
  (let ((line (string->charline* "qwerty==="))
        (sp   (span))
        (pred (lambda (ch) (char=? ch #\=))))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i 12) sp)
      (span-insert-right! sp (charline-count-right line i pred))))
                                                   ,(span 0 0 0 0 0 0 3 2 1 0 0 0)
  (let* ((l1 (string->charline* "foo/bar"))
         (l2 (charline-copy-on-write l1)))
    (charline-erase-range! l1 3 4)
    (charline-insert-at! l2 3 #\~)
    (list l1 l2))                                  ,((string->charline* "foobar") (string->charline* "foo~/bar"))
  (let* ((l1 (string->charline* "abcdefgh"))
         (l2 (charline-copy-on-write l1)))
    (charline-insert-at/cbuf! l1 5 (string->charline* "012345") 2 5)
    (list l1 l2))                                  ,((string->charline* "abcde234fgh") (string->charline* "abcdefgh"))
  ;; ------------------------ charlines -----------------------------------
  (charlines (string->charline* "foo/bar")
    (string->charline "\n"))                       ,(strings->charlines* "foo/bar" "\n")
  (charlines-index (strings->charlines* "qwerty@$%" "asdf")
    4 1
    (lambda (ch) (char=? ch #\@)))                 7
  (charlines-index-right (charlines
    (string->charline* "IOHPR$\n")
    (string->charline* "ORJZX"))
    0 0
    (lambda (ch) (char=? ch #\Z)))                 10
  (charlines-count (strings->charlines* "abc\n" "ccc")
    4 1
    (lambda (ch) (char=? ch #\c)))                 3
  (charlines-count-right (strings->charlines* "abc\n" "ccc")
    3 0
    (lambda (ch) (not (char=? ch #\c))))           1
  ;; ------------------------ vscreen -------------------------------------
  (let ((screen (vscreen* 8 30 "qwerty\n" "asdfgh")))
    (vscreen-cursor-vxy-set! screen 3 1)
    (vscreen-cursor-move/left! screen 6)
    (values->list (vscreen-cursor-vxy screen)))    (4 0)
  (let ((screen (vscreen* 8 30 "qwertyuiop\n" "asdfgh")))
    (vscreen-cursor-vxy-set! screen 5 0)
    (vscreen-cursor-move/right! screen 13)
    (values->list (vscreen-cursor-vxy screen)))    (6 1)
  (let ((screen (vscreen* 8 30 "qwerty\n" "asdfghjkl")))
    (vscreen-cursor-vxy-set! screen 9 1)
    (vscreen-cursor-move/up! screen 1)
    (values->list (vscreen-cursor-vxy screen)))    (6 0)
  (let ((screen (vscreen* 8 30 "abcdef\n" "0123456")))
    (vscreen-erase-at-xy! screen 5 0 3)
    screen)                                        ,(vscreen* 8 30 "abcde123" "456")
  (let ((screen (vscreen* 8 30 "{[()]}\n" "abcdef" "0123456")))
    (vscreen-cursor-vxy-set! screen 3 2)
    (vscreen-erase-left/line! screen)
    screen)                                        ,(vscreen* 8 30 "{[()]}\n" "3456")
  (let ((screen (vscreen* 8 30 "abcdef" "012\n" "{[()]}\n")))
    (vscreen-cursor-vxy-set! screen 4 0)
    (vscreen-erase-right/line! screen)
    screen)                                        ,(vscreen* 8 30 "abcd\n" "{[()]}\n")
  (let ((screen (vscreen* 8 30 "abcdef" "012\n")))
    (vscreen-insert-at-xy/char! screen 4 1 #\space)
    screen)                                        ,(vscreen* 8 30 "abcdef" "012\n" " ")
  (let ((screen (vscreen* 8 30 "abcdef" "012\n")))
    (vscreen-insert-at-xy/newline! screen 4 0)
    screen)                                        ,(vscreen* 8 30 "abcd\n" "ef012\n" "")
  (let ((screen (vscreen* 8 30 "abcdefgh" "012\n")))
    (vscreen-insert-at-xy/cspan! screen 4 0 (string->charspan* "uwxyz"))
    screen)                                        ,(vscreen* 8 30 "abcduwxy" "zefgh012" "\n" "")
  (let ((screen (vscreen* 8 30 "abcdefgh" "012\n")))
    (values->list
      (vscreen-count-before-xy/left screen 4 1
        (lambda (ch) (not (char=? ch #\d))))))         (4 0 8)
  (let ((screen (vscreen* 8 30 "abcdefgh" "012\n")))
    (values->list
      (vscreen-count-at-xy/right screen 4 0
        (lambda (ch) (not (char=? ch #\newline))))))   (3 1 7)
  (let ((screen (vscreen* 8 30 "abcdefgh" "012\n" "qwert")))
    (vscreen-cursor-ixy-set! screen 3 1) ; move the cursor to the char '\n'
    (vscreen-resize! screen 5 30)
    (list (vscreen-cursor-ix screen)
          (vscreen-cursor-iy screen) screen))          ,(1 2 (vscreen* 5 30 "abcde" "fgh01" "2\n" "qwert" ""))
  (let ((screen (vscreen* 8 30 "abcdefgh" "012\n" "qwerty")))
      (vscreen-cursor-ixy-set! screen 3 1) ; move the cursor to the char '\n'
      (vscreen-resize! screen 9 30)
      (list (vscreen-cursor-ix screen)
            (vscreen-cursor-iy screen) screen))        ,(2 1 (vscreen* 9 30 "abcdefgh0" "12\n" "qwerty"))
  ;; ------------------------ wire ----------------------------------------
  (wire-put->bytevector
    (void))                                            #vu8(0 0 0)
  (wire-put->bytevector
    (list 1 2 3 127 255 (box #\a)))                    #vu8(15 0 0 38 6 0 0 1 2 3 16 127 17 255 0 34 31 97)
)
