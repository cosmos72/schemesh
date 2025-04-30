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
  ;; ----------------------- cellspan -------------------------------------
  (cellspan #\1 #\2 #\3)                           ,(string->cellspan "123")
  (list->cellspan '(#\i #\j #\k #\l))              ,(string->cellspan "ijkl")
  (string->cellspan "pqrst")                       ,(string->cellspan "pqrst")
  (string->cellspan "ouh[()&*U")                   ,(string->cellspan "ouh[()&*U")
  (cellspan-length (cellspan #\a #\b #\c))         3
  (cellspan-capacity-right (cellspan #\a #\b #\c)) 3
  (cellspan-empty? (cellspan))                     #t
  (cellspan-empty? (cellspan #\~))                 #f
  (cell->char
    (cellspan-ref-right (cellspan #\{ #\\)))       #\\
  (cell->char
    (cellspan-ref (cellspan #\x #\y #\z) 2))       #\z
  #|
  (cellspan-count=
    (string->cellspan "abcdef") 2
    (string->cellspan "1cde34") 1 4)               3
  (cellspan=?
    (string->cellspan "abcdef") 2
    (string->cellspan "1cde34") 1 3)               #t
  |#
  (let ((sp (cellspan #\A #\B)))
    (cellspan-insert-left! sp #\{ #\~) sp)         ,(string->cellspan "{~AB")
  (let ((sp (cellspan #\4 #\5 #\6)))
    (cellspan-insert-right! sp #\7 #\8) sp)        ,(string->cellspan "45678")
  (let ((sp (string->cellspan "qwerty")))
    (cellspan-delete-left! sp 1) sp)               ,(string->cellspan "werty")
  (let ((sp (string->cellspan "asdfuiop")))
    (cellspan-delete-right! sp 3) sp)              ,(string->cellspan "asdfu")
  (let ((sp (cellspan #\@ #\a #\b #\c)))
    (cellspan-index sp
      (lambda (elem)
        (eq? #\b (cell->char elem)))))             2
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
  (charspan-count=
    (string->charspan* "abcdef") 2
    (string->charspan* "1cde34") 1 4)              3
  (charspan=?
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
    (charspan-insert-left! sp #\{ #\~) sp)         ,(string->charspan* "{~AB")
  (let ((sp (charspan #\4 #\5 #\6)))
    (charspan-insert-right! sp #\7 #\8) sp)        ,(string->charspan* "45678")
  (let ((sp (string->charspan "qwerty")))
    (charspan-delete-left! sp 1) sp)               ,(string->charspan* "werty")
  (let ((sp (string->charspan "asdfuiop")))
    (charspan-delete-right! sp 3) sp)              ,(string->charspan* "asdfu")
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
    (gbuffer-delete! gb 2 4)
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
    (chargbuffer-delete! gb 2 4)
    (chargbuffer-insert-at! gb 1 #\x)
    gb)                                            ,(string->chargbuffer* "axbe")
  ;; ------------------------ vline ------------------------------------
  (vline "abc 123")                                ,(vline "abc 123")
  (vline "echo \n")                                ,(vline "echo \n")
  (vline-nl? (vline "echo \n"))                    #t
  (vline-length (vline "echo \n"))                 6
  (vline-index/char
    (vline "qwerty=<>") 0 9 #\=)                   6
  (let ((line (vline "foo/bar"))
        (sp   (span))
        (pred (lambda (cl) (char=? #\b (cell->char cl)))))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i 10) sp)
      (span-insert-right! sp (vline-index-right line i pred))))
                                                   ,(span 4 4 4 4 4 #f #f #f #f #f)
  (let ((line (vline "qwerty==="))
        (sp   (span))
        (pred (lambda (cl) (char=? #\= (cell->char cl)))))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i 12) sp)
      (span-insert-right! sp (vline-count line i pred))))
                                                   ,(span 0 0 0 0 0 0 0 1 2 3 3 3)
  (let ((line (vline "qwerty==="))
        (sp   (span))
        (pred (lambda (cl) (char=? #\= (cell->char cl)))))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i 12) sp)
      (span-insert-right! sp (vline-count-right line i pred))))
                                                   ,(span 0 0 0 0 0 0 3 2 1 0 0 0)
  (let* ((l1 (vline "foo/bar"))
         (l2 (vline-copy-on-write l1)))
    (vline-delete! l1 3 4)
    (vline-insert-at! l2 3 #\~)
    (list l1 l2))                                  ,((vline "foobar") (vline "foo~/bar"))
  (let* ((l1 (vline "abcdefgh"))
         (l2 (vline-copy-on-write l1)))
    (vline-insert-at/cellgbuffer! l1 5 (vline "012345") 2 5)
    (list l1 l2))                                  ,((vline "abcde234fgh") (vline "abcdefgh"))
  ;; ------------------------ vlines -----------------------------------
  (vlines (vline "foo/bar") (vline "\n"))              ,(vlines "foo/bar" "\n")
  (vlines-index (vlines "qwerty@$%" "asdf")
    4 1
    (lambda (cl) (char=? #\@ (cell->char cl))))        7
  (vlines-index-right (vlines
    (vline "IOHPR$\n") "ORJZX")
    0 0
    (lambda (cl) (char=? #\Z (cell->char cl))))        10
  (vlines-count (vlines "abc\n" "ccc")
    4 1
    (lambda (cl) (char=? #\c (cell->char cl))))        3
  (vlines-count-right (vlines "abc\n" "ccc")
    3 0
    (lambda (cl) (not (char=? #\c (cell->char cl)))))  1
  ;; ------------------------ vscreen -------------------------------------
  (let ((screen (vscreen 8 30 "qwerty\n" "asdfgh")))
    (vscreen-cursor-vxy-set! screen 3 1)
    (vscreen-cursor-move/left! screen 6)
    (values->list (vscreen-cursor-vxy screen)))        (4 0)
  (let ((screen (vscreen 8 30 "qwertyuiop\n" "asdfgh")))
    (vscreen-cursor-vxy-set! screen 5 0)
    (vscreen-cursor-move/right! screen 13)
    (values->list (vscreen-cursor-vxy screen)))        (6 1)
  (let ((screen (vscreen 8 30 "qwerty\n" "asdfghjkl")))
    (vscreen-cursor-vxy-set! screen 9 1)
    (vscreen-cursor-move/up! screen 1)
    (values->list (vscreen-cursor-vxy screen)))        (6 0)
  (let ((screen (vscreen 8 30 "abcdef\n" "0123456")))
    (vscreen-delete-at-xy! screen 5 0 3)
    screen)                                            ,(vscreen 8 30 "abcde123" "456")
  (let ((screen (vscreen 8 30 "{[()]}\n" "abcdef" "0123456")))
    (vscreen-cursor-vxy-set! screen 3 2)
    (vscreen-delete-left/vline! screen)
    screen)                                            ,(vscreen 8 30 "{[()]}\n" "3456")
  (let ((screen (vscreen 8 30 "abcdef" "012\n" "{[()]}\n")))
    (vscreen-cursor-vxy-set! screen 4 0)
    (vscreen-delete-right/vline! screen)
    screen)                                            ,(vscreen 8 30 "abcd\n" "{[()]}\n")
  (let ((screen (vscreen 8 30 "abcdef" "012\n")))
    (vscreen-insert-at-xy/c! screen 4 1 #\space)
    screen)                                            ,(vscreen 8 30 "abcdef" "012\n" " ")
  (let ((screen (vscreen 8 30 "abcdef" "012\n")))
    (vscreen-insert-at-xy/newline! screen 4 0)
    screen)                                            ,(vscreen 8 30 "abcd\n" "ef012\n" "")
  (let ((screen (vscreen 8 30 "abcdefgh" "012\n")))
    (vscreen-insert-at-xy/cellspan! screen 4 0 (string->cellspan "uwxyz"))
    screen)                                            ,(vscreen 8 30 "abcduwxy" "zefgh012" "\n" "")
  (let ((screen (vscreen 8 30 "abcdefgh" "012\n")))
    (values->list
      (vscreen-count-before-xy/left screen 4 1
        (lambda (ch) (not (char=? ch #\d))))))         (4 0 8)
  (let ((screen (vscreen 8 30 "abcdefgh" "012\n")))
    (values->list
      (vscreen-count-at-xy/right screen 4 0
        (lambda (ch) (not (char=? ch #\newline))))))   (3 1 7)
  (let ((screen (vscreen 8 30 "abcdefgh" "012\n" "qwert")))
    (vscreen-cursor-ixy-set! screen 3 1) ; move the cursor to the char '\n'
    (vscreen-resize! screen 5 30)
    (list (vscreen-cursor-ix screen)
          (vscreen-cursor-iy screen) screen))          ,(1 2 (vscreen 5 30 "abcde" "fgh01" "2\n" "qwert" ""))
  (let ((screen (vscreen 8 30 "abcdefgh" "012\n" "qwerty")))
      (vscreen-cursor-ixy-set! screen 3 1) ; move the cursor to the char '\n'
      (vscreen-resize! screen 9 30)
      (list (vscreen-cursor-ix screen)
            (vscreen-cursor-iy screen) screen))        ,(2 1 (vscreen 9 30 "abcdefgh0" "12\n" "qwerty"))
  ;; ------------------------ wire ----------------------------------------
  (datum->wire (void))                                 #vu8(0)
  (datum->wire "\xFF;")                                #vu8(3 41 1 255)
  (datum->wire "\x100;")                               #vu8(4 42 1 0 1)
  (datum->wire "\xFFFF;")                              #vu8(4 42 1 255 255)
  (datum->wire "\x10000;")                             #vu8(5 43 1 0 0 1)
  (datum->wire "\x10FFFF;")                            #vu8(5 43 1 255 255 16)
  (datum->wire
    (list 1 2 3 127 255 (box (eof-object))))           #vu8(15 38 6 0 0 0 1 2 3 16 127 17 255 0 34 29)
  (datum->wire
    (list* (void) -7/3 1/2+1i 'fl=? 'string=?))        #vu8(17 37 5 0 0 0 28 21 16 249 3 22 21 1 2 1 70 74)
  (datum->wire
    '#(#\a #\xFF #\xFFFF #\x10FFFF "bcd" #vfx(-1 1)))  #vu8(22 39 6 31 97 31 255 32 255 255 33 255 255 16 41 3 98 99 100 44 2 15 1)
  (datum->wire
    (gbuffer (span #vu8(1)) (ok 9 10) (charspan #\a #\xFF)
             (chargbuffer #\< #\>)))                   #vu8(24 245 4 244 1 40 1 1 243 6 38 2 0 0 0 9 10 248 2 97 255 251 2 60 62)
  (datum->wire
    (make-time 'time-utc 999999999 #x80000000))        #vu8(14 242 88 19 255 201 154 59 20 5 0 0 0 128 0)
  (datum->wire
    (eq-hashtable (void) 1.5 '() #x123456789))         #vu8(20 51 2 27 20 5 137 103 69 35 1 28 23 0 0 0 0 0 0 248 63)
  (datum->wire
    (eqv-hashtable #\{ 0.5+2.0i))                      #vu8(21 52 1 31 123 24 0 0 0 0 0 0 224 63 0 0 0 0 0 0 0 64)
  (let ((bv (datum->wire
              (hashtable string-hash string=? "a" 1 "b" 2))))
    (or (bytevector=? bv #vu8(12 53 76 74 2 41 1 97 1 41 1 98 2))
        (bytevector=? bv #vu8(12 53 76 74 2 41 1 98 2 41 1 97 1))))       #t


  (values->list (wire->datum  #vu8(4 243 6 36 27)))    ,((ok ()) 5)
  (values->list (wire->datum  #vu8(3 244 1 25)))       ,((span #f) 4)
  (values->list (wire->datum  #vu8(3 245 1 26)))       ,((gbuffer #t) 4)
  (values->list (wire->datum  #vu8(2 246 0)))          ,((bytespan) 3)
  ;(values->list (wire->datum  #vu8(2 247 0)))         ,((bytegbuffer) 3)
  (values->list (wire->datum  #vu8(3 248 1 64)))       ,((string->charspan* "@") 4)
  (values->list (wire->datum  #vu8(4 249 1 65 0)))     ,((string->charspan* "A") 5)
  (values->list (wire->datum  #vu8(5 250 1 66 0 0)))   ,((string->charspan* "B") 6)
  (values->list (wire->datum  #vu8(3 251 1 67)))       ,((string->chargbuffer* "C") 4)
  (values->list (wire->datum  #vu8(4 252 1 68 0)))     ,((string->chargbuffer* "D") 5)
  (values->list (wire->datum  #vu8(5 253 1 69 0 0)))   ,((string->chargbuffer* "E") 6)
  (values->list (wire->datum  #vu8(14 242 88 19
      255 201 154 59 20 5 255 255 255 255 0)))         ,@"(#<time-utc 4294967295.999999999> 15)"

  (datum->wire (vector
    (bitwise-arithmetic-shift 1 64)
    (bitwise-arithmetic-shift -1 60)
    #\xDC80 #\xDCFF 'foo "bar\x20AC;" '#vfx(0)
    #vu8(255 254 253) (bytespan 7)))                       #vu8(55 39 9 20 9 0 0 0 0 0 0 0 0 1 20 8 0 0 0 0 0 0 0 240 32
                                                                128 220 32 255 220 46 3 102 111 111 42 4 98 0 97 0 114 0
                                                                172 32 44 1 0 40 3 255 254 253 246 1 7)

  (values->list (wire->datum
     #vu8(59 39 9 20 9 0 0 0 0 0 0 0 0 1 20 8 0 0 0 0 0 0 0 240 32
          ;; here "bar\x20AC;" is unnecessarily serialized with tag-string24
          ;; instead of the more compact tag-string16.
          ;; harmless, and we want to test that it works too
          128 220 32 255 220 46 3 102 111 111 43 4 98 0 0 97 0 0 114 0 0
          172 32 0 44 1 0 40 3 255 254 253 246 1 7)))      ,(#(18446744073709551616 -1152921504606846976
                                                               #\xDC80 #\xDCFF foo "bar\x20AC;" #vfx(0)
                                                               #vu8(255 254 253) (bytespan 7)) 60)

  (let ((ht (first-value (wire->datum #vu8(14 53 76 74 2 41 2 101 102 14 41 2 99 100 15)))))
    (vector-sort
      (lambda (cell1 cell2)
        (string<? (car cell1) (car cell2)))
      (hashtable-cells ht)))                               #(("cd" . -1) ("ef" . -2))

  (let* ((payload-len 512)
         (message-len (fx+ 4 payload-len))
         (bv (make-bytevector message-len)))
    (with-exception-handler
      (lambda (ex)
        (display-condition ex)
        (newline)
        (write bv (current-output-port))
        (newline)
        (raise ex))
      (lambda ()
        (repeat 0
          (do ((i 0 (fx1+ i)))
              ((fx>=? i message-len))
            (bytevector-u8-set! bv i (random 128)))
          (let-values (((obj pos) (wire->datum bv)))
            (when (and (fixnum? pos) (fx>? pos 0))
              (unless (or (boolean? obj) (eq? (void) obj) (fixnum? obj) (symbol? obj) (char? obj))
                (format #t "~s\n" obj))
              (assert* 'test-wire->datum
                       (fx<=? pos payload-len))))))))          ,@"#<void>"

)
