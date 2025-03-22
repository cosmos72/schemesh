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

  (+ 1 2 3) 6
  (* 4 5 6) 120
  ;; ----------------- bootstrap ------------------------------------------
  (let ((x 0))
    (repeat 5 (set! x (fx1+ x)))
    x)                                             5
  (try (assert* 'who #t) 1 (catch (ex) 2))         1
  (try (assert* 'who #f) 1 (catch (ex) 2))         2
  (values->list (values 1 2 3))                    (1 2 3)
  (let-macro ((plus . args) `(+ ,@args))
     (plus 3 4 5))                                 12
  (let-macro ((plus arg0 . args) `(+ ,arg0 ,@args))
     (plus 3 4 5))                                 12
  (-> + 1 2 -> / 4)                                4/3
  (-> + 2 3 -> / _ 4)                              5/4
  ;; '(expand-omit-library-invocations #t) (void)  do not use, requires Chez Scheme >= 10.0.0
  ;; '(begin (debugf \"warmup\") (debugf \"a\") (debugf \"b\") (debugf \"c\")) (void)
  ;; ----------------- containers/misc ------------------------------------
  (subvector '#(aa bb cc dd) 1 3)                  #(bb cc)
  (subbytevector #vu8(44 55 66 77) 2 3)            #vu8(66)
  (bytevector-compare #vu8(44 55) #vu8(44 55))     0
  (bytevector-compare #vu8(66 77) #vu8(66 77 0))   -1
  (bytevector-compare #vu8(66 77) #vu8(66 78))     -1
  (bytevector-compare #vu8(79) #vu8(78 0))         1
  (string-range-count= "qwertyuiop" 2 "_ertyuio7"
                        1 8)                       7
  (string-replace-all "abcdbacdabcd" "ab" "0")     "0cdbacd0cd"
  (string-split "" #\:)                            ("")
  (string-split ":" #\:)                           ("" "")
  (string-split "x:" #\:)                          ("x" "")
  (string-split ":y" #\:)                          ("" "y")
  (string-split "ab:cdef::g" #\: 1 10)             ("b" "cdef" "" "g")
  (string-trim-split-at-blanks "")                 ()
  (string-trim-split-at-blanks "\n\x0;ab c\x1f;")  ("ab" "c")
  (list-remove-consecutive-duplicates!
    (list "foo" "foo" "foo" "bar" "bar" "" "bar")
    string=?)                                      ("foo" "bar" "" "bar")
  (do ((i #x-10000 (fx1+ i)))
      ((fx>=? i #x120000) #t)
    (if (or (fx<=? #x0000 i #xD7FF)
            (fx<=? #xDC80 i #xDCFF)
            (fx<=? #xE000 i #x10FFFF))
      (assert* 'test (fx=? i (char->integer (integer->char* i))))
      (unless (throws? (integer->char* i))
        (error 'integer->char* "should throw" i)))) #t
  (string-range<? "abcdef" 1 5 "_abxyef" 2 4)       #t
  ;; ----------------- containers/sort ------------------------------------
  (let ((v (vector 9 8 7 6 5 4 3 2 1 0)))
    (vector-sort*! fx<? v 1 9)
    v)                                              #(9 1 2 3 4 5 6 7 8 0)
  ;; ----------------- bytevector/utf8 ------------------------------------
  (values->list (bytevector-ref/utf8b #vu8()))               (#t      0)   ; incomplete UTF-8
  (values->list (bytevector-ref/utf8b #vu8(1)))              (#\x01   1)
  (values->list (bytevector-ref/utf8b #vu8(33)))             (#\!     1)
  (values->list (bytevector-ref/utf8b #vu8(#x7E)))           (#\~     1)
  (values->list (bytevector-ref/utf8b #vu8(#x7F)))           (#\x7F   1)
  (values->list (bytevector-ref/utf8b #vu8(#x80)))           (#\xDC80 1) ; invalid UTF-8
  (values->list (bytevector-ref/utf8b #vu8(#xC0 #x80) 0 1))  (#t      1) ; incomplete UTF-8
  (values->list (bytevector-ref/utf8b #vu8(#xC0 #x80)))      (#\xDCC0 1) ; overlong UTF-8
  (values->list (bytevector-ref/utf8b #vu8(#xC1 #xBF)))      (#\xDCC1 1) ; overlong UTF-8
  (values->list (bytevector-ref/utf8b #vu8(#xC2 #x7F)))      (#\xDCC2 1) ; bad UTF-8 2nd byte
  (values->list (bytevector-ref/utf8b #vu8(#xC2 #x80)))      (#\x80   2) ; U+0080
  (values->list (bytevector-ref/utf8b #vu8(#xC2 #xA3)))      (#\xA3   2) ; U+00A3
  (values->list (bytevector-ref/utf8b #vu8(#xC2 #xBF)))      (#\xBF   2) ; U+00BF
  (values->list (bytevector-ref/utf8b #vu8(#xC2 #xC0)))      (#\xDCC2 1) ; bad UTF-8 2nd byte
  (values->list (bytevector-ref/utf8b #vu8(#xC3 #x80)))      (#\xC0   2) ; U+00C0
  (values->list (bytevector-ref/utf8b #vu8(#xDF #xBF)))      (#\x7FF  2) ; U+07FF
  (values->list (bytevector-ref/utf8b #vu8(#xE0 #x80)))      (#t      2) ; incomplete UTF-8
  (values->list (bytevector-ref/utf8b #vu8(#xE0 #x80 #x80))) (#\xDCE0 1) ; overlong UTF-8
  (values->list (bytevector-ref/utf8b #vu8(#xE0 #x9F #xBF))) (#\xDCE0 1) ; overlong UTF-8
  (values->list (bytevector-ref/utf8b #vu8(#xE0 #xA0 #x80))) (#\x800  3) ; U+0800
  (values->list (bytevector-ref/utf8b #vu8(#xED #x80 #x80))) (#\xD000 3) ; U+D000
  (values->list (bytevector-ref/utf8b #vu8(#xED #x9F #xBF))) (#\xD7FF 3) ; U+D7FF
  (values->list (bytevector-ref/utf8b #vu8(#xED #xA0 #x80))) (#\xDCED 1) ; invalid UTF-8, would encode U+D800
  (values->list (bytevector-ref/utf8b #vu8(#xED #xB2 #x80))) (#\xDCED 1) ; invalid UTF-8, would encode U+DC80
  (values->list (bytevector-ref/utf8b #vu8(#xED #xB3 #xBF))) (#\xDCED 1) ; invalid UTF-8, would encode U+DCFF
  (values->list (bytevector-ref/utf8b #vu8(#xED #xBF #xBF))) (#\xDCED 1) ; invalid UTF-8, would encode U+DFFF
  (values->list (bytevector-ref/utf8b #vu8(#xee #x80 #x80))) (#\xE000 3) ; U+E000
  (values->list (bytevector-ref/utf8b #vu8(#xef #xBF #xBF))) (#\xFFFF 3) ; U+FFFF
  (values->list (bytevector-ref/utf8b #vu8(#xf0 #x80 #x80 #x80) 0 3)) (#t 3)      ; incomplete UTF-8
  (values->list (bytevector-ref/utf8b #vu8(#xf0 #x80 #x80 #x80)))     (#\xDCF0 1) ; overlong UTF-8
  (values->list (bytevector-ref/utf8b #vu8(#xf0 #x8f #xBF #xBF)))     (#\xDCF0 1) ; overlong UTF-8
  (values->list (bytevector-ref/utf8b #vu8(#xf0 #x90 #x80 #x80)))    (#\x10000 4) ; U+10000
  (values->list (bytevector-ref/utf8b #vu8(#xf4 #x8f #xBF #xBF)))   (#\x10FFFF 4) ; U+10FFFF
  (values->list (bytevector-ref/utf8b #vu8(#xf4 #x90 #x80 #x80)))     (#\xDCF4 1) ; invalid UTF-8
  (values->list (bytevector-ref/utf8b #vu8(#xf4 #xBF #xBF #xBF)))     (#\xDCF4 1) ; invalid UTF-8
  (values->list (bytevector-ref/utf8b #vu8(#xf5 #x80 #x80 #x80)))     (#\xDCF5 1) ; invalid UTF-8
  (values->list (bytevector-ref/utf8b #vu8(#xf6)))                    (#\xDCF6 1) ; invalid UTF-8
  (values->list (bytevector-ref/utf8b #vu8(#xfe)))                    (#\xDCFE 1) ; invalid UTF-8
  (values->list (bytevector-ref/utf8b #vu8(#xff)))                    (#\xDCFF 1) ; invalid UTF-8
  (string->utf8b (string (integer->char* #xdc80)))           #vu8(#x80)
  (string->utf8b (string (integer->char* #xdcff)))           #vu8(#xff)
  (string->utf8b "\xdc80;\xdc81;\xdcfe;\xdcff;")             #vu8(#x80 #x81 #xfe #xff)
  ;; test incomplete UTF-8 sequence #x80 at end of bytes
  (utf8b->string (string->utf8b "abc\x20AC;\xDC80;"))        "abc\x20AC;\xDC80;"
  (list
    (char->utf8b-length (integer->char 0))
    (char->utf8b-length (integer->char #x7F))
    (char->utf8b-length (integer->char #x80))
    (char->utf8b-length (integer->char #x7ff))
    (char->utf8b-length (integer->char #x800))
    (char->utf8b-length (integer->char #xffff))
    (char->utf8b-length (integer->char #x10000))
    (char->utf8b-length (integer->char #x10ffff)))          (1 1 2 2 3 3 4 4)
  (let ((bv (make-bytevector 1)))
    (bytevector-set/utf8b! bv 0 #\~)
    bv)                                                      #vu8(#x7e)
  (let ((bv (make-bytevector 2)))
    (bytevector-set/utf8b! bv 0 (integer->char #xa3))        ; pound sign
    bv)                                                      #vu8(#xc2 #xa3)
  (let ((bv (make-bytevector 3)))
    (bytevector-set/utf8b! bv 0 (integer->char #x20ac))      ; euro sign
    bv)                                                      #vu8(#xe2 #x82 #xac)
  (let ((bv (make-bytevector 4)))
    (bytevector-set/utf8b! bv 0 (integer->char #x10348))
    bv)                                                      #vu8(#xf0 #x90 #x8d #x88)
  (let ((bv (make-bytevector 4)))
    (bytevector-set/utf8b! bv 0 (integer->char #x10ffff))
    bv)                                                      #vu8(#xf4 #x8f #xbf #xbf)
  ;; ----------------- bytespan-ref/char -------------------------------------
  (values->list (bytespan-ref/char (bytespan) 0))                      (#t 0)       ; incomplete UTF-8
  (values->list (bytespan-ref/char (bytespan 1) 0))                    (#\x01 1)    ; U+0001
  (values->list (bytespan-ref/char (bytespan #x7F) 0))                 (#\x7F 1)    ; U+007F
  (values->list (bytespan-ref/char (bytespan #x80) 0))                 (#\xDC80 1)  ; invalid UTF-8
  (values->list (bytespan-ref/char (bytespan #xc2 #x80) 0))            (#\x80 2)    ; U+0080
  (values->list (bytespan-ref/char (bytespan #xdf #xBF) 0))            (#\x7FF 2)   ; U+07FF
  (values->list (bytespan-ref/char (bytespan #xe0 #xA0 #x80) 0))       (#\x800 3)   ; U+0800
  (values->list (bytespan-ref/char (bytespan #xED #x80 #x80) 0))       (#\xD000 3)  ; U+D000
  (values->list (bytespan-ref/char (bytespan #xED #x9f #xBF) 0))       (#\xD7FF 3)  ; U+D7FF
  (values->list (bytespan-ref/char (bytespan #xED #xA0 #x80) 0))       (#\xDCED 1)  ; invalid UTF-8
  (values->list (bytespan-ref/char (bytespan #xED #xBF #xBF) 0))       (#\xDCED 1)  ; invalid UTF-8
  (values->list (bytespan-ref/char (bytespan #xee #x80 #x80) 0))       (#\xE000 3)  ; U+E000
  (values->list (bytespan-ref/char (bytespan #xef #xBF #xBF) 0))       (#\xFFFF 3)  ; U+FFFF
  (values->list (bytespan-ref/char (bytespan #xf0 #x90 #x80 #x80) 0))  (#\x10000 4) ; U+10000
  (values->list (bytespan-ref/char (bytespan #xf4 #x8f #xBF #xBF) 0))  (#\x10FFFF 4); U+10FFFF
  (let ((sp (bytespan)))
    (bytespan-insert-right/char! sp #\~) sp)                         ,(bytespan 126)
  (let ((sp (bytespan)))
    (bytespan-insert-right/char! sp (integer->char #xa3)) sp)        ,(bytespan 194 163)
  (let ((sp (bytespan)))
    (bytespan-insert-right/char! sp (integer->char #x20ac)) sp)      ,(bytespan 226 130 172)
  (let ((sp (bytespan)))
    (bytespan-insert-left/char! sp (integer->char #x10348)) sp)    ,(bytespan 240 144 141 136)
  (let ((sp (bytespan)))
    (bytespan-insert-left/char! sp (integer->char #x10ffff)) sp)   ,(bytespan 244 143 191 191)
  ;; ----------------- bytespan-fixnum-display ----------------------------
  (let ((sp (bytespan)))
    (for ((n (in-list '(0 1 9 10 99 100 999 1000 9999 10000 99999 100000 999999 1000000
                       9999998 10000000 12345678 -1 -9 -10 -87654321))))
      (bytespan-display-right/fixnum! sp n)
      (bytespan-insert-right/u8! sp 32))
    (utf8b-bytespan->string sp))
              "0 1 9 10 99 100 999 1000 9999 10000 99999 100000 999999 1000000 9999998 10000000 12345678 -1 -9 -10 -87654321 "

  ;; ------------------------- span ---------------------------------------
  (span 1 2 3)                             ,(span 1 2 3)
  (list->span '(foo bar baz))              ,(span foo bar baz)
  (span-length (span 1 2 3))               3
  (span-capacity-left (span 1 2 3))       3
  (span-capacity-right  (span 1 2 3))       3
  (span-empty? (span))                     #t
  (span-empty? (span 'x))                  #f
  (span-ref-right (span 'x 'y))                 y
  (span-ref (span 'a 'b 'c) 1)             b
  (let* ((v  (vector 1 2 3))
         (sp (vector->span v)))
     ;; set! does NOT propagate to the span
     (vector-set! v 1 7) sp)               ,(span 1 2 3)
  (let* ((v  (vector 1 2 3))
         (sp (vector->span* v)))
     ;; set! propagates to the span
     (vector-set! v 1 7) sp)               ,(span 1 7 3)
  (let ((sp (span 'p 'q 'r)))
    (span-insert-left! sp 'i 'j) sp)      ,(span i j p q r)
  (let ((sp (span 'foo)))
    (span-insert-right! sp 'bar 'qux) sp)   ,(span foo bar qux)
  (let ((sp (span 1 2 3))
        (sp2 (span -1 0)))
    (span-insert-left/span! sp sp2 0 2)
    sp)                                    ,(span -1 0 1 2 3)
  (let ((sp (span 1 2 3))
        (sp2 (span -1 0)))
    (span-insert-right/span! sp sp2) sp)    ,(span 1 2 3 -1 0)
  (let ((sp (span 'a 'b 'c 'd)))
    (span-erase-left! sp 3) sp)           ,(span d)
  (let ((sp (span 'a 'b 'c 'd)))
    (span-erase-right! sp 1) sp)            ,(span a b c)
  (let ((sp (span 'a 'b 'c 'd)))
    (span-index sp 0 999
      (lambda (elem) (eq? 'c elem))))     2
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
  ;; --------------------- list -------------------------------------------
  (let ((ret '()))
    (for-list ((elem '(a b c)))
      (set! ret (cons elem ret)))
    ret)                                               (c b a)
  (list-reverse*! (list))                              ()
  (list-reverse*! (list 1))                            (1)
  (list-reverse*! (list 1 2))                          (2 . 1)
  (list-reverse*! (list 1 2 3 4 5 6))                  (6 5 4 3 2 . 1)
  ;; ------------------------ hashtable -----------------------------------
  (vector-sort
    (lambda (cell1 cell2) (< (car cell1) (car cell2)))
    (hashtable-cells (eq-hashtable
      '(3 . C) '(2 . B) '(1 . A))))                    #((1 . A) (2 . B) (3 . C))
  (vector-sort
    (lambda (cell1 cell2) (< (car cell1) (car cell2)))
    (hashtable-cells (alist->eqv-hashtable
      '((1.0 . A) (2.1 . B) (3 . C)))))                #((1.0 . A) (2.1 . B) (3 . C))
  (vector-sort
    (lambda (cell1 cell2) (< (car cell1) (car cell2)))
    (hashtable-cells (eqv-hashtable
      '(3.1 . C) '(2 . B) '(1 . A))))                  #((1 . A) (2 . B) (3.1 . C))
  (vector-sort
    (lambda (cell1 cell2) (string<? (car cell1) (car cell2)))
    (hashtable-cells
      (hashtable string-hash string=?
        '("a" . 1) '("B" . 2) '("+" . 3))))            #(("+" . 3) ("B" . 2) ("a" . 1))
  (string-hashtable->argv
    (hashtable string-hash string=?
      '("A" . "X") '("B" . "Y") '("C" . "Z")))         #(#vu8(65 61 88 0) #vu8(66 61 89 0) #vu8(67 61 90 0))
  (let ((ret '()))
    (for-hash-pairs ((cell (eqv-hashtable '(1.0 . A) '(2.1 . B) '(3 . C))))
      (set! ret (cons cell ret)))
    (sort!
      (lambda (cell1 cell2) (< (car cell1) (car cell2)))
      ret))                                            ((1.0 . A) (2.1 . B) (3 . C))

)
