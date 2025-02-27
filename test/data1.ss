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
  ;; '(expand-omit-library-invocations #t) (void)  do not use, requires Chez Scheme >= 10.0.0
  (expand '(-> a b (c ^ d) (e f ^)))               (e f (c (b a) d))
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
    (bytespan-insert-back/char! sp #\~) sp)                         ,(bytespan 126)
  (let ((sp (bytespan)))
    (bytespan-insert-back/char! sp (integer->char #xa3)) sp)        ,(bytespan 194 163)
  (let ((sp (bytespan)))
    (bytespan-insert-back/char! sp (integer->char #x20ac)) sp)      ,(bytespan 226 130 172)
  (let ((sp (bytespan)))
    (bytespan-insert-front/char! sp (integer->char #x10348)) sp)    ,(bytespan 240 144 141 136)
  (let ((sp (bytespan)))
    (bytespan-insert-front/char! sp (integer->char #x10ffff)) sp)   ,(bytespan 244 143 191 191)
  ;; ----------------- bytespan-fixnum-display ----------------------------
  (let ((sp (bytespan)))
    (for ((n (in-list '(0 1 9 10 99 100 999 1000 9999 10000 99999 100000 999999 1000000
                       9999998 10000000 12345678 -1 -9 -10 -87654321))))
      (bytespan-display-back/fixnum! sp n)
      (bytespan-insert-back/u8! sp 32))
    (utf8b-bytespan->string sp))
   "0 1 9 10 99 100 999 1000 9999 10000 99999 100000 999999 1000000 9999998 10000000 12345678 -1 -9 -10 -87654321 "

  ;; ------------------------- span ---------------------------------------
  (span 1 2 3)                             ,(span 1 2 3)
  (list->span '(foo bar baz))              ,(span foo bar baz)
  (span-length (span 1 2 3))               3
  (span-capacity-front (span 1 2 3))       3
  (span-capacity-back  (span 1 2 3))       3
  (span-empty? (span))                     #t
  (span-empty? (span 'x))                  #f
  (span-back (span 'x 'y))                 y
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
    (span-insert-front! sp 'i 'j) sp)      ,(span i j p q r)
  (let ((sp (span 'foo)))
    (span-insert-back! sp 'bar 'qux) sp)   ,(span foo bar qux)
  (let ((sp (span 1 2 3))
        (sp2 (span -1 0)))
    (span-insert-front/span! sp sp2 0 2)
    sp)                                    ,(span -1 0 1 2 3)
  (let ((sp (span 1 2 3))
        (sp2 (span -1 0)))
    (span-insert-back/span! sp sp2) sp)    ,(span 1 2 3 -1 0)
  (let ((sp (span 'a 'b 'c 'd)))
    (span-erase-front! sp 3) sp)           ,(span d)
  (let ((sp (span 'a 'b 'c 'd)))
    (span-erase-back! sp 1) sp)            ,(span a b c)
  (let ((sp (span 'a 'b 'c 'd)))
    (span-find sp 0 999
      (lambda (elem) (eq? 'c elem))))     2
  ;; ----------------------- bytespan -------------------------------------
  (bytespan 1 2 3)                                 ,(bytespan 1 2 3)
  (list->bytespan '(56 12 0 46))                   ,(bytespan 56 12 0 46)
  (bytevector->bytespan #vu8(7 19 88 255))         ,(bytespan 7 19 88 255)
  (bytespan->bytevector (bytespan 65 66 67))       #vu8(65 66 67)
  (bytespan-length (bytespan 1 2 3))               3
  (bytespan-capacity-back (bytespan 1 2 3))        3
  (bytespan-empty? (bytespan))                     #t
  (bytespan-empty? (bytespan 250))                 #f
  (bytespan-back/u8 (bytespan 251 252))            252
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
    (bytespan-insert-back/u8! sp 7 8) sp)          ,(bytespan 4 5 6 7 8)
  (let ((bsp (bytespan 9 10 11 12)))
    (bytespan-find/u8 bsp
      (lambda (elem) (fx=? 11 elem))))             2
  ;; ----------------------- charspan -------------------------------------
  (charspan #\1 #\2 #\3)                           ,(string->charspan* "123")
  (list->charspan '(#\i #\j #\k #\l))              ,(string->charspan* "ijkl")
  (string->charspan "pqrst")                       ,(string->charspan* "pqrst")
  (string->charspan* "ouh[()&*U")                  ,(string->charspan* "ouh[()&*U")
  (charspan->string (string->charspan "pqrst"))    "pqrst"
  (charspan-length (charspan #\a #\b #\c))         3
  (charspan-capacity-back (charspan #\a #\b #\c))  3
  (charspan-empty? (charspan))                     #t
  (charspan-empty? (charspan #\~))                 #f
  (charspan-back (charspan #\{ #\\))               #\\
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
    (charspan-insert-front! sp #\{ #\~) sp)        ,(string->charspan* "{~AB")
  (let ((sp (charspan #\4 #\5 #\6)))
    (charspan-insert-back! sp #\7 #\8) sp)         ,(string->charspan* "45678")
  (let ((sp (string->charspan "qwerty")))
    (charspan-erase-front! sp 1) sp)               ,(string->charspan* "werty")
  (let ((sp (string->charspan "asdfuiop")))
    (charspan-erase-back! sp 3) sp)                ,(string->charspan* "asdfu")
  (let ((sp (charspan #\@ #\a #\b #\c)))
    (charspan-find sp
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
  (charline-find/left
    (string->charline* "qwerty=<>")
    9
    (lambda (ch) (char=? ch #\=)))                 6
  (let ((line (string->charline* "foo/bar"))
        (sp   (span))
        (pred (lambda (ch) (char=? ch #\b))))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i 10) sp)
      (span-insert-back! sp (charline-find/right line i pred))))
                                                   ,(span 4 4 4 4 4 #f #f #f #f #f)
  (let ((line (string->charline* "qwerty==="))
        (sp   (span))
        (pred (lambda (ch) (char=? ch #\=))))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i 12) sp)
      (span-insert-back! sp (charline-count/left line i pred))))
                                                   ,(span 0 0 0 0 0 0 0 1 2 3 3 3)
  (let ((line (string->charline* "qwerty==="))
        (sp   (span))
        (pred (lambda (ch) (char=? ch #\=))))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i 12) sp)
      (span-insert-back! sp (charline-count/right line i pred))))
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
  (charlines-find/left (strings->charlines* "qwerty@$%" "asdf")
    4 1
    (lambda (ch) (char=? ch #\@)))                 7
  (charlines-find/right (charlines
    (string->charline* "IOHPR$\n")
    (string->charline* "ORJZX"))
    0 0
    (lambda (ch) (char=? ch #\Z)))                 10
  (charlines-count/left (strings->charlines* "abc\n" "ccc")
    4 1
    (lambda (ch) (char=? ch #\c)))                 3
  (charlines-count/right (strings->charlines* "abc\n" "ccc")
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
#|
  ;; --------------------- list -------------------------------------------
  {"(let ((ret '()))\n"
   "  (list-iterate '(a b c)\n"
   "    (lambda (elem)\n"
   "      (set! ret (cons elem ret))\n"
   ;     stop iterating if (eq? 'b elem)
   "      (not (eq? 'b elem))))\n"
   "  ret)",
   "(b a)"},
  {"(list-reverse*! (list))       ()"},
  {"(list-reverse*! (list 1))       (1)"},
  {"(list-reverse*! (list 1 2))       (2 . 1)"},
  {"(list-reverse*! (list 1 2 3 4 5 6))       (6 5 4 3 2 . 1)"},
  ;; ------------------------ hashtable -----------------------------------
  {"(vector-sort"
   "  (lambda (cell1 cell2) (< (car cell1) (car cell2)))"
   "  (hashtable-cells (eq-hashtable '(3 . C) '(2 . B) '(1 . A))))",
   "#((1 . A) (2 . B) (3 . C))"},
  {"(vector-sort"
   "  (lambda (cell1 cell2) (< (car cell1) (car cell2)))"
   "  (hashtable-cells (alist->eqv-hashtable '((1.0 . A) (2.1 . B) (3 . C)))))",
   "#((1.0 . A) (2.1 . B) (3 . C))"},
  {"(vector-sort"
   "  (lambda (cell1 cell2) (< (car cell1) (car cell2)))"
   "  (hashtable-cells (eqv-hashtable  '(3.1 . C) '(2 . B) '(1 . A))))",
   "#((1 . A) (2 . B) (3.1 . C))"},
  {"(vector-sort"
   "  (lambda (cell1 cell2) (string<? (car cell1) (car cell2)))"
   "  (hashtable-cells\n"
   "    (hashtable string-hash string=? '(\"a\" . 1) '(\"B\" . 2) '(\"+\" . 3))))",
   "#((+ . 3) (B . 2) (a . 1))"},
  {"(string-hashtable->argv\n"
   "  (hashtable string-hash string=?\n"
   "             '(\"A\" . \"X\") '(\"B\" . \"Y\") '(\"C\" . \"Z\")))",
   "#(#vu8(65 61 88 0) #vu8(66 61 89 0) #vu8(67 61 90 0))"},
  {"(let ((ret '()))\n"
   "  (hashtable-iterate (eqv-hashtable '(1.0 . A) '(2.1 . B) '(3 . C))\n"
   "    (lambda (cell)\n"
   "      (set! ret (cons cell ret))))\n"
   "  (sort!"
   "    (lambda (cell1 cell2) (< (car cell1) (car cell2)))"
   "    ret))",
   "((1.0 . A) (2.1 . B) (3 . C))"},
  ;; ------------------------ lineedit io ---------------------------------
  {"(get-string-all\n"
   "  (open-charline-input-port\n"
   "    (string->charline* \"58gu405gu*(&)\\n\"))))",
   "58gu405gu*(&)\n"},
  {"(get-string-all\n"
   "  (open-charlines-input-port\n"
   "    (charlines\n"
   "      (string->charline* \"085ug&^%\\n\")))))",
   "085ug&^%\n"},
  {"(read\n"
   "  (open-charline-input-port\n"
   "    (string->charline* \"(re8u (+ -) [* /] 'foo bar . baz)\"))))",
   "(re8u (+ -) (* /) 'foo bar . baz)"},
  {"(read\n"
   "  (open-charlines-input-port\n"
   "    (charlines\n"
   "      (string->charline* \"(urehg* (a . 'b) 12\")\n"
   "      (charline)\n"
   "      (string->charline* \"3.45e3 . #\\\m\\n)\"))))",
   "(urehg* (a quote b) 123450.0 . m)"},
  ;; ------------------------ parser scheme -------------------------------
  {"(parse-scheme-forms1 (string->parsectx"
   "  \"foo bar\"))",
   "(foo bar)"},
  ;; #; comments the next s-expr
  {"(parse-scheme-forms1 (string->parsectx"
   "  \"(foo bar) #; (a b (c)) '(d [ef g] h)\"))",
   "((foo bar) '(d (ef g) h))"},
  {"(parse-scheme-forms1 (string->parsectx"
   "  \"(a (b c . d) . e)\"))",
   "((a (b c . d) . e))"},
  {"(format #f \"~s\" (parse-scheme-forms1 (string->parsectx"
   "  \" #\\\m #\\\x7e \")))",
   "(#\m #\~)"},
  ;; Chez Scheme allows additional character names, and also octal sequences #\000 ... #\377
  {"(map char->integer (parse-scheme-forms1 (string->parsectx"
   "  \" #\\\rubout #\\\bel #\\\vt #\\\nel #\\\ls #\\\000 #\\\001 #\\\376 #\\\377 \")))",
   "(127 7 11 133 8232 0 1 254 255)"},
  ;; character literals #\xdc80 ... #\xdcff are allowed only by UTF-8b
  {"(map char->integer (parse-scheme-forms1 (string->parsectx\n"
   "  \" #\\\x20ac #\\\xdc80 #\\\xdcff \")))",
   "(8364 56448 56575)"},
  ;; string escape sequences #\xdc80; ... #\xdcff; are allowed only by UTF-8b
  {"(let ((ret '()))\n"
   "  (string-iterate\n"
   "      (car (parse-scheme-forms1 (string->parsectx"
   "                 \"\\\" \\\\x20ac; \\\\xdc80; \\\\xdcff; \\\"\")))\n"
   "    (lambda (i ch)\n"
   "      (set! ret (cons (char->integer ch) ret))))\n"
   "  (reverse! ret))\n",
   "(32 8364 32 56448 32 56575 32)"},
  {"(parse-scheme-forms1 (string->parsectx"
   "  \"(list #| '\\\" . #| ,`@# |# |#" ; nested block comments
   "      '#(a 1.0 2/3) #2(d) #vu8(1 2 3) #4vu8(9) #vfx(-1 0 2) #3vfx(4))\"))",
   "((list '#(a 1.0 2/3) #(d d) #vu8(1 2 3) #vu8(9 9 9 9) #vfx(-1 0 2) #vfx(4 4 4)))"},

  ;; invariant: {#!scheme ...} is always equivalent to (...)
  {"(parse-scheme-forms1 (string->parsectx"
   "  \"(1 2 3)\" (parsers)))",
   "((1 2 3))"},
  {"(parse-scheme-forms1 (string->parsectx"
   "  \"{#!scheme 1 2 3}\" (parsers)))",
   "((1 2 3))"},
  ;; ------------------------ parser shell1 -------------------------------
  {"(parse-shell-form1 (string->parsectx \"\")))", ""},
  {"(parse-shell-form1 (string->parsectx \"{}\")))", "(shell)"},
  {"(parse-shell-form1 (string->parsectx \"{{}}\")))", "(shell (shell))"},
  {"(parse-shell-form1 (string->parsectx \"ls -l>/dev/null&\"))", "(shell ls -l > /dev/null &)"},
  {"(parse-shell-form1 (string->parsectx \"{;foo} <log 2>&1 && bar<>baz|wc -l;;\"))",
   "(shell (shell ; foo) < log 2 >& 1 && bar <> baz | wc -l ; ;)"},
  {"(parse-shell-form1 (string->parsectx \"echo|{cat;{true}\n}&\"))",
   "(shell echo | (shell cat ; (shell true) ;) &)"},
  {"(parse-shell-form1 (string->parsectx \"ls; [foo || bar &] & echo\"))",
   "(shell ls ; (shell-subshell foo || bar &) & echo)"},
  {"(parse-shell-form1 (string->parsectx \"ls && [A=1 foo || bar &] || [B=2 echo]\"))",
   "(shell ls && (shell-subshell A = 1 foo || bar &) || (shell-subshell B = 2 echo))"},
  {"(parse-shell-form1 (string->parsectx \"ls[A-Z]?[!ax-z] .\"))",
   "(shell (shell-wildcard ls % A-Z ? %! ax-z) .)"},
  {"(parse-shell-form1 (string->parsectx \"{{{{echo|cat}}}}\"))",
   "(shell (shell (shell (shell echo | cat))))"},
  {"(parse-shell-form1 (string->parsectx\n"
   "  \"a<>/dev/null||b>/dev/zero&&!c>&2\"))",
   "(shell a <> /dev/null || b > /dev/zero && ! c >& 2)"},
  ;; test fd number [N] before redirection
  {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
   "  \"foo 0</dev/zero 1<>/dev/urandom 2<&- 3>>logfile 4>otherfile 5>&/dev/null\")))",
   "(shell \"foo\" 0 < \"/dev/zero\" 1 <> \"/dev/urandom\" 2 <& \"-\" 3 >> \"logfile\""
   " 4 > \"otherfile\" 5 >& \"/dev/null\")"},
  {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
   "  \"ls \\\"-l\\\" '.'\")))",
   "(shell \"ls\" \"-l\" \".\")"},
  {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
   "  \"ls \\\"some\\\"'file'path\")))",
   "(shell \"ls\" (shell-wildcard \"some\" \"file\" \"path\"))"},
  {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
   "  \"ls `cmd1 && cmd2 || cmd3 -arg3`\"))))",
   "(shell \"ls\" (shell-backquote \"cmd1\" && \"cmd2\" \\x7C;\\x7C; \"cmd3\" \"-arg3\"))"},
  {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
   "  \"ls $var1 \\\"$var2\\\" '$var3'\")))",
   "(shell \"ls\" (shell-env \"var1\") (shell-env \"var2\") \"$var3\")"},
  {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
   "  \"ls ${v 1} \\\"${ v 2 }\\\" '${ v 3 }'\")))",
   "(shell \"ls\" (shell-env \"v 1\") (shell-env \" v 2 \") \"${ v 3 }\")"},
  {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
   "  \"ls \\\"$var1\\\"'$var2'$var3\")))",
   "(shell \"ls\" (shell-wildcard (shell-env \"var1\") \"$var2\" (shell-env \"var3\")))"},
  {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
   "  \"ls $[cmd arg $var]\")))",
   "(shell \"ls\" (shell-backquote \"cmd\" \"arg\" (shell-env \"var\")))"},
  {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
   "  \"ls \\\"$[cmd arg $var]\\\"\")))",
   "(shell \"ls\" (shell-backquote \"cmd\" \"arg\" (shell-env \"var\")))"},
  {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
   "  \"ls '$[cmd arg $var]'\")))",
   "(shell \"ls\" \"$[cmd arg $var]\")"},
  ;; test () inside shell syntax
  {"(parse-shell-form1 (string->parsectx \"echo a || (cons 1 2)\" (parsers)))",
   "(shell echo a || (cons 1 2))"},
  ;; ------------------------ parse-forms ---------------------------------
  {"(parse-forms1\n"
   "  (string->parsectx \"\" (parsers))\n"
   "  'scheme)",
   "()"},
  {"(parse-forms1\n"
   "  (string->parsectx \"+\" (parsers))\n"
   "  'scheme)",
   "(+)"},
  {"(parse-forms1\n"
   ; #!eof is equivalent to end-of-file in the input port
   "  (string->parsectx \"'(a . b) c #!eof . ) syntax error\" (parsers))\n"
   "  'scheme)",
   "('(a . b) c)"},
  {"(values->list (parse-forms\n"
   "  (string->parsectx \"uiop asdf #!scheme xyz %%a\" (parsers))\n"
   "  'scheme))",
   "((uiop asdf xyz %%a) #<parser scheme>)"},
  {"(values->list (parse-forms\n"
   "  (string->parsectx \"uiop asdf #!scheme (xyz %%a)\" (parsers))\n"
   "  'scheme))",
   "((uiop asdf (xyz %%a)) #<parser scheme>)"},
  ;; #! not followed by [0-9A-Za-z] skips the rest of line
  {"(values->list (parse-forms\n"
   "  (string->parsectx \"qwerty #!/some/path '()\" (parsers))\n"
   "  'scheme))",
   "((qwerty) #<parser scheme>)"},
  {"(values->list (parse-forms\n"
   "  (string->parsectx \"qwerty #!/some/path\\n '()\" (parsers))\n"
   "  'scheme))",
   "((qwerty '()) #<parser scheme>)"},
  ;; ; skips the rest of line too
  {"(values->list (parse-forms\n"
   "  (string->parsectx \"uiop ; this is a comment\" (parsers))\n"
   "  'scheme))",
   "((uiop) #<parser scheme>)"},
  {"(values->list (parse-forms\n"
   "  (string->parsectx \"uiop ;\\n this is not a comment\" (parsers))\n"
   "  'scheme))",
   "((uiop this is not a comment) #<parser scheme>)"},
  {"(values->list (parse-forms\n"
   "  (string->parsectx \"`('foo ,bar ,@baz) #`(#'sfoo #,sbar #,@sbaz)\" (parsers))\n"
   "  'scheme))",
   "((`('foo ,bar ,@baz) #`(#'sfoo #,sbar #,@sbaz)) #<parser scheme>)"},
  {"(parse-forms1\n"
   "  (string->parsectx \"foo && bar || baz &\" (parsers))\n"
   "  'shell)",
   "((shell foo && bar || baz &))"},
  {"(parse-forms1\n"
   "  (string->parsectx \"ls -l >& log.txt\" (parsers))\n"
   "  'shell))",
   "((shell ls -l >& log.txt))"},
  ;; character { switches to shell parser
  {"(parse-forms1\n"
   "  (string->parsectx \"7 {ls -l >& log.txt}\" (parsers))\n"
   "  'scheme))",
   "(7 (shell ls -l >& log.txt))"},
  {"(parse-forms1\n"
   "  (string->parsectx \"(values {ls -al >> log.txt})\" (parsers))\n"
   "  'scheme))",
   "((values (shell ls -al >> log.txt)))"},
  {"(parse-forms1\n"
   "  (string->parsectx \"9 #!shell ls -al >> log.txt\" (parsers))\n"
   "  'scheme))",
   "(9 (shell ls -al >> log.txt))"},
  ;; directive #!shell switches to shell parser also inside (...)
  {"(parse-forms1\n"
   "  (string->parsectx \"(#!shell ls -al >> log.txt)\" (parsers))\n"
   "  'scheme))",
   "((shell ls -al >> log.txt))"},
  {"(parse-forms1\n"
   "  (string->parsectx \"(values foo bar #!shell baz >> log.txt; wc -l log.txt)\""
   "    (parsers))\n"
   "  'scheme))",
   "((values foo bar (shell baz >> log.txt ; wc -l log.txt)))"},
  {"(parse-forms1\n"
   "  (string->parsectx \"ls ; #!shell echo\" (parsers))\n"
   "  'shell))",
   "((shell ls ; echo))"},
  ;; #\( inside shell syntax switches to Scheme parser for a single Scheme form,
   * then continues parsing shell syntax.
   * If the #\( is the first token, and the parsed Scheme form is followed by one of:
   *   newline, semicolon, another #\( or eof,
   * then the Scheme form is compiled as-is, without wrapping it inside (shell ...)
  {"(parse-forms1\n"
   "  (string->parsectx \"(+ 1 2)\" (parsers))\n"
   "  'shell)",
   "((+ 1 2))"},
  {"(parse-forms1\n"
   "  (string->parsectx \"(+ 2 3) ; echo\" (parsers))\n"
   "  'shell)",
   "((+ 2 3) (shell echo))"},
  {"(parse-forms1\n"
   "  (string->parsectx \"(+ 4 5) (* 6 7) ; ls\" (parsers))\n"
   "  'shell)",
   "((+ 4 5) (* 6 7) (shell ls))"},
  ;
   * in shell syntax, an initial Scheme form ( ... ) is NOT followed by one of:
   *   newline, semicolon, another #\( or eof
   * => wrap the Scheme form inside the overall (shell ...) together with subsequent shell forms

  {"(parse-forms1\n"
   "  (string->parsectx \"(sh-cmd \\\"true\\\") && echo\" (parsers))\n"
   "  'shell)",
   "((shell (sh-cmd true) && echo))"},
  ;; test inserting Scheme forms inside shell syntax
  {"(parse-forms1\n"
   "  (string->parsectx \"echo (+ 8 9)\" (parsers))\n"
   "  'shell)",
   "((shell echo (+ 8 9)))"},
  {"(parse-forms1\n"
   "  (string->parsectx \"ls (apply + a `(,@b)) &\" (parsers))\n"
   "  'shell))",
   "((shell ls (apply + a `(,@b)) &))"},
  {"(parse-forms1\n"
   "  (string->parsectx \"ls (my-dir) >> log.txt\" (parsers))\n"
   "  'shell))",
   "((shell ls (my-dir) >> log.txt))"},
  {"(parse-forms1\n"
   "  (string->parsectx \"{foo; bar}\" (parsers))\n"
   "  'shell)",
   "((shell foo ; bar))"},
  {"(parse-forms1\n"
   "  (string->parsectx \"[foo; bar]\" (parsers))\n"
   "  'shell)",
   "((shell-subshell foo ; bar))"},
  ;; open bracket [ at the beginning of a command starts a subshell, not a wildcard
  {"(parse-forms1\n"
   "  (string->parsectx \"[foo] [bar]\" (parsers))\n"
   "  'shell)",
   "((shell (shell-subshell foo) (shell-subshell bar)))"},
  ;; open bracket [ not at the beginning of a command starts a wildcard, not a subshell
  {"(parse-forms1\n"
   "  (string->parsectx \"''[foo] [bar]\" (parsers))\n"
   "  'shell)",
   "((shell (shell-wildcard % foo) (shell-wildcard % bar)))"},
  {"(values->list (parse-forms\n"
   "  (string->parsectx \"ls ~; #!scheme (f a b)\" (parsers))\n"
   "  'shell))",
   "(((shell ls (shell-wildcard ~) ;) (f a b)) #<parser scheme>)"},
  ;; test multiple #!... directives
  {"(values->list (parse-forms\n"
   "  (string->parsectx \"(+ a b) #!shell ls -al >> log.txt; #!scheme foo bar\""
   "    (parsers))\n"
   "  'scheme))",
   "(((+ a b) (shell ls -al >> log.txt ;) foo bar) #<parser scheme>)"},
  ;; ------------------------ parse-paren --------------------------------
  {"(values->list (paren->values (string->paren \"{\")))", "(scheme #t 0 0 #t 1 0)"},
  {"(values->list (paren->values (string->paren \"{[(\")))", "(scheme #t 0 0 #t 3 0)"},
  {"(values->list (paren->values (paren-inner-ref (string->paren \"{\") 0)))",
   "(shell { 0 0 #f 1 0)"},
  {"(values->list (paren->values (string->paren \"{\n\")))", "(scheme #t 0 0 #t 0 1)"},
  {"(values->list (paren->values (paren-inner-ref (string->paren \"{\n\") 0)))",
   "(shell { 0 0 #f 0 1)"},
  {"(values->list (paren->values (paren-inner-ref* (string->paren \"{[(\") 0 0 0)))",
   "(scheme ( 2 0 #f 3 0)"},
  {"(string->paren \"(foo \\\"a()\\\" \\\"b[]\\\" \\\"c{}\\\" [* |2| 3])\")",
   "#<paren _(\"\" \"\" \"\" [||])_>"},
  {"(string->paren \"#\newline #\\\( #\\\) #\\\[ #\\\] #\\\{ #\\\} #\\\#\")",
   "#<paren __>"},
  {"(string->paren \"#| comment . , \\\\ |#\")", "#<paren _##_>"},
  ;; [] are grouping tokens in shell syntax, and `` are not special in lisp syntax
  {"(string->paren \"{[(``)]}\")", "#<paren _{[()]}_>"},
  ;; [] are grouping tokens in lisp syntax and `` are grouping tokens in shell syntax
  {"(string->paren \"([{``}])\")", "#<paren _([{``}])_>"},
  ;; test $( $[ ${ shell syntax )
  {"(string->paren \"{$(`{}()`)}\")", "#<paren _{(`{} ()`)}_>"},
  {"(string->paren \"{$[$({${}})]}\")", "#<paren _{[({{}})]}_>"},
  ;; test single-quoted strings in shell syntax
  {"(string->paren \"{'foo\\\"bar{}[]()``baz'}\")", "#<paren _{''}_>"},
  ;; test double-quoted strings in shell syntax
  {"(string->paren \"{\\\"foobar{}[]``${baz}\\\"}\")", "#<paren _{\"`` {}\"}_>"},
  ;; paren are not special in shell syntax inside double quoted string
  {"(string->paren \"{\\\"()\\\"}\")", "#<paren _{\"\"}_>"},
  ;; parse mismatched paren
  {"(string->paren \"'\" 'shell)", "#<paren _'" GRAY("'") "_>"},
  {"(string->paren \"([{)]}\")",
   "#<paren _([{" GRAY("(") ") " GRAY("[") "]}" GRAY("]") GRAY(")") "_>"},
  {"(string->paren \"(\\\" a\\\"\")", "#<paren _(\"\"" GRAY(")") "_>"},
  ;; the code after #!scheme is inside a nested paren with name = 'scheme
  {"(string->paren \"ls #!scheme 1 2 3\" 'shell)", "#<paren ____>"},
  {"(string->paren \"{ls ; #!scheme 1 2 3}\")", "#<paren _{{}}_>"},
  {"(string->paren \"(values '{ls; #!scheme 1 2 3})\")", "#<paren _({{}})_>"},
  {"(let ((p (string->paren \"{[a] && b]\")))\n"
   "  (list\n"
   "    (paren-find/surrounds p 0 0)\n"
   "    (paren-find/surrounds p 1 0)\n"
   "    (paren-find/surrounds p 2 0)\n"
   "    (paren-find/surrounds p 3 0)\n"
   "    (paren-find/surrounds p 4 0)\n"
   "    (paren-find/surrounds p 5 0)\n"
   "    (paren-find/surrounds p 6 0)\n"
   "    (paren-find/surrounds p 7 0)\n"
   "    (paren-find/surrounds p 8 0)\n"
   "    (paren-find/surrounds p 9 0)\n"
   "    (paren-find/surrounds p 10 0)))",
   ""
#define P0_ "#<paren _{[] " GRAY("[") "]" GRAY("}") "_> "
#define P1 "#<paren {[] " GRAY("[") "]" GRAY("}") ">"
#define P1_ P1 " "
#define P2_ "#<paren []> "
   "(" P0_ P1_ P2_ P2_ P1_ P1_ P1_ P1_ P1_ P1_ P1 ")"},
#undef P0_
#undef P1
#undef P1_
#undef P2_
  ;; -------------------------- parenmatcher -------------------------------
  {"(values->list (paren->values\n"
   "  (parenmatcher-find/at\n"
   "    (make-parenmatcher)\n"
   "    (string->parsectx \"([{``}] #| |# )\" (parsers))\n"
   "    'scheme\n"
   "    6 0)))",
   "(scheme [ 1 0 ] 6 0)"},
  {"(values->list (paren->values\n"
   "  (parenmatcher-find/surrounds\n"
   "    (make-parenmatcher)\n"
   "    (string->parsectx \"([{``)))\" (parsers))\n"
   "    'scheme\n"
   "    6 0)))",
   "(shell { 2 0 #f 8 0)"},
  ;; -------------------------- tty ---------------------------------------
  ;; (tty-size) returs a cons (width . height), or c_errno() < 0 on error
  {"(let ((sz (tty-size)))\n"
   "  (if (pair? sz)\n"
   "    (and (integer? (car sz)) (positive? (car sz))\n"
   "         (integer? (cdr sz)) (positive? (cdr sz)))\n"
   "    (and (integer? sz) (negative? sz))))\n",
   "#t"},
  ;; ------------------------- posix --------------------------------------
  {"(c-errno)", "0"},
  {"(file-type \".\" 'catch)", "dir"},
  {"(file-type \"parser/parser.ss\" 'catch)", "file"},
  {"(directory-sort! (directory-list-type \"parser\"))",
   "((. . dir) (.. . dir) (lisp-read-token.ss . file) (lisp.ss . file) (parser.ss . file)"
   " (r6rs.ss . file) (scheme.ss . file) (shell-read-token.ss . file) (shell.ss . file))"},
  ;; ------------------------- posix patterns -----------------------------
  {"(sh-pattern \"foo\" '* \".bar\" '? '% \"[a-z]\" '%! \"A-Z\")",
   "(sh-pattern foo '* .bar '? '% [a-z] '%! A-Z)"},
  {"(sh-pattern '* '% \"ch\")", "(sh-pattern '* '% ch)"},
  {"(try (sh-pattern \"foo\" \".bar\") #f (catch (ex) #t))", "#t"},
  {"(try (sh-pattern '%) #f (catch (ex) #t))", "#t"},
  {"(try (sh-pattern '%!) #f (catch (ex) #t))", "#t"},
  {"(try (sh-pattern '+) #f (catch (ex) #t))", "#t"},
  {"(sh-pattern-match?"
   "  (sh-pattern \"foo\" '? \"bar\")"
   "  \"foo.bar\")",
   "#t"},
  {"(sh-pattern-match?"
   "  (sh-pattern \"asdf\" '% \"abc.\" '%! \"a-pr-z\" \"werty\" '?)"
   "  \"asdf.qwerty.\")",
   "#t"},
  {"(try (sh-pattern-match? (sh-pattern '* '% \"ch\") \"shell.c\") (catch (ex) ex))", "#t"},
  ;; initial wildcards never match an initial dot
  {"(sh-pattern-match? (sh-pattern '? \"foo\")        \".foo\")", "#f"},
  {"(sh-pattern-match? (sh-pattern '% \" ~\" \"foo\") \".foo\")", "#f"},
  {"(sh-pattern-match? (sh-pattern '% \".\" \"foo\")  \".foo\")", "#f"},
  {"(sh-pattern-match? (sh-pattern '%! \"f\" \"foo\") \".foo\")", "#f"},
  {"(sh-pattern-match? (sh-pattern '*)                \".foo\")", "#f"},
  {"(sh-pattern-match? (sh-pattern '* \"foo\")        \".my.foo\")", "#f"},
  ;; match empty pattern
  {"(sh-pattern-match? (sh-pattern) \"\")", "#t"},
  {"(sh-pattern-match? (sh-pattern) \"o\")", "#f"},
  ;; match empty string
  {"(sh-pattern-match? (sh-pattern '*) \"\")", "#t"},
  {"(sh-pattern-match? (sh-pattern '?) \"\")", "#f"},
  {"(sh-pattern-match? (sh-pattern '% \" -~\") \"\")", "#f"},
  {"(sh-pattern-match? (sh-pattern '% \"!~\") \"\")", "#f"},
  ;; match string against '*
  {"(sh-pattern-match? (sh-pattern '*) \"uiop.def..\")", "#t"},
  {"(sh-pattern-match? (sh-pattern '*) \"\")", "#t"},
  {"(sh-pattern-match? (sh-pattern '* '*) \"\")", "#t"},
  {"(sh-pattern-match? (sh-pattern '* '* '*) \"\")", "#t"},
  {"(sh-pattern-match? (sh-pattern '* \"bar\") \"foo.bar\")", "#t"},
  {"(sh-pattern-match? (sh-pattern \"abc\" '* \"def\") \"abc...def\")", "#t"},
  {"(sh-pattern-match? (sh-pattern '* \"zzz\" '? '*) \"abc.zzz.def\")", "#t"},
  {"(sh-pattern-match? (sh-pattern '* \"xyz\" '%! \"x-z\" \"xyz\" '*) \"xyzxyz.xyz\")", "#t"},
  {"(sh-pattern-match? (sh-pattern '* '* \"abc\" '%! \".\" '* '* \"abc\" '* '*)"
   " \"abc.zzz.abc^abc\")",
   "#t"},
  ;; ------------------------- shell paths --------------------------------
  {"(sh-path-absolute? (string->charspan* \"/foo\"))", "#t"},
  {"(sh-path-absolute? (string->charspan* \"bar/\"))", "#f"},
  {"(sh-path \"//foo///bar////\")", "(string->charspan* \"//foo///bar////\")"},
  {"(sh-subpath \"//foo///bar////\")", "(string->charspan* \"/foo/bar\")"},
  {"(sh-subpath \"/foo/bar/\" \"/aaa/\" \"../baz/bbbb/\")",
   "(string->charspan* \"/foo/bar/baz/bbbb\")"},
  {"(sh-path? (string->charspan* \"../a//b/\"))", "#t"},
  {"(sh-path? (string->charspan* \"\\x0;\"))", "#f"},
  {"(sh-subpath? (string->charspan* \"../a//b/\"))", "#f"},
  {"(sh-subpath? (string->charspan* \"a//b\"))", "#f"},
  {"(sh-subpath? (string->charspan* \"a/b/\"))", "#t"},
  ;; ------------------------- shell aliases ------------------------------
  {"(begin\n"
   "  (sh-alias-set! \"test-alias-foo\" '(\"bar\" \"baz\"))\n"
   "  (sh-aliases-expand '(\"test-alias-foo\" \"123\" \"456\")))\n",
   "(bar baz 123 456)"},
  ;; ------------------------- shell job ---------------------------------
  {"(begin\n"
   "  (sh-env-set! #t \"foo\" \"bar\")\n"
   "  (cons\n"
   "    (sh-env-ref   #t \"foo\")\n"
   "    (values->list (sh-env-visibility-ref #t \"foo\"))))",
   "(bar bar private)"},
  {"(let ((j (sh-subshell (sh-cmd \"sleep\" \"1\") '\\x3B; (sh-cmd \"echo\" \"done\"))))\n"
   "  (let-values (((port get-string) (open-string-output-port)))\n"
   "    (sh-job-display j port)\n"
   "    (newline          port)\n"
   "    (sh-job-write   j port)\n"
   "    (get-string)))\n",
   "[sleep 1 ; echo done]\n"
   "(sh-subshell (sh-cmd \"sleep\" \"1\") '\\x3B; (sh-cmd \"echo\" \"done\"))"},
  {"(let ((j (sh-and (sh-or (sh-subshell (sh-cmd \"sleep\" \"1\")) (sh-cmd \"ls\"))\n"
   "                 (sh-cmd \"cd\" \"..\"))))\n"
   "  (let-values (((port get-string) (open-string-output-port)))\n"
   "    (sh-job-display j port)\n"
   "    (newline          port)\n"
   "    (sh-job-write   j port)\n"
   "    (get-string)))\n",
   "{{[sleep 1] || ls} && cd ..}\n"
   "(sh-and (sh-or (sh-subshell (sh-cmd \"sleep\" \"1\")) (sh-cmd \"ls\"))"
   " (sh-cmd \"cd\" \"..\"))"},
  {"(sh-cmd  \"echo\"  \"foo\" \" bar \")", "(sh-cmd \"echo\" \"foo\" \" bar \")"},
  {"(sh-cmd* \"ls\" (lambda (j) \".\"))", "(sh-cmd* \"ls\" #<procedure>)"},
  {"(sh-cmd* \"A\" '= \"B\" \"echo\")", "(sh-cmd* \"A\" '= \"B\" \"echo\")"},
  {"(sh-find-job 0)", "#f"},
  {"(sh-find-job 1)", "#f"},
  {"(sh-find-job #t)", "(#<global> #t)"},
#endif
#if 1
  {"(sh-run/i (sh-cmd \"true\"))", ""}, ; (void) is displayed as empty string
  {"(sh-run   (sh-cmd \"false\"))", "(failed 1)"},
  {"(sh-run   (sh-cmd \"expr\" \"0\"))", ""},
  {"(sh-run   (sh-cmd \"expr\" \"257\"))", "(failed 257)"},
  {"(sh-run/i (sh-list (sh-cmd \"false\") (sh-cmd \"true\")))\n", ""},
  {"(sh-run   (sh-list (sh-cmd \"true\") (sh-cmd \"false\")))\n", "(failed 1)"},
  {"(sh-run/i (sh-and (sh-cmd \"true\") (sh-cmd \"false\")))", "(failed 1)"},
  {"(sh-run   (sh-and (sh-cmd \"true\") (sh-cmd \"false\")))", "(failed 1)"},
  {"(sh-run/i (sh-or  (sh-cmd \"true\") (sh-cmd \"false\")))", ""},
  {"(sh-run   (sh-or  (sh-cmd \"true\") (sh-cmd \"false\")))", ""},
  {"(sh-run   (sh-or  (sh-cmd \"false\") (sh-cmd \"false\")))", "(failed 1)"},
  {"(sh-run/i (sh-not (sh-cmd \"true\")))", "(failed 1)"},
  {"(sh-run   (sh-not (sh-cmd \"true\")))", "(failed 1)"},
  {"(sh-run/i (sh-not (sh-cmd \"false\")))", ""},
  {"(sh-run   (sh-not (sh-cmd \"false\")))", ""},
  {"(let ((j (sh-and (sh-cmd \"true\") (sh-cmd \"command\" \"false\"))))\n"
   "  (sh-start j)\n"
   "  (sh-bg j)\n"
   "  (sh-fg j))\n",
   "(failed 1)"},
  {"(let ((j (sh-pipe* (sh-cmd \"true\") '\\x7C;& (sh-cmd \"command\" \"false\"))))\n"
   "  (sh-start j)\n"
   "  (sh-bg j)\n"
   "  (sh-wait j))\n",
   "(failed 1)"},
  ;; (sh-start) of a builtin, or a multijob containing (recursively) only builtins,
   * directly returns their exit status, as (sh-run) would do.
   * Reason: there is no external process started asynchronously in the background
  {"(sh-start (sh-and (sh-cmd \"true\") (sh-cmd \"false\")))\n", ;
   "(failed 1)"},
  {"(let ((j (sh-cmd \"sleep\" \"1\")))\n"
   "  (sh-start j)\n"
   "  (sh-bg j))\n",
   "(running 1)"},
  {"(sh-run (sh-subshell (sh-cmd \"true\") '\\x3B; (sh-cmd \"false\")))\n", ;
   "(failed 1)"},
  ;; ------------------------- shell syntax -------------------------------
  {"(sh-parse-datum '(shell \"wc\" \"-l\" \"myfile\" > \"mylog\" \\x3B; \"echo\" \"done\"))",
   "(sh-list (sh-cmd* wc -l myfile 1 '> mylog) '; (sh-cmd echo done))"},
  {"(sh-parse-datum '(shell \"find\" \"-type\" \"f\" \\x7C;& \"wc\" &))",
   "(sh-list (sh-pipe* (sh-cmd find -type f) '|& (sh-cmd wc)) '&)"},
  ;; (sh-parse) does not alter nested (shell "foo") and returns it verbatim
  {"(sh-parse-datum '(shell (shell \"foo\") \\x3B; \"bar\"))",
   "(sh-list (shell foo) '; (sh-cmd bar))"},
  {"(sh-parse-datum '(shell ! \"foo\" && \"bar\"))",
   "(sh-and (sh-not (sh-cmd foo)) (sh-cmd bar))"},
  ;; double negation is optimized away
  {"(sh-parse-datum '(shell ! ! \"true\"))", "(sh-cmd true)"},
  {"(sh-parse-datum '(shell ! ! ! \"false\"))", "(sh-not (sh-cmd false))"},
  {"(sh-parse-datum '(shell-subshell \"abc\" && \"def\"))",
   "(sh-subshell (sh-and (sh-cmd abc) (sh-cmd def)))"},

#define OPTION-PARENT-JOB "(($primitive 2 cons) 'same-parent-as-job job)"

#define INVOKELIB-SHELL-JOBS                                                                       \
  "(begin (($primitive 3 $invoke-library) '(schemesh shell job) '(0 7 6) 'job) "

  ;; ------------------------- shell macros -------------------------------
  {"(expand '(shell))", ;
   INVOKELIB-SHELL-JOBS "(sh-cmd))"},
  {"(expand '(shell 2 >& 1))", ;
   INVOKELIB-SHELL-JOBS "(sh-cmd* 2 '>& 1))"},
  {"(expand '(shell \"ls\" \"-l\" && \"wc\" \"-b\" \\x7C;\\x7C; \"echo\" \"error\" &))",
   INVOKELIB-SHELL-JOBS
   "(sh-list (sh-or (sh-and (sh-cmd ls -l) (sh-cmd wc -b)) (sh-cmd echo error)) '&))"},
  {"(expand '(shell \"true\" \\x7C;\\x7C; ! \"false\"))",
   INVOKELIB-SHELL-JOBS "(sh-or (sh-cmd true) (sh-not (sh-cmd false))))"},
  {"(expand '(shell-list (shell \"ls\" \"-al\" >> \"log.txt\")))",
   INVOKELIB-SHELL-JOBS "(sh-cmd* ls -al 1 '>> log.txt))"},
  {"(expand '(shell-expr (if a b c)))",
   INVOKELIB-SHELL-JOBS "(sh-cmd* builtin expr (lambda () (sh-bool (if a b c)))))"},
  {"(expand (parse-shell-form1 (string->parsectx\n"
   "  \"{{{{echo|cat}}}}\")))",
   INVOKELIB-SHELL-JOBS "(sh-pipe* (sh-cmd echo) '| (sh-cmd cat)))"},
  {"(expand (parse-shell-form1 (string->parsectx\n"
   "  \"{echo|{cat;{true}}}\")))",
   INVOKELIB-SHELL-JOBS "(sh-pipe* (sh-cmd echo) '| (sh-list (sh-cmd cat) '; (sh-cmd true))))"},
  {"(expand '(shell (shell \"ls\" & \"echo\")))",
   INVOKELIB-SHELL-JOBS "(sh-list (sh-cmd ls) '& (sh-cmd echo)))"},
  {"(expand '(shell (shell \"foo\") \\x3B; \"bar\"))",
   INVOKELIB-SHELL-JOBS "(sh-list (sh-cmd foo) '; (sh-cmd bar)))"},
  {"(expand '(shell (shell \"ls\" & \"echo\") 2 >& 1))",
   INVOKELIB-SHELL-JOBS "(sh-redirect! (sh-list (sh-cmd ls) '& (sh-cmd echo)) 2 '>& 1))"},
  {"(shell \\x3B; (shell \"foo\") \\x3B; \"bar\")",
   "(sh-list '\\x3B; (sh-cmd \"foo\") '\\x3B; (sh-cmd \"bar\"))"},
  {"(shell (shell \"ls\" & \"echo\") 2 >& 1)",
   "(sh-redirect! (sh-list (sh-cmd \"ls\") '& (sh-cmd \"echo\")) 2 '>& 1)"},
  {"(parse-shell-form1 (string->parsectx\n"
   "  \"{{foo};bar}\"))",
   "(shell (shell foo) ; bar)"},
  {"(parse-shell-form1 (string->parsectx\n"
   "  \"A=B ls\")))",
   "(shell A = B ls)"},
  {"(parse-shell-form1 (string->parsectx\n"
   "  \"{C=D echo}\")))",
   "(shell C = D echo)"},
  {"(expand (parse-shell-form1 (string->parsectx\n"
   "  \"{A=B ls}\")))",
   INVOKELIB-SHELL-JOBS "(sh-cmd* A '= B ls))"},
  {"(parse-shell-form1 (string->parsectx\n"
   "  \"{FOO=$BAR/subdir echo}\")))",
   "(shell FOO = (shell-wildcard (shell-env BAR) /subdir) echo)"},
  {"(expand '(shell-wildcard *))", ;
   INVOKELIB-SHELL-JOBS "(lambda (job) (sh-wildcard job '*)))"},
  {"(expand '(shell-wildcard ?))", ;
   INVOKELIB-SHELL-JOBS "(lambda (job) (sh-wildcard job '?)))"},
  {"(expand '(shell-wildcard ~))", ;
   INVOKELIB-SHELL-JOBS "(lambda (job) (sh-wildcard job '~)))"},
  {"(expand '(shell-wildcard \"a\" (shell-wildcard ~ \"b/\" *)"
   " ? % \"def\" %! \"ghi\"))", ;
   INVOKELIB-SHELL-JOBS "(lambda (job) (sh-wildcard job a '~ b/ '* '? '% def '%! ghi)))"},
  {"(expand (parse-shell-form1 (string->parsectx\n"
   "  \"{FOO=$BAR/subdir echo}\"))))",
   INVOKELIB-SHELL-JOBS "(sh-cmd* FOO '= (lambda (job) (sh-wildcard job"
                        " (lambda (job) (sh-env-ref job BAR)) /subdir)) echo))"},
  {"(parse-shell-form1 (string->parsectx\n"
   "  \"A=$[echo abc; echo def]\"))",
   "(shell A = (shell-backquote echo abc ; echo def))"},
  {"(parse-shell-form1 (string->parsectx\n"
   "  \"A=`echo abc; echo def`\"))",
   "(shell A = (shell-backquote echo abc ; echo def))"},
  {"(expand '(shell \"A\" = (shell-backquote \"echo\" \"abc\" \\x3B; \"echo\" \"def\")))",
   INVOKELIB-SHELL-JOBS
   "(sh-cmd* A '= (lambda (job)"
   " (sh-run/string-rtrim-newlines (sh-list (sh-cmd echo abc) '; (sh-cmd echo def))"
   " " OPTION-PARENT-JOB "))))"},
  {"(expand '(shell (shell-wildcard \"l\" \"s\")))", ;
   INVOKELIB-SHELL-JOBS "(sh-cmd* ls))"},
  {"(expand '(shell (shell-wildcard \"l\" \"s\") \".\"))",
   INVOKELIB-SHELL-JOBS "(sh-cmd* ls .))"},
  {"(expand '(shell (shell-backquote \"echo\" \"ls\")))",
   INVOKELIB-SHELL-JOBS
   "(sh-cmd* (lambda (job) (sh-run/string-rtrim-newlines (sh-cmd echo ls) " ;
   OPTION-PARENT-JOB "))))"},
  ;; test wildcards and patterns [...]
  {"(parse-shell-form1 (string->parsectx\n"
   "  \"{echo *}\")))",
   "(shell echo (shell-wildcard *))"},
  {"(parse-shell-form1 (string->parsectx\n"
   "  \"{echo .*[a-z]?.so}\")))",
   "(shell echo (shell-wildcard . * % a-z ? .so))"},
  {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
   "  \"A=* B=~ ls ~bar\"))))",
   "(shell \"A\" = \"*\" \"B\" = (shell-wildcard ~) \"ls\" (shell-wildcard ~ \"bar\"))"},
  {"(parse-shell-form1 (string->parsectx\n"
   "  \"echo ab'c'\\\"d\\\"*?[a-z]\"))",
   "(shell echo (shell-wildcard ab c d (shell-wildcard * ? % a-z)))"},
  ;; in shell syntax, = is an operator only before command name
  {"(parse-shell-form1 (string->parsectx\n"
   "  \"ls A=B\")))",
   "(shell ls A=B)"},
  {"(parse-shell-form1 (string->parsectx\n"
   "  \"{ls A=B}\")))",
   "(shell ls A=B)"},
  {"(expand (parse-shell-form1 (string->parsectx\n"
   "  \"ls A=B\"))))",
   INVOKELIB-SHELL-JOBS "(sh-cmd ls A=B))"},
  {"(parse-shell-form1 (string->parsectx\n"
   "  \"echo [ab]* ? [!z]\"))))",
   "(shell echo (shell-wildcard % ab *) (shell-wildcard ?) (shell-wildcard %! z))"},
  {"(expand (parse-shell-form1 (string->parsectx\n"
   "  \"ls [ab]*\"))))",
   INVOKELIB-SHELL-JOBS "(sh-cmd* ls (lambda (job) (sh-wildcard job '% ab '*))))"},
  {"(parse-shell-form1 (string->parsectx\n"
   "  \"echo $[foo&&bar]\"))",
   "(shell echo (shell-backquote foo && bar))"},
  {"(expand (parse-shell-form1 (string->parsectx\n"
   "  \"echo $[foo&&bar]\")))",
   INVOKELIB-SHELL-JOBS "(sh-cmd* echo (lambda (job) (sh-run/string-rtrim-newlines"
                        " (sh-and (sh-cmd foo) (sh-cmd bar)) " OPTION-PARENT-JOB "))))"},
  {"(expand (parse-shell-form1 (string->parsectx\n"
   "  \"{ls} > log.txt &\")))",
   INVOKELIB-SHELL-JOBS "(sh-list* (sh-cmd ls) 1 '> log.txt '&))"},
  {"(sh-eval (parse-shell-form1 (string->parsectx\n"
   "  \"{ls} > log.txt &\")))",
   "(sh-list (sh-cmd* \"ls\" 1 '> \"log.txt\") '&)"},
  {"(expand '(shell \"echo\" \"abc\" > \"DEL_ME\" &&"
   " \"cat\" \"DEL_ME\" && \"rm\" \"DEL_ME\"))",
   INVOKELIB-SHELL-JOBS
   "(sh-and (sh-cmd* echo abc 1 '> DEL_ME) (sh-cmd cat DEL_ME) (sh-cmd rm DEL_ME)))"},
  {"(shell \"echo\" \"abc\" > \"DEL_ME\" && \"cat\" \"DEL_ME\" && \"rm\" \"DEL_ME\")",
   "(sh-and (sh-cmd* \"echo\" \"abc\" 1 '> \"DEL_ME\")"
   " (sh-cmd \"cat\" \"DEL_ME\") (sh-cmd \"rm\" \"DEL_ME\"))"},
  ;; ------------------------- wildcard expansion -------------------------
  {"(sh-wildcard #t \"a\" \"bcd\" \"\" \"ef\")", "abcdef"},
  {"(sh-wildcard->sh-patterns '(*))", "(span (sh-pattern '*))"},
  {"(sh-wildcard->sh-patterns '(\"/\" * \".so\"))", "(span / (sh-pattern '* .so))"},
  {"(sh-wildcard->sh-patterns '(\"//abc//\" \"//def//\"))", "(span / abc/ def/)"},
  {"(sh-wildcard->sh-patterns '(\"/foo/\" * \"/\" \"/bar\"))",
   "(span / foo/ (sh-pattern '* /) bar)"},
  {"(sh-wildcard #t '* \"/\" '* \".c\")",
   "(containers/containers.c posix/posix.c shell/shell.c)"},
  {"(sh-wildcard #t \"Makefile\")", "(Makefile)"}, ; file exists => returned ad list
  {"(sh-wildcard #t \"_does_not_exist_\")", ; file does not exists => returned as string
   "_does_not_exist_"},
  ;; ------------------------- job execution ------------------------------
  ;; builtins and their exit status
  {"(sh-run (shell \"true\"))", ""},
  {"(sh-run (shell \"false\"))", "(failed 1)"},
  {"(sh-run (shell \"echo0\"))", ""},
  {"(sh-run (shell \"expr\" \"210\"))", "(failed 210)"},
  {"(sh-run (shell-subshell \"true\"))", ""},
  {"(sh-run (shell-subshell \"false\"))", "(failed 1)"},
  {"(sh-run (shell-subshell \"echo0\"))", ""},
  {"(sh-run (shell-subshell \"expr\" \"210\"))", "(failed 210)"},
  ;; (sh-run/string)
  {"(sh-run/string (shell \"echo\" (shell-wildcard (shell-env \"FOO\") \"=123\" )))", "=123\n"},
  {"(sh-run/string (shell \"echo\" \"a\"  \"b\" \"c\"))", "a b c\n"},
  {"(sh-run/string-rtrim-newlines (shell \"echo\" \" abc \"))", " abc "},
  {"(sh-run/string (shell \"FOO\" = \"abc\" \\x3B; \"echo\" (shell-env \"FOO\")))", "abc\n"},
  {"(sh-run/string (shell \"set\" \"FOO\" \"def\" \\x3B; \"set\" \"FOO\"))", "set FOO 'def'\n"},
  {"(sh-run/string (shell \"unset\" \"FOO\" \\x3B; \"set\" \"FOO\"))", ""},
  {"(sh-run/string (shell \"command\" \"echo\" \"abc\" \\x3B; \"echo\" \"def\" )))",
   "abc\ndef\n"},
  ;; test that overwriting existing environment variables works
  {"(sh-run/string (shell\n"
   "    \"FOO\" = (shell-backquote \"echo\" \"ghijk\") \\x3B;\n"
   "    \"echo\" (shell-env \"FOO\")))\n",
   "ghijk\n"},
  {"(sh-run (shell \"echo\" \"abc\" > \"DEL_ME\""
   " && \"cat\" \"DEL_ME\" > \"/dev/null\""
   " && \"rm\" \"DEL_ME\"))",
   ""},
  {"(sh-run/string (shell"
   "    \"echo\" \"a\" \"b\" \"c\" > \"DEL_ME\""
   " && \"cat\" \"DEL_ME\""
   " && \"rm\" \"DEL_ME\""
   " && \"echo\" \"ok\""
   " \\x7C;\\x7C; \"echo\" \"error\"))",
   "a b c\nok\n"},
  {"(sh-run/string (shell \"echo\" \"foo  bar\\n asdf\" \\x7C; \"grep\" \"asd\" \\x3B; \"echo\" "
   "\"ok\"))",
   " asdf\nok\n"},
  {"(sh-run (shell \"echo\" \"xyz\" \\x7C;"
   " (shell \"command\" \"true\" && \"grep\" \"abc\" > \"/dev/null\")))",
   "(failed 1)"},
  {"(format #f \"~s\" (sh-run/string (shell \"echo0\" \"def\" \"gh\" \"i\" \"\")))",
   "\"def\\x0;gh\\x0;i\\x0;\\x0;\""},
  {"(format #f \"~s\" (sh-run/string (shell \"split-at-0\" \"echo\" (shell-backquote \"echo0\" "
   "\"jkl\" \"mn\" \"o\" "
   "\"\"))))",
   "\"jkl mn o \\n\""},
  ;; run builtin in a subprocess
  {"(sh-run"
   "  (sh-cmd \"false\") '(spawn? . #t))",
   "(failed 1)"},
  {"(let ((j (sh-cmd \"false\")))"
   "  (sh-start j '(spawn? . #t))"
   "  (sh-wait j))",
   "(failed 1)"},
  ;; run a pipe in current shell
  {"(sh-run (shell"
   "  \"command\" \"true\" \\x7C;"
   "  \"false\"))",
   "(failed 1)"},
  {"(sh-run (shell"
   "  \"false\" \\x7C;"
   "  \"command\" \"true\" \\x7C;"
   "  \"expr\" \"17\"))",
   "(failed 17)"},
  ;; run a pipe in a subshell
  {"(sh-run (shell-subshell"
   "  \"builtin\" \"true\" \\x7C;"
   "  \"builtin\" \"command\" \"false\" \\x7C;"
   "  \"global\"  \"expr\" \"19\"))",
   "(failed 19)"},
  ;; ------------------------- sh-read ------------------------------------
  {"(sh-read-string* \"#!/some/path some-arg\\n(display (+ 1 2)) {ls}\""
   "  'scheme #t)",
   "(begin (display (+ 1 2)) (sh-run (shell ls)))"},
  {"(sh-read-string* \"#!/some/other/path\\n(display (* 3 4)); ls\""
   "  'shell #t)",
   "(begin (display (* 3 4)) (sh-run (shell ls)))"},
  {"(sh-read-file \"utils/test_file.ss\")",
   "(begin (define (fib n)"
   " (let %fib ((i n))"
   " (if (fx>? i 2) (fx+ (%fib (fx1- i)) (%fib (fx- i 2))) 1))) "
   "(sh-run (shell ; FOO = bar ;)))"},
  {"(sh-read-file \"utils/test_file.sh\")",
   "(begin (sh-run (shell ; ;"
   " BAR =  ; foo a b c | bar (shell-env BAR)"
   " && (shell echo (shell-backquote baz --quiet) < /dev/null 2 >& 1 || fail --verbose) ; ;)) "
   "(set! a 42))"},
  ;; ------------------------- repl ---------------------------------------
  ;; {"(expand-omit-library-invocations #t)", ""}, ; avoid, requires Chez Scheme >= 10.0.0
  {"(values->list (repl-parse\n"
   "  (string->parsectx \"(+ 2 3) (values 7 (cons 'a 'b))\" (parsers))\n"
   "  'scheme))\n",
   "(((+ 2 3) (values 7 (cons 'a 'b))) #<parser scheme>)"},
  {"(values->list (repl-parse\n"
   "  (string->parsectx \"ls -l | wc -b && echo ok || echo error &\" (parsers))\n"
   "  'shell))\n",
   "(((shell ls -l | wc -b && echo ok || echo error &)) #<parser shell>)"},
  {"(values->list (repl-parse\n"
   "  (string->parsectx \"(values '{})\" (parsers))\n"
   "  'scheme))\n",
   "(((values '(shell))) #<parser scheme>)"},
  {"(values->list (repl-parse\n"
   "  (string->parsectx \"(values '{ls; #!scheme 1 2 3})\" (parsers))\n"
   "  'scheme))\n",
   ; ugly result, and not very useful
   "(((values '(shell ls ; 1 2 3))) #<parser scheme>)"},
  {"(values->list (repl-parse\n"
   "  (string->parsectx \"(1 2 3)\" (parsers))\n"
   "  'scheme))\n",
   "(((1 2 3)) #<parser scheme>)"},
  {"(values->list (repl-parse\n"
   "  (string->parsectx \"#!scheme 1 2 3\" (parsers))\n"
   "  'shell))\n",
   "((1 2 3) #<parser scheme>)"},
  {"(values->list (repl-parse\n"
   "  (string->parsectx \"1 2 3\" (parsers))\n"
   "  'shell))\n",
   "(((shell 1 2 3)) #<parser shell>)"},
  {"(values->list (repl-parse\n"
   "  (string->parsectx \"{#!scheme 1 2 3}\" (parsers))\n"
   "  'scheme))\n",
   ; must return the same as previous test
   "(((1 2 3)) #<parser scheme>)"},
  {"(values->list (repl-parse\n"
   "  (string->parsectx \"{#!scheme 1 2 3}\" (parsers))\n"
   "  'shell))\n",
   ; ideally would return the same as previous test, but deciding to omit the (shell ...) wrapper
      is tricky
   "(((shell (1 2 3))) #<parser shell>)"},
|#

)
