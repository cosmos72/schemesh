#!/usr/bin/env schemesh

(sh-run {
  curl http://www.unicode.org/Public/UCD/latest/ucd/EastAsianWidth.txt --output gen-unicode-width.txt &&
  grep '^[0-9A-F]' gen-unicode-width.txt | cut -d'#' -f1 | grep '; [FW]' | cut -d';' -f1 | tr '.' ' ' > gen-unicode-wide.txt
})

; The unassigned code points in the following blocks default to "W":
;         CJK Unified Ideographs Extension A: U+3400..U+4DBF
;         CJK Unified Ideographs:             U+4E00..U+9FFF
;         CJK Compatibility Ideographs:       U+F900..U+FAFF
;  - All undesignated code points in Planes 2 and 3, whether inside or
;      outside of allocated blocks, default to "W":
;         Plane 2:                            U+20000..U+2FFFD
;         Plane 3:                            U+30000..U+3FFFD

(define table (make-bytevector #x110000 0))

(define (wide-set! lo hi)
  (bytevector-fill-range! table lo (fx1+ hi) 1))

(define (set-narrow! lo hi)
  (bytevector-fill-range! table lo (fx1+ hi) 0))

;; these are always wide
(wide-set! #x3400 #x4DBF)
(wide-set! #x4E00 #x9FFF)
(wide-set! #xF900 #xFAFF)
(wide-set! #x20000 #x3FFFF)

(define (parse-lo-hi line)
  (let ((l (string-trim-split-at-blanks line)))
    (case (length l)
      ((1)
        (let ((lo (string->number (car l) 16)))
          (values lo lo)))
      ((2)
        (let ((lo (string->number (car l) 16))
              (hi (string->number (cadr l) 16)))
          (values lo hi)))
      (else
        (values #f #f)))))

(let ((in (open-file-utf8b-input-port "gen-unicode-wide.txt")))
  (dynamic-wind
    void
    (lambda ()
      (do ((line (get-line in) (get-line in)))
          ((not (string? line)))
        (let-values (((lo hi) (parse-lo-hi line)))
          (when (and lo hi)
            (wide-set! lo hi)))))
    (lambda ()
      (close-port in))))

(define (show-range lo hi flag)
  (if (fx<=? (fx- hi lo) 180)
    (when flag
      (format #t "(wide-set! #x~x #x~x)\n" lo hi))
    (unless flag
      (format #t "\n  (char<=? #\\x~x ch #\\x~x)" lo hi))))

(define (show-table flag)
  (let %loop ((lo #f) (i 0) (n (bytevector-length table)))
    (cond
      ((fx>=? i n)
        (when lo
          (show-range lo (fx1- i) flag)))
      (lo
        (if (fxzero? (bytevector-u8-ref table i))
          (begin
            (show-range lo (fx1- i) flag)
            (%loop #f (fx1+ i) n))
          (%loop lo (fx1+ i) n)))
      (else
        (%loop (if (fxzero? (bytevector-u8-ref table i)) #f i)
               (fx1+ i) n)))))

(define (lowest-wide)
  (do ((i 0 (fx1+ i))
       (n (bytevector-length table)))
      ((or (fx>=? i n) (not (fxzero? (bytevector-u8-ref table i))))
        (if (fx>=? i n) #f i))))

(define (highest-wide)
  (do ((i (fx1- (bytevector-length table)) (fx1- i)))
      ((or (fx<? i 0) (not (fxzero? (bytevector-u8-ref table i))))
        (if (fx<? i 0) #f i))))

(sh-run {$(display "(or")   >  "gen-unicode-wide-default.ss"})
(sh-run {$(show-table #f)   >> "gen-unicode-wide-default.ss"})
(sh-run {$(display "\n)\n") >> "gen-unicode-wide-default.ss"})

(sh-run {$(show-table #t) > "gen-unicode-wide-set.ss"})

(sh-run {$(display (lowest-wide))  > "gen-unicode-wide-lowest.ss"})
(sh-run {$(display (highest-wide)) > "gen-unicode-wide-highest.ss"})

; (file-delete "gen-unicode-width.txt")
; (file-delete "gen-unicode-wide.txt")
