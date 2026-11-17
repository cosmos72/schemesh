#!/usr/bin/env schemesh

(unless (file-type "gen-unicode-width0.txt")
  (sh-run
    {curl https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt --output gen-unicode-data.txt &&
     grep -e ';Cc;' -e ';Cf;' -e ';Zl;' -e ';Zp;' gen-unicode-data.txt | cut -d';' -f1 > gen-unicode-width0.txt}))


(define table (make-eqv-ordered-hash))


(define (rtrim str)
  (let %rtrim ((str str) (end (string-length str)))
    (cond
      ((fx<=? end 0)
        "")
      ((char<=? (string-ref str (fx1- end)) #\space)
        (%rtrim str (fx1- end)))
      (else
        (substring str 0 end)))))


(with-sh-closable ((in (file->port "gen-unicode-width0.txt")))
  (do ((line (get-line in) (get-line in)))
      ((not (string? line)))
    (let ((n (string->number (rtrim line) 16)))
      ;; lineedit needs #\newline to have width 1,
      ;; and codepoints < 32 are hardcoded anyway
      (when (fx>=? n 32)
        (ordered-hash-set! table n #t)))))


(define (emit port lo hi)
  (when (and lo hi)
    (format port "(charwidth-set! #x~x #x~x 0)\n" lo hi)))


(define (emit-all out)
  (let ((lo #f)
        (hi #f))
    (for-ordered-hash ((key val table))
      (cond
        ((not hi)
          (set! lo key)
          (set! hi key))
        ((fx=? (fx1+ hi) key)
          (set! hi key))
        (else
          (emit out lo hi)
          (set! lo key)
          (set! hi key))))
     (emit out lo hi)))


(with-sh-closable ((out (file->port "gen-unicode-width0-set.ss" 'write '(create truncate))))
  (emit-all out))


