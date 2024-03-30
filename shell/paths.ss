;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh shell paths (0 1))
  (export sh-path sh-path-absolute? sh-path-append! sh-path-concat sh-path-iterate sh-path-normalize)
  (import
    (rnrs)
    (only (chezscheme) fx1+ fx1- void)
    (only (schemesh bootstrap) while)
    (schemesh containers charspan)
    (only (schemesh containers misc) list-iterate))


;; convert a list of strings into paths, and concatenate them into a single normalized path.
;; returned path will start with "/" if and only if first string starts with "/"
(define (sh-path . strings)
  (if (null? strings)
    (charspan)
    (let ((result (sh-path-normalize (string->charspan* (car strings)))))
      (list-iterate (cdr strings)
        (lambda (str)
          (sh-path-append! result (string->charspan* str))))
      result)))


;; return #t if path starts with "/" otherwise return #f
(define (sh-path-absolute? path)
  (and (not (charspan-empty? path))
       (char=? #\/ (charspan-front path))))


;; return #t if path ends with "/" otherwise return #f
(define (path-ends-with-sep? path)
  (and (not (charspan-empty? path))
       (char=? #\/ (charspan-back path))))


(define (char-is-sep? ch)
  (char=? #\/ ch))

;; given a path represented as charspan, split its subset start...start+n
;; using "/" separator and call (proc path pos len) on each component of the path,
;; where pos is the start index of the i-th component in the path, and len is its length.
(define (sh-path-iterate path start n proc)
  (let* ((clen (charspan-length path))
         (pos  (fxmin clen (fxmax 0 start)))
         (end  (fxmin clen (fx+ pos (fxmax 0 n)))))
    (while (fx<? pos end)
      (let ((sep (or (charspan-find path pos (fx- end pos) char-is-sep?)
                     end)))
        (proc path pos (fx- sep pos))
        (set! pos (fx1+ sep))))))

;; given a path represented as charspan,
;; return the length of its parent path.
;; returned length includes the final "/" ONLY if it's the only character.
(define (path-parent-len path)
  (let ((pos (charspan-rfind path 0 (charspan-length path) char-is-sep?)))
    (cond
      ((not pos)     0) ;; parent of relative path "foo" is the empty string
      ((fxzero? pos) 1) ;; keep "/" because it's the only character
      (else          pos))))


(define (path-is-dot? path start len)
  (and (fx=? len 1)
       (char=? #\. (charspan-ref path start))))

(define (path-is-dot-dot? path start len)
  (and (fx=? len 2)
       (char=? #\. (charspan-ref path start))
       (char=? #\. (charspan-ref path (fx1+ start)))))


;; append suffix to filesystem path prefix, both represented as charspan.
;; prefix will be modified in-place.
;; prefix must be already normalized, i.e. if splitted with "/" separator,
;; no element can be "." or ".."
;; suffix does not need to be normalized.
;; If prefix is not "/" but ends with "/", its final "/" is ignored.
;; If suffix is "/" or ends with "/", its final "/" is ignored.
;; returns (void), as usual for setters.
(define (sh-path-append! prefix suffix)
  (let ((prefix-len (trim-path-prefix-len prefix))
        (suffix-len (trim-path-suffix-len suffix)))
    (charspan-resize-back! prefix prefix-len)
    (sh-path-iterate suffix 0 suffix-len
      (lambda (suffix start len)
        (cond
          ((or (fxzero? len) (path-is-dot? suffix start len))
            (void))
          ((path-is-dot-dot? suffix start len)
            (charspan-resize-back! prefix (path-parent-len prefix)))
          (#t
            (unless (path-ends-with-sep? prefix)
              (charspan-insert-back! prefix #\/))
            (charspan-insert-back/cspan! prefix suffix start len)))))))


(define (trim-path-prefix-len path)
  (let ((len (charspan-length path)))
    (if (and (fx>? len 1)
             (char=? #\/ (charspan-back path)))
      (fx1- len)
      len)))

(define (trim-path-suffix-len path)
  (let ((len (charspan-length path)))
    (if (and (fx>? len 0)
             (char=? #\/ (charspan-back path)))
      (fx1- len)
      len)))

;; concatenate filesystem paths prefix and suffix, both represented as charspan.
;; prefix must be already normalized, i.e. if splitted with "/" separator,
;; no element can be "." or ".."
;; suffix does not need to be normalized.
;; If prefix is not "/" but ends with "/", its final "/" is ignored.
;; If suffix is "/" or ends with "/", its final "/" is ignored.
;; returns a new path, containing the concatenation of prefix and suffix.
(define (sh-path-concat prefix suffix)
  (let ((result     (make-charspan 0))
        (prefix-len (charspan-length prefix)))
    (charspan-reserve-back! result (fx+ prefix-len (charspan-length suffix)))
    (charspan-insert-back/cspan! result prefix 0 prefix-len)
    (sh-path-append! result suffix)
    result))


;; normalize filesystem path, represented as charspan.
;; i.e. it applies the effect of "." and ".." components in path.
;; returns a new path, which will end with "/" only if it's the only character
(define sh-path-normalize
  (let ((root  (string->charspan* "/"))
        (empty (string->charspan* "")))
    (lambda (path)
      (sh-path-concat (if (sh-path-absolute? path) root empty)
                      path))))



) ; close library
