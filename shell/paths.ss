;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh shell paths (0 8 1))
  (export sh-path sh-path? sh-path-absolute? sh-path-relative?
          sh-path-append sh-path-append! sh-path-iterate
          sh-subpath sh-subpath? sh-path->subpath text->sh-path*)
  (import
    (rnrs)
    (only (chezscheme)               fx1+ fx1- void)
    (only (schemesh bootstrap)       assert* fx<=?* while)
    (schemesh containers charspan)
    (only (schemesh containers list) for-list))


;; convert a string or a charspan to charspan
(define (text->sh-path* text)
  (if (charspan? text) text (string->charspan* text)))


;; a path is a charspan representing a relative or absolute directory.
;; It cannot contain #\nul i.e. codepoint 0, because POSIX uses it as path terminator.
;; It is absolute if starts with "/", otherwise it is relative.
;; It contains zero or more components, separated by "/"
;; Each component can be an arbitrary string, including the empty string, "." or ".."
;; By construction, components cannot contain "/" because it is the component separator.
;; All other codepoints different from 0 are allowed.
;;
;; a subpath is a path with the additional constraint that components
;; cannot be "." or ".." or the empty string.
;; As a consequence, a subpath has the following constraints and guarantees:
;;  * it cannot reference ".." or "../.." etc.
;;  * it CAN be empty.
;;  * it is either absolute (if it starts with "/") or relative (in all other cases).
;;  * it cannot contain two or more consecutive "/"
;;  * it should end with "/" only if it's the only character.


;; concatenate specified strings or charspans and create a path,
;; inserting a "/" separator between each concatenation.
;; returned path will start with "/" if and only if first non-empty string or charspan
;; starts with "/"
(define (sh-path . strings-or-charspans)
  (if (null? strings-or-charspans)
    (charspan)
    (let* ((first (car strings-or-charspans))
           (result (if (charspan? first) (charspan-copy first) (string->charspan first))))
      (for-list ((item (cdr strings-or-charspans)))
        (let* ((next (if (charspan? item) item (string->charspan* item)))
               (sep-before? (path-ends-with-sep? result))
               (sep-after?  (sh-path-absolute? next)))
          (cond
            ((and sep-before? sep-after?)
              (charspan-erase-right! result 1))
            ((not (or sep-before? sep-after?))
              (charspan-insert-right! result #\/)))
          (charspan-insert-right/cspan! result next)))
      result)))


;; return #t if argument is a path i.e. a charspan that does not contain #\nul,
;; otherwise return #f
(define (sh-path? obj)
  (and (charspan? obj) (not (charspan-index/char obj #\nul))))


;; return #t if path is absolute i.e. it starts with "/" otherwise return #f
(define (sh-path-absolute? path)
  (and (not (charspan-empty? path))
       (char=? #\/ (charspan-ref path 0))))

;; return #t if path is relative i.e. it does NOT start with "/" otherwise return #f
(define (sh-path-relative? path)
  (not (sh-path-absolute? path)))

;; return #t if path ends with "/" otherwise return #f
(define (path-ends-with-sep? path)
  (and (not (charspan-empty? path))
       (char=? #\/ (charspan-ref-right path))))


;; given a charspan path, split its range [start, end) using "/" as separator
;; and call (proc path pos-start pos-end) on each component of the path,
;; where pos-start is the start index of the i-th component in the path,
;;   and pos-start is its end index.
;; stop iterating if (proc ...) returns #f.
;;
;; Returns #t if all calls to (proc path pos-start pos-end) returned truish,
;; otherwise returns #f.
(define sh-path-iterate
  (case-lambda
    ((path proc)
      (sh-path-iterate* path 0 (charspan-length path) proc))
    ((path start end proc)
      (sh-path-iterate* path start end proc))))


;; same as (sh-path-iterate*), all arguments are mandatory
(define (sh-path-iterate* path start end proc)
  (assert* 'sh-path-iterate (fx<=?* 0 start end (charspan-length path)))
  (assert* 'sh-path-iterate (procedure? proc))
  (let %loop ((pos start))
    (if (fx>=? pos end)
      #t
      (let ((sep (or (charspan-index/char path pos end #\/)
                     end)))
        (if (proc path pos sep)
          (%loop (fx1+ sep))
          #f)))))


;; given a path, return the length of its parent path.
;; returned length include the final "/" ONLY if it's the only character.
(define (path-parent-len path)
  (let ((pos (charspan-index-right/char path #\/)))
    (cond
      ((not pos)     0) ;; parent of relative path "foo" is the empty string
      ((fxzero? pos) 1) ;; keep "/" because it's the only character
      (else          pos))))


(define (path-is-dot? path start end)
  (and (fx=? start (fx1- end))
       (char=? #\. (charspan-ref path start))))

(define (path-is-dot-dot? path start end)
  (and (fx=? start (fx- end 2))
       (char=? #\. (charspan-ref path start))
       (char=? #\. (charspan-ref path (fx1+ start)))))


;; in-place concatenate a subpath (prefix) and a path (suffix).
;; prefix must be a subpath and will be modified in-place.
;; suffix must be a path and will not be modified.
;; returns (void), as usual for setters.
;; prefix will still be a subpath after this function returns.
(define (sh-path-append! prefix suffix)
  (let ((prefix-len (trim-path-prefix-len prefix))
        (suffix-len (trim-path-suffix-len suffix)))
    (charspan-resize-right! prefix prefix-len)
    (sh-path-iterate suffix 0 suffix-len
      (lambda (elem start end)
        (cond
          ((or (fx>=? start end) (path-is-dot? elem start end))
            (void))
          ((path-is-dot-dot? elem start end)
            (charspan-resize-right! prefix (path-parent-len prefix)))
          (else
            (unless (path-ends-with-sep? prefix)
              (charspan-insert-right! prefix #\/))
            (charspan-insert-right/cspan! prefix suffix start end)))))))


(define (trim-path-prefix-len path)
  (let ((len (charspan-length path)))
    (if (and (fx>? len 1)
             (char=? #\/ (charspan-ref-right path)))
      (fx1- len)
      len)))

(define (trim-path-suffix-len path)
  (let ((len (charspan-length path)))
    (if (and (fx>? len 0)
             (char=? #\/ (charspan-ref-right path)))
      (fx1- len)
      len)))

;; concatenate a subpath (prefix) and a path (suffix).
;; prefix must be a subpath and will not be modified.
;; suffix must be a path and will not be modified.
;; returns a new subpath containing the concatenation of prefix and suffix.
(define (sh-path-append prefix suffix)
  (let ((result     (make-charspan 0))
        (prefix-len (charspan-length prefix)))
    (charspan-reserve-right! result (fx+ prefix-len (charspan-length suffix)))
    (charspan-insert-right/cspan! result prefix 0 prefix-len)
    (sh-path-append! result suffix)
    result))

;; concatenate specified strings or charspans and create a sub path,
;; inserting a "/" separator between each concatenation.
;; returned subpath will start with "/" if and only if first non-empty string or charspan
;; starts with "/"
(define (sh-subpath . strings-or-charspans)
  (sh-path->subpath (apply sh-path strings-or-charspans)))


;; return #t if specified argument is a subpath,
;; i.e. if it is a charspan, and it does not contain "//",
;; and if split with "/" as separator, no component is "." or ".."
(define (sh-subpath? obj)
  (and
    (sh-path? obj)
    (let ((path obj)
          (good? #t))
      (sh-path-iterate path
        (lambda (elem start end)
          ;; only first component can be empty i.e. path can start with "/"
          (when (or (and (fx>=? start end) (not (fxzero? start)))
                    (path-is-dot? elem start end)
                    (path-is-dot-dot? elem start end))
            (set! good? #f))
          good?)) ; exits early from sh-path-iterate if good? is #f
      good?)))


;; convert a path to a subpath.
;; It applies the effect of "." and ".." components in path and removes them.
;; returns a new subpath.
(define sh-path->subpath
  (let ((root  (string->charspan* "/"))
        (empty (string->charspan* "")))
    (lambda (path)
      (sh-path-append (if (sh-path-absolute? path) root empty)
                      path))))



) ; close library
