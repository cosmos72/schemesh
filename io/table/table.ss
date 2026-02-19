;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k io table (0 9 3))
  (export make-table-writer table-writer table-writer?
          table-reflect-info-set!)
  (import
    (rnrs)
    (only (chezscheme)                       date? format fx1+ fx1- fx/ time? time-second void)
    (only (scheme2k bootstrap)               assert* for)
    (only (scheme2k containers bytespan)     bytespan-clear! bytespan-display-left/integer! latin1-bytespan->string make-bytespan)
    (only (scheme2k containers date)         date->string)
    (only (scheme2k containers hashtable)    hashtable eq-hashtable)
    (only (scheme2k containers ordered-hash) for-ordered-hash in-ordered-hash make-eq-ordered-hash ordered-hash-empty? ordered-hash-ref ordered-hash-set!)
    (only (scheme2k containers span)         for-span span span-empty? span-insert-right! span-length span-ref)
    (only (scheme2k containers time)         time->string)
    (only (scheme2k io obj)                  writer writer-put writer-eof? writer-close)
    (only (scheme2k reflect)                 in-fields make-reflect-info make-reflect-info-autodetect reflect-info-fill!))


(define-record-type column
  (fields
    name                  ; field-name, converted to string
    align                 ; symbol, 'right if column must be right-aligned
    (mutable maxlen)      ; maximum length of strings in this column
    (mutable width))      ; chosen column width
  (nongenerative %table-column-7c46d04b-34f4-4046-b5c7-b63753c1be40))


(define-record-type (table-writer %make-table-writer table-writer?)
  (parent writer)
  (fields
    out                   ; textual output port
    theme                 ; 'basic or 'default
    colors                ; number of colors. one of: #f 8 16 256 #t, where #f means no colors and #t means truecolor i.e. 16 millions
    cols                  ; ordered-hash field-name -> column
    rows                  ; span, contains rows to be written. Each row is a ordered-hash field-name -> string
    wbuf                  ; bytespan, write buffer
    (mutable cache)       ; #f or eq-hashtable rtd -> reflect-info, set in construction or created lazily
    (mutable header?)     ; #t if we still need to write the table header
    (mutable footer?)     ; #t if we still need to write the table footer before closing this table-writer
    close-out?)           ; boolean, #t if closing the table-writer must close the underlying textual output port
  (protocol
    (lambda (args->new)
      (lambda (out close-out? theme colors cache)
        ((args->new %table-writer-put %table-writer-close)
          out theme colors (make-eq-ordered-hash) (span) (make-bytespan 0)
          cache #t #f (and close-out? #t)))))
  (nongenerative %table-writer-7c46d04b-34f4-4046-b5c7-b63753c1be41))


(define-syntax _type (identifier-syntax '<type>))


;; Create a table-writer that, at each call to one of (writer-put) or (table-writer-put)
;; pretty-prints the received element to the underlying textual output port,
;; in ascii-art tabular format.
;;
;; Note: as per writer contract, by default closing a table-writer does NOT close the underlying textual output port,
;; because it is a pre-existing, borrowed resource passed to the constructor.
;;
;; If a table-writer should take ownership of the textual output port passed to the constructor,
;; then the optional argument close-out? must be truish.
;;
;; Optional argument:
;;   close-out? - if truish, closing this writer will close output-port out
;;   theme     - a symbol. Currently supported values are: 'basic 'default
;;   colors   - one of: #f 8 16 256 #t, where #f means no colors and #t means truecolor i.e. 16 millions
;;   cache   - must be #f or a possibly empty eq-hashtable containing rtd -> reflect-info
(define make-table-writer
  (case-lambda
    ((out close-out? theme colors cache)
      (assert* 'make-table-writer (port? out))
      (assert* 'make-table-writer (textual-port? out))
      (assert* 'make-table-writer (output-port? out))
      (assert* 'make-table-writer (symbol? theme))
      (assert* 'make-table-writer (memv colors '(#f 8 16 256 #t)))
      (when cache
        (assert* 'make-table-writer (hashtable? cache)))
      (%make-table-writer out close-out? theme colors cache))
    ((out close-out? theme colors)
      (make-table-writer out close-out? theme colors #f))
    ((out close-out? theme)
      (make-table-writer out close-out? theme #f #f))
    ((out)
      (make-table-writer out #f 'default #f #f))
    (()
      (make-table-writer (current-output-port) #f 'default #f #f))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reflect-info

(define table-reflect-infos (make-eq-hashtable))

(define (table-reflect-info-set! rtd type-symbol-or-proc field-names-and-accessors)
  (assert* 'table-reflect-info-set! (record-type-descriptor? rtd))
  ;; (plist? field-names-and-accessors) is already checked by (make-reflect-info)
  (let ((table table-reflect-infos)
        (info (if (null? field-names-and-accessors)
                (make-reflect-info-autodetect rtd type-symbol-or-proc)
                (make-reflect-info                type-symbol-or-proc field-names-and-accessors))))
    (hashtable-set! table rtd info)))


(define (ensure-cache tx)
  (or (table-writer-cache tx)
      (let ((cache (make-eq-hashtable)))
        (table-writer-cache-set! tx cache)
        cache)))


;; search for obj's rtd in json-reflect-infos and if a reflect-info is found, return an iterator on it.
;; otherwise return an iterator on obj's reflect fields via (in-fields obj cache)
(define (in-table-fields obj cache)
  (if (record? obj)
    (let ((info (hashtable-ref table-reflect-infos (record-rtd obj) #f)))
      (if info
        (in-ordered-hash info)
        (in-fields obj cache)))
    (in-fields obj cache)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ascii art


(define spaces (make-string 128 #\space))


(define dashes
  (let ((basic   (make-string 128 #\-))
        (default (make-string 128 #\x2500)))
    (lambda (theme)
      (case theme
        ((basic) basic)
        (else    default)))))


(define (vbar theme)
  (case theme
    ((basic) #\|)
    (else    #\x2502)))


;; both x and y must be 0, 1 or 2
(define (corner theme x y)
  (case theme
    ((basic) #\+)
    (else    (string-ref
                "\x250c;\x252c;\x2510;\x251c;\x253c;\x2524;\x2514;\x2534;\x2518;"
                (fx+ x (fx* y 3))))))


(define (put-pad out str n)
  (when (fx>? n 0)
    (let ((n0 (fxmin n (string-length str))))
      (put-string out str 0 n0)
      (put-spaces out (fx- n n0)))))


(define (put-spaces out n)
  (put-pad out spaces n))


(define (put-dashes out theme n)
  (put-pad out (dashes theme) n))


(define (put-vbar out theme)
  (put-char out (vbar theme)))


;; both x and y must be 0, 1 or 2
(define (put-corner out theme x y)
  (let ((ch (corner theme x y)))
    (unless (char=? ch #\x0)
      (put-char out ch))))


(define color-green       (begin "\x1b;[32m"))
(define color-bold-black  (begin "\x1b;[1;30m"))
(define color-bold-red    (begin "\x1b;[1;31m"))
(define color-bold-green  (begin "\x1b;[1;32m"))
(define color-bold-yellow (begin "\x1b;[1;33m"))
(define color-bold-cyan   (begin "\x1b;[1;36m"))
(define color-bold-white  (begin "\x1b;[1;37m"))


(define (put-color-header out theme)
  (put-string out color-green))


(define cell-colors-table
  (hashtable string-hash string=?
    "dir-entry"     (eq-hashtable 'name color-bold-white)
    "process-entry" (eq-hashtable 'name color-bold-white
                                  'state (hashtable string-hash string=?
                                           "D" color-bold-red
                                           "R" color-bold-green
                                           "T" color-bold-yellow
                                           "Z" color-bold-black))))


(define (put-color-cell out theme row-type k v)
  (when (string? row-type)
    (let ((colors (hashtable-ref cell-colors-table row-type #f)))
      (when colors
        (let ((color (hashtable-ref colors k #f)))
          (when color
            (let ((vcolor (cond ((not color)     #f)
                                ((string? color) color)
                                (else            (hashtable-ref color v #f)))))
              (when vcolor
                (put-string out vcolor)))))))))


(define (put-nocolor out theme)
  (put-string out "\x1b;[m"))


(define (column-put-dashes col out theme x y)
  (put-corner out theme x y)
  (let ((width (or (column-width col) (column-maxlen col))))
    (put-dashes out theme width)))


(define (table-put-dashes tx y)
  (let ((cols  (table-writer-cols  tx))
        (out   (table-writer-out   tx))
        (theme (table-writer-theme tx))
        (x     0))
    (for-ordered-hash ((k col cols))
      (column-put-dashes col out theme x y)
      (set! x 1))
    (put-corner out theme 2 y)
    (newline out)))


(define (display-header-cell tx col)
  (let* ((name   (column-name col))
         (len    (string-length name))
         (width  (or (column-width col) (column-maxlen col)))
         (pad    (fx- width len))
         (lpad   (fx/ pad 2))
         (rpad   (fx- pad lpad))
         (out    (table-writer-out tx))
         (theme  (table-writer-theme tx))
         (colors (table-writer-colors tx)))
    (put-vbar out theme)
    ;; (debugf "; display-header-cell name ~s, len ~s, width ~s, pad ~s, lpad ~s, rpad ~s" name len width pad lpad rpad)
    (put-spaces out lpad)
    (when colors
      (put-color-header out theme))
    (put-string out name)
    (when colors
      (put-nocolor out theme))
    (put-spaces out rpad)))


(define (display-header-cells tx cols)
  (for-ordered-hash ((k col cols))
    (display-header-cell tx col))
  (let ((out (table-writer-out tx)))
    (put-vbar out (table-writer-theme tx))
    (newline out)))


(define (display-header tx)
  (when (table-writer-header? tx)
    (let ((cols   (table-writer-cols   tx))
          (out    (table-writer-out    tx))
          (theme  (table-writer-theme  tx)))
      (unless (ordered-hash-empty? cols)
        (table-put-dashes tx 0)
        (display-header-cells tx cols)
        (table-put-dashes tx 1)))
    (table-writer-header?-set! tx #f)
    (table-writer-footer?-set! tx #t)))


(define (display-footer tx)
  (when (table-writer-footer? tx)
    (when (fx>=? (span-length (table-writer-rows tx)) 50)
      (let ((cols (table-writer-cols tx)))
        (unless (ordered-hash-empty? cols)
          ;; long table, repeat column names after all rows
          (table-put-dashes tx 1)
          (display-header-cells tx cols))))
    (table-put-dashes tx 2)
    (table-writer-footer?-set! tx #f)
    (table-writer-header?-set! tx #t)))


(define (display-row-cell tx col row-type k v)
  (let* ((len    (string-display-length v))
         (width  (or (column-width col) (column-maxlen col)))
         (align  (column-align col))
         (pad    (fx- width len))
         (out    (table-writer-out tx))
         (theme  (table-writer-theme tx))
         (colors (table-writer-colors tx)))
    ;; (debugf "; put-cell v ~s, len ~s, width ~s, pad ~s, v len width pad)
    (when (eq? 'right align)
      (put-spaces out pad))
    (when colors
      (put-color-cell out theme row-type k v))
    (put-string out v)
    (when colors
      (put-nocolor out theme))
    (unless (eq? 'right align)
      (put-spaces out pad))))


(define (display-row-cells tx row)
  (let ((out    (table-writer-out    tx))
        (theme  (table-writer-theme  tx))
        (colors (table-writer-colors tx))
        (cols   (table-writer-cols   tx))
        (row-type (ordered-hash-ref row _type #f)))
    (for-ordered-hash ((k col cols))
      (put-vbar out theme)
      (display-row-cell tx col row-type k (ordered-hash-ref row k "")))
    (put-vbar out theme)
    (newline out)))


(define (display-all tx)
  (let ((rows (table-writer-rows tx)))
    (unless (span-empty? rows)
      (display-header tx)
      (for-span ((row rows))
        (display-row-cells tx row))
      (display-footer tx))))


(define (integer->string tx datum)
  (let ((wbuf (table-writer-wbuf tx)))
    (bytespan-clear! wbuf)
    (bytespan-display-left/integer! wbuf datum)
    (latin1-bytespan->string wbuf)))


;; FIXME: use reflection recursively and create nested tables?
(define (datum->string tx datum)
  (cond
    ((eq? (void) datum)
      "")
    ((string? datum)
      datum)
    ((date? datum)
      (date->string datum))
    ((and (integer? datum) (exact? datum))
      (integer->string tx datum))
    ((time? datum)
      ;; (if (eq? 'time-utc (time-type datum))
      ;;  (date->string (time-utc->date datum))
      ;;  (time->string datum))
      (integer->string tx (time-second datum)))
    (else
      (format #f "~s" datum))))


(define (char-ends-escape-seq? ch)
  (or (char<=? #\@ ch #\Z)
      (char<=? #\a ch #\z)
      (char=? ch #\%)
      (char=? ch #\`)))


(define (skip-escape-seq str i n)
  (cond
    ((fx>=? i n)
      i)
    ((char-ends-escape-seq? (string-ref str i))
      (fx1+ i))
    (else
      (skip-escape-seq str (fx1+ i) n))))


(define (string-display-length str)
  (let %loop ((str str) (i 0) (n (string-length str)) (ret 0))
    (cond
      ((fx>=? i n)
        ret)
      ((char=? #\esc (string-ref str i))
        (%loop str (skip-escape-seq str (fx1+ i) n) n ret))
      (else
        (%loop str (fx1+ i) n (fx1+ ret))))))


(define (update-column tx k v str)
  (unless (eq? _type k)
    (let* ((cols (table-writer-cols tx))
           (col  (or (ordered-hash-ref cols k #f)
                     (let* ((name (symbol->string k))
                            (col  (make-column name
                                               (if (or (number? v) (time? v)) 'right 'left)
                                               (string-length name)
                                               #f)))
                       (ordered-hash-set! cols k col)
                       col))))
      (column-maxlen-set! col (fxmax (column-maxlen col)
                                     (string-display-length str))))))


(define (obj->row tx obj)
  (let ((row  (make-eq-ordered-hash)))
    (for ((k v (in-table-fields obj (ensure-cache tx))))
      (let ((str (datum->string tx v)))
        (ordered-hash-set! row k str)
        (update-column tx k v str)))
    row))


;; called by (table-writer-put) and (writer-put)
(define (%table-writer-put tx obj)
  (span-insert-right! (table-writer-rows tx) (obj->row tx obj)))


;; called by (table-writer-close) and (writer-close)
(define (%table-writer-close tx)
  (display-all tx)
  (let ((out (table-writer-out tx)))
    (if (table-writer-close-out? tx)
      ;; close out only if table-writer constructor was called with truish close-out?
      (close-port out)
      ;; otherwise only flush it
      (flush-output-port out))))


) ; close library
