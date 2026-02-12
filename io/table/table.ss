;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k io table (0 9 3))
  (export make-table-writer table-writer table-writer? table-writer-eof? table-writer-close table-writer-put
          table-reflect-info-set!)
  (import
    (rnrs)
    (only (chezscheme)                       date? format fx1+ fx1- fxvector-length fxvector-ref fxvector-set!
                                             make-fxvector time? void)
    (only (scheme2k bootstrap)               assert*)
    (only (scheme2k containers charspan)     charspan)
    (only (scheme2k containers date)         date->string)
    (only (scheme2k containers ordered-hash) for-ordered-hash in-ordered-hash make-eq-ordered-hash ordered-hash-set!)
    (only (scheme2k containers span)         for-span span span-empty? span-insert-right! span-length span-ref)
    (only (scheme2k containers time)         time->string)
    (only (scheme2k io obj)                  obj-writer obj-writer-put obj-writer-eof? obj-writer-close)
    (only (scheme2k reflect)                 in-fields make-reflect-info make-reflect-info-autodetect reflect-info-fill!))


(define-record-type (table-writer %make-table-writer table-writer?)
  (parent obj-writer)
  (fields
    out                   ; textual output port
    wbuf                  ; charspan, write buffer
    (mutable widths)      ; #f or fxvector, width of each column
    (mutable lengths)     ; #f or fxvector, maximum length of text in each column
    buf                   ; span, contains rows to be written. Each row is a ordered-hash field-name -> string
    (mutable cache)       ; #f or eq-hashtable rtd -> reflect-info, set in construction or created lazily
    (mutable header?)     ; #t if we still need to write the table header
    (mutable footer?)     ; #t if we still need to write the table footer before closing this table-writer
    close-out?)           ; boolean, #t if closing the table-writer must close the underlying textual output port
  (protocol
    (lambda (args->new)
      (lambda (out close-out? cache)
        ((args->new %table-writer-put %table-writer-close)
          out (charspan) #f #f (span) #f #t #f (and close-out? #t)))))
  (nongenerative %table-writer-7c46d04b-34f4-4046-b5c7-b63753c1be40))


;; Create a table-writer that, at each call to one of (obj-writer-put) or (table-writer-put)
;; pretty-prints the received element to the underlying textual output port,
;; in ascii-art tabular format.
;;
;; Note: as per obj-writer contract, by default closing a table-writer does NOT close the underlying textual output port,
;; because it is a pre-existing, borrowed resource passed to the constructor.
;;
;; If a table-writer should take ownership of the textual output port passed to the constructor,
;; then the optional argument close-out? must be truish.
;;
;; Optional argument cache must be #f or a possibly empty eq-hashtable containing rtd -> reflect-info
(define make-table-writer
  (case-lambda
    ((out close-out? cache)
      (assert* 'make-table-writer (port? out))
      (assert* 'make-table-writer (textual-port? out))
      (assert* 'make-table-writer (output-port? out))
      (%make-table-writer out close-out? cache))
    ((out close-out?)
      (make-table-writer out close-out? #f))
    ((out)
      (make-table-writer out #f #f))
    (()
      (make-table-writer (current-output-port) #f #f))))


(define (table-writer-eof? tx)
  (assert* 'table-writer-eof? (table-writer? tx))
  (obj-writer-eof? tx))


(define (table-writer-close tx)
  (assert* 'table-writer-close (table-writer? tx))
  (obj-writer-close tx))


(define (table-writer-put tx)
  (assert* 'table-writer-put (table-writer? tx))
  (obj-writer-put tx))


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


;; search for obj's rtd in json-reflect-infos and if a reflect-info is found, return a sequence on it.
;; otherwise return a sequence on obj's reflect fields via (in-fields obj cache)
(define (in-table-fields obj cache)
  (let ((info (hashtable-ref table-reflect-infos (record-rtd obj) #f)))
    (if info
      (in-ordered-hash info)
      (in-fields obj cache))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ascii art

(define (display-header tx)
  (when (table-writer-header? tx)
    (let ((buf (table-writer-buf tx))
          (out (table-writer-out tx)))
      (unless (span-empty? buf)
        (let ((row (span-ref buf 0)))
          (for-ordered-hash ((k v row))
            (put-char out #\|)
            (display k out))
          (put-char out #\|)
          (newline out))))
    (table-writer-header?-set! tx #f)))


(define (display-row tx row)
  (let ((out (table-writer-out tx)))
    (for-ordered-hash ((k v row))
      (put-char out #\|)
      (display v out))
    (put-char out #\|)
    (newline out)))


(define (display-footer tx)
  (when (table-writer-footer? tx)
    (table-writer-footer?-set! tx #f)
    (table-writer-header?-set! tx #t)))


(define (display-all tx)
  (display-header tx)
  (for-span ((row (table-writer-buf tx)))
    (display-row tx row))
  (display-footer tx))


;; FIXME: use reflection recursively and create nested tables?
(define (datum->string datum)
  (cond
    ((string? datum)
      datum)
    ((time? datum)
      (time->string datum))
    ((date? datum)
      (date->string datum))
    (else
      (format #f "~s" datum))))


(define (obj->row tx obj)
  (let %obj->row ((seq  (in-table-fields obj (ensure-cache tx)))
                  (row  (make-eq-ordered-hash)))
    (let-values (((key value ok?) (seq)))
      (cond
        (ok?
          (ordered-hash-set! row key (datum->string value))
          (%obj->row seq row))
        (else
          row)))))


;; called by (table-writer-put) and (obj-writer-put)
(define (%table-writer-put tx obj)
  (span-insert-right! (table-writer-buf tx) (obj->row tx obj)))


;; called by (table-writer-close) and (obj-writer-close)
(define (%table-writer-close tx)
  (display-all tx)
  ;; close out only if table-writer constructor was called with truish close-out?
  (let ((out (table-writer-out tx)))
    (if (table-writer-close-out? tx)
      (close-port out)
      (flush-output-port out))))


) ; close library
