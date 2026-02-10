;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k io table (0 9 3))
  (export make-table-writer table-writer table-writer? table-writer-eof? table-writer-close table-writer-put
          table-record-info-set!)
  (import
    (rnrs)
    (only (chezscheme)                       date? format fx1+ fx1- fxvector-length fxvector-ref fxvector-set!
                                             make-fxvector time? void)
    (only (scheme2k bootstrap)               assert*)
    (only (scheme2k containers charspan)     charspan)
    (only (scheme2k containers date)         date->string)
    (only (scheme2k containers ordered-hash) make-eq-ordered-hash ordered-hash-cursor ordered-hash-set!)
    (only (scheme2k containers span)         span span-insert-right! span-length)
    (only (scheme2k containers time)         time->string)
    (only (scheme2k io obj)                  obj-writer obj-writer-put obj-writer-eof? obj-writer-close)
    (only (scheme2k reflect)                 field-cursor field-cursor-next! make-record-info record-info-fill!))


(define-record-type (table-writer %make-table-writer table-writer?)
  (parent obj-writer)
  (fields
    out                   ; textual output port
    wbuf                  ; charspan, write buffer
    (mutable widths)      ; #f or fxvector, width of each column
    (mutable lengths)     ; #f or fxvector, maximum length of text in each column
    buf                   ; span, contains rows to be written. Each row is a ordered-hash field-name -> string
    (mutable cache)       ; #f or eq-hashtable rtd -> record-info, set in construction or created lazily
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
;; Optional argument cache must be #f or a possibly empty eq-hashtable containing rtd -> record-info
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
;;; record-info

;; create and return a record-info describing how to pretty-pring objects with specified rtd to tabular form.
;; uses reflection to obtain field names and accessors.
(define (make-record-info/autodetect rtd)
  (let ((info (make-record-info '())))
    (record-info-fill! info rtd)
    info))


(define table-record-infos (make-eq-hashtable))

(define (table-record-info-set! rtd field-names-and-accessors)
  (assert* 'table-record-info-set! (record-type-descriptor? rtd))
  ;; (plist? field-names-and-accessors) is already checked by (make-record-info)
  (let ((table table-record-infos)
        (info (if (null? field-names-and-accessors)
                (make-record-info/autodetect rtd)
                (make-record-info field-names-and-accessors))))
    (hashtable-set! table rtd info)))


(define (ensure-cache tx)
  (or (table-writer-cache tx)
      (let ((cache (make-eq-hashtable)))
        (table-writer-cache-set! tx cache)
        cache)))


;; search for obj's rtd in json-record-infos and if a record-info is found, return a cursor on it.
;; otherwise return a cursor on obj's reflect fields via (field-cursor obj cache)
(define (table-record-info-cursor obj cache)
  (let ((info (hashtable-ref table-record-infos (record-rtd obj) #f)))
    (if info
      (ordered-hash-cursor info)
      (field-cursor obj cache))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ascii art

(define (put-header tx)
  (void))


(define (put-footer tx)
  (void))


(define (put-row tx row)
  (void))


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
  (let ((iter (table-record-info-cursor obj (ensure-cache tx)))
        (row  (make-eq-ordered-hash)))
    (do ((cell (field-cursor-next! iter) (field-cursor-next! iter)))
        ((not cell) row)
      (ordered-hash-set! row (car cell) (datum->string (cdr cell))))))


;; called by (table-writer-put) and (obj-writer-put)
(define (%table-writer-put tx obj)
  (if (table-writer-header? tx)
    (let ((buf (table-writer-buf tx)))
      (span-insert-right! buf (obj->row tx obj))
      (when (fx>=? (span-length buf) 4)
        (put-header tx)))
    (put-row tx (obj->row tx obj))))


;; called by (table-writer-close) and (obj-writer-close)
(define (%table-writer-close tx)
  (let ((out (table-writer-out tx)))
    (when (table-writer-footer? tx)
      (put-footer tx)
      (table-writer-footer?-set! tx #f)
      (table-writer-header?-set! tx #t))
    ;; close out only if table-writer constructor was called with truish close-out?
    (if (table-writer-close-out? tx)
      (close-port out)
      (flush-output-port out))))


) ; close library
