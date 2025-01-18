;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit charhistory io (0 1))
  (export
    charhistory-load           charhistory-save
    charhistory-load-from-path charhistory-save-to-path
    charhistory-load-from-port charhistory-save-to-port)
  (import
    (rnrs)
    (only (chezscheme)               fx1+)
    (only (schemesh bootstrap)       try catch)
    (only (schemesh containers misc) string-replace/char!)
    (only (schemesh containers utf8b) string->utf8b)
    (schemesh containers gbuffer)
    (schemesh containers charline)
    (schemesh containers charlines)
    (schemesh lineedit charhistory))


;; save charhistory to file (charhistory-path hist)
;; return #t if successful, otherwise return #f
(define (charhistory-save hist)
  (let ((path (charhistory-path hist)))
    (and path (charhistory-save-to-path hist path))))


;; save charhistory to file specified by path.
;; return #t if successful, otherwise return #f
(define (charhistory-save-to-path hist path)
  (let ((port #f))
    (try
      (dynamic-wind
        (lambda ()
          (set! port (open-file-output-port path (file-options no-fail) (buffer-mode block))))
        (lambda ()
          (charhistory-save-to-port hist port))
        (lambda ()
          (when port
            (close-port port))))
      (catch (ex)
        #f))))

;; save charhistory to specified binary output port.
;; return #t if successful, otherwise return #f
(define (charhistory-save-to-port hist port)
  (charhistory-iterate hist
    (lambda (i lines)
      (charlines-save-to-port lines port))))


;; save charlines to specified binary output port.
;; return #t if successful, otherwise return #f
(define (charlines-save-to-port lines port)
  (and
    (charlines-iterate lines
      (lambda (i line)
        (charline-save-to-port line port)))
    (begin
      (put-u8 port 10) ; returns unspecified value, may be #f
      #t)))


;; save charline to specified binary output port.
;; return #t if successful, otherwise return #f
(define (charline-save-to-port line port)
  (put-bytevector port (string->utf8b (string-replace/char! (charline->string line) #\newline #\nul))))



;; load charhistory from file (charhistory-path hist)
;; return #t if successful, otherwise return #f
(define (charhistory-load hist)
  (let ((path (charhistory-path hist)))
    (and path (charhistory-load-from-path hist path))))


(define (charhistory-load-from-path hist path)
  (let ((port #f))
    (try
      (dynamic-wind
        (lambda ()
          (set! port (open-file-input-port path (file-options) (buffer-mode block))))
        (lambda ()
          (charhistory-load-from-port hist port))
        (lambda ()
          (when port
            (close-port port))))
      (catch (ex)
        #f))))


(define (charhistory-load-from-port hist port)
  #f)


) ; close library
