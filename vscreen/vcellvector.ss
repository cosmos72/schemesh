;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file vscreen/all.ss


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;      define Scheme type "vcellvector", a vector of cells       ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; create a vcellvector containing n cells.
;; If vcell is specified, then vcellvector is filled with it.
;; Otherwise it is filled with (vcell #\nul).
(define make-vcellvector
  (case-lambda
    ((n)
      (make-bytevector (vcell<< n) 0))
    ((n fill)
      (unless (vcell? fill)
        (assert* 'make-vcellvector (char? fill)))
      (let ((ret (make-vcellvector n)))
        (unless (memv fill '(0 #\nul))
          (vcellvector-fill! ret 0 n fill))
        ret))))


(define (vcellvector-length clv)
  (vcell>> (bytevector-length clv)))

(define (vcellvector-empty? clv)
  (fxzero? (bytevector-length clv)))


(define vcellvector-ref
  (case-lambda
    ((clv idx)
      (bytevector-s32-native-ref clv (vcell<< idx)))
    ((clv)
      (bytevector-s32-native-ref clv 0))))


;; c must be a character or vcell
(define (vcellvector-set! clv idx c)
  (bytevector-s32-native-set! clv (vcell<< idx) (if (char? c) (vcell c) c)))

;; replace character at index idx, keeping the current colors
(define (vcellvector-update/char! clv idx ch)
  (let ((palette (vcell->vpalette (vcellvector-ref clv idx))))
    (vcellvector-set! clv idx (vcell ch palette))))

;; replace the colors at index idx, keeping the current character
(define (vcellvector-update/colors! clv idx cols)
  (let ((ch (vcell->char (vcellvector-ref clv idx))))
    (vcellvector-set! clv idx (vcell ch cols))))

;; replace the palette at index idx, keeping the current character
(define (vcellvector-update/palette! clv idx palette)
  (let ((ch (vcell->char (vcellvector-ref clv idx))))
    (vcellvector-set! clv idx (vcell ch palette))))


;; c must be a character or vcell
(define vcellvector-fill!
  (case-lambda
    ((clv start end c)
      (assert* 'vcellvector-fill! (fx<=?* 0 start end (vcellvector-length clv)))
      (unless (char? c)
        (assert* 'vcellvector-fill! (vcell? c)))
      (let* ((bstart (vcell<< start))
             (bend   (vcell<< end))
             (cl     (if (char? c) (vcell c) c))
             (u8     (bitwise-and cl #xff)))
        (if (= cl (* #x1010101 u8))
          (subbytevector-fill! clv bstart bend u8)
          (do ((bi bstart (fx+ bi vcell-bytes)))
              ((fx>=? bi bend))
            (bytevector-s32-native-set! clv bi cl)))))
    ((clv c)
      (vcellvector-fill! clv 0 (vcellvector-length clv) c))))


;; copy n cells from a vcellvector to another one
(define (vcellvector-copy! src src-start dst dst-start n)
  (assert* 'vcellvector-copy! (fx<=?* 0 src-start (fx+ src-start n) (vcellvector-length src)))
  (assert* 'vcellvector-copy! (fx<=?* 0 dst-start (fx+ dst-start n) (vcellvector-length dst)))
  (bytevector-copy! src (vcell<< src-start)
                    dst (vcell<< dst-start) (vcell<< n)))


;; copy n characters from a string to a vcellvector
(define (vcellvector-copy/string! str-src src-start clv-dst dst-start n)
  (assert* 'vcellvector-copy/string! (fx<=?* 0 src-start (fx+ src-start n) (string-length str-src)))
  (assert* 'vcellvector-copy/string! (fx<=?* 0 dst-start (fx+ dst-start n) (vcellvector-length clv-dst)))
  (do ((si src-start (fx1+ si))
       (di dst-start (fx1+ di))
       (dend (fx+ dst-start n)))
      ((fx>=? di dend))
    (vcellvector-set! clv-dst di (vcell (string-ref str-src si)))))



;; convert list of cells or characters to vcellvector
(define (list->vcellvector c-list)
  (let* ((n   (length c-list))
         (dst (make-vcellvector n)))
    (do ((i 0 (fx1+ i))
         (tail c-list (cdr tail)))
        ((null? tail) dst)
      (let* ((c  (car tail))
             (cl (if (char? c) (vcell c) c)))
        (vcellvector-set! dst i cl)))))


;; convert a portion of string to vcellvector
(define string->vcellvector
  (case-lambda
    ((str start end)
      (assert* 'string->vcellvector (fx<=?* 0 start end (string-length str)))
      (let* ((n   (fx- end start))
             (dst (make-vcellvector n)))
        (do ((si start (fx1+ si))
             (di   0   (fx1+ di)))
            ((fx>=? si n) dst)
          (vcellvector-set! dst di (vcell (string-ref str si))))))
    ((str)
      (string->vcellvector str 0 (string-length str)))))


;; display a range of vcellvector, including colors, to textual output port.
;; does NOT escape special characters
(define vcellvector-display
  (case-lambda
    ((clv start end port)
      (let ((old-palette 0))
        (do ((i start (fx1+ i)))
            ((fx>=? i end))
          (let* ((cl      (vcellvector-ref clv i))
                 (ch      (vcell->char cl))
                 (palette (vcell->vpalette cl)))
            (unless (fx=? palette old-palette)
              (vpalette-display palette port)
              (set! old-palette palette))
            (put-char port ch)))))
    ((clv start end)
      (vcellvector-display clv start end (current-output-port)))
    ((clv port)
      (vcellvector-display clv 0 (vcellvector-length clv) port))
    ((clv)
      (vcellvector-display clv 0 (vcellvector-length clv) (current-output-port)))))



;; write a range of vcellvector, including colors, to textual output port
;; escapes special characters
(define vcellvector-write
  (case-lambda
    ((clv start end port)
      (put-char port #\")
      (let ((old-palette 0))
        (do ((i start (fx1+ i)))
            ((fx>=? i end))
          (let* ((cl      (vcellvector-ref clv i))
                 (palette (vcell->vpalette cl)))
            (vcell-write cl old-palette port)
            (unless (fx=? palette old-palette)
              (set! old-palette palette)))))
      (put-char port #\"))
    ((clv start end)
      (vcellvector-write clv start end (current-output-port)))
    ((clv port)
      (vcellvector-write clv 0 (vcellvector-length clv) port))
    ((clv)
      (vcellvector-write clv 0 (vcellvector-length clv) (current-output-port)))))
