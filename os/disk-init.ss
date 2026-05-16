;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.


;; customize how "disk-reader" objects are printed
(record-writer (record-type-descriptor disk-reader)
  (lambda (rx port writer)
    (put-string port "#<disk-reader")
    (put-string port (if (reader-eof? rx) " eof>" " ok>"))))


;; customize how "disk-entry" objects are printed
(record-writer (record-type-descriptor disk-entry)
  (lambda (e port writer)
    (put-string port "(make-disk-entry ")
                            (writer (disk-entry-id e) port)
    (put-char port #\space) (writer (disk-entry-device e) port)
    (put-char port #\space) (writer (disk-entry-mount-point e) port)
    (put-char port #\space) (writer (disk-entry-bytes-total e) port)
    (put-char port #\space) (writer (disk-entry-bytes-free e) port)
    (put-char port #\space) (writer (disk-entry-bytes-avail e) port)
    (put-char port #\space) (writer (disk-entry-inodes-total e) port)
    (put-char port #\space) (writer (disk-entry-inodes-free e) port)
    (put-char port #\space) (writer (disk-entry-inodes-avail e) port)
    (put-char port #\space) (writer (disk-entry-block-size e) port)
    (let ((dev (disk-entry-dev e)))
      (cond
        ((and (number? dev) (exact? dev))
          (put-string port " (c-make-dev ") (writer (c-dev-major dev) port)
          (put-char port #\space)           (writer (c-dev-minor dev) port)
          (put-string port ")"))
        (else
          (put-char port #\space) (writer dev port))))
    (put-string port ")")))


(let* ((rtd (record-type-descriptor disk-entry))
       (type-sym (record-type-name rtd))
       (tag-disk-entry 241))

  ;; customize visible reflect fields for `disk-entry` objects.
  ;; register a deserializer that does NOT call (make-disk-entry), because it would alters incoming fields order
  (reflect-info-set! rtd (make-reflect-info-autodetect rtd type-sym) type-sym deserialize-disk-entry)

  ;; customize how `wire` library serializes/deserializes `disk-entry` objects
  (wire-register-rtd-reflect rtd tag-disk-entry make-disk-entry))
