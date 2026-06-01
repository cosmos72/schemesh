;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.


;; customize how "process-reader" objects are printed
(record-writer (record-type-descriptor process-reader)
  (lambda (rx port writer)
    (put-string port "#<process-reader")
    (put-string port (if (reader-eof? rx) " eof>" " ok>"))))


;; customize how "process-entry" objects are printed
(record-writer (record-type-descriptor process-entry)
  (lambda (e port writer)
    (put-string port "(make-process-entry ")
                            (writer (process-entry-pid e) port)
    (put-char port #\space) (writer (process-entry-name e) port)
    (put-char port #\space) (writer (process-entry-tty e) port)
    (put-char port #\space) (writer (process-entry-state e) port)
    (put-char port #\space) (writer (process-entry-user e) port)
    (put-char port #\space) (writer (process-entry-group e) port)
    (put-char port #\space) (writer (process-entry-uid e) port)
    (put-char port #\space) (writer (process-entry-gid e) port)
    (put-char port #\space) (writer (process-entry-ppid e) port)
    (put-char port #\space) (writer (process-entry-pgid e) port)
    (put-char port #\space) (writer (process-entry-sid e) port)
    (put-char port #\space) (writer (process-entry-mem-rss e) port)
    (put-char port #\space) (writer (process-entry-mem-virtual e) port)
    (put-char port #\space) (writer (process-entry-start-time e) port)
    (put-char port #\space) (writer (process-entry-user-time e) port)
    (put-char port #\space) (writer (process-entry-sys-time e) port)
    (put-char port #\space) (writer (process-entry-iowait-time e) port)
    (put-char port #\space) (writer (process-entry-priority e) port)
    (put-char port #\space) (writer (process-entry-threads e) port)
    (put-char port #\space) (writer (process-entry-min-fault e) port)
    (put-char port #\space) (writer (process-entry-maj-fault e) port)
    (put-string port ")")))


(let* ((rtd (record-type-descriptor process-entry))
       (type-sym (record-type-name rtd))
       (tag-process-entry 242))

  ;; customize visible reflect fields for `process-entry` objects.
  ;; register a deserializer that does NOT call (make-process-entry), because it would alters incoming fields order:
  ;; it only converts string->symbol the field process-entry-state
  (reflect-info-set! rtd (make-reflect-info-autodetect rtd type-sym) type-sym deserialize-process-entry)

  ;; customize how `wire` library serializes/deserializes `process-entry` objects
  (wire-register-rtd-reflect rtd tag-process-entry make-process-entry))
