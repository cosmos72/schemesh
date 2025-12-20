;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; define (scheme2k posix) as a library that exports all its imported bindings
(library-reexport (scheme2k posix (0 9 2))
  (import
    (scheme2k posix dir)
    (scheme2k posix fd)
    (scheme2k posix io)
    (scheme2k posix pattern)
    (scheme2k posix pid)

    ;; by default, do not re-export bindings from (scheme2k posix replacements) or (scheme2k posix thread)
    ;; because they intentionally conflict with R6RS functions (file-exists?) (delete-file)
    ;; (get-char) (get-datum) (get-line) (get-string-all) (get-string-n) (get-string-some)
    ;; and with Chez Scheme functions for accessing the filesystem and managing threads.
    ;;
    ;; Reason for the conflict: the functions in (scheme2k posix replacements)
    ;; are intended as replacements for the default ones, and they add UTF-8b support.
    ;;
    ;; If a user wants them, they need to (import (scheme2k posix replacements)) or (import (scheme2k))
    ;;
    ;; (scheme2k posix replacements)

    (scheme2k posix rlimit)
    (scheme2k posix signal)
    (scheme2k posix status)
    (scheme2k posix tty)))
