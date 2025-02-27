;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh test (0 7 6))
  (export
      run-tests)
  (import
    (except (rnrs) file-exists? delete-file)
    (rnrs base)
    (rnrs exceptions)
    (only (rnrs mutable-strings)      string-set!)
    (only (chezscheme)                display-condition format fx1+ fx1- fx/)
    (schemesh))


(define OPTION-PARENT-JOB '(($primitive 2 cons) 'same-parent-as-job job))

(define (INVOKELIB-SHELL-JOBS form)
  `(begin
    (($primitive 3 $invoke-library) '(schemesh shell job) '(0 7 6) 'job)
    ,form))


(define (run-tests file-path)
  (let* ((tests  (sh-read-file file-path))
         (n      (vector-length tests))
         (fail-n 0))

    (do ((i 0 (fx+ i 2)))
        ((fx>=? i n))
      (unless (run-test (vector-ref tests i) (vector-ref tests (fx1+ i)))
        (set! fail-n (fx1+ fail-n))))

    (unless (run-tests-utf8b)
      (set! fail-n (fx1+ fail-n)))

    (cons (fx1+ n) fail-n))) ; also count (run-tests-utf8b)


(define (run-test form expected-result)
  ; (format #t "test: ~s\n", form)
  (call/cc
    (lambda (return)
      (with-exception-handler
        (lambda (ex)
          (let-values (((port get-string) (open-string-output-port)))
            (display-condition ex port)
            (format #t "test failed:\n    Scheme code  ~s\n    exception    ~a\n    expecting    ~s\n"
              form (get-string) expected-result))
          (return #f))
        (lambda ()
          (let* ((result (sh-eval form))
                 (ok?    (equal-test? result expected-result)))
            (unless ok?
              (format #t "test failed:\n    Scheme code  ~s\n    evaluated to ~s\n    expecting    ~s\n"
                form result (simplify-expected-result expected-result)))
            ok?))))))


(define (simplify-expected-result expected-result)
  (if (and (pair? expected-result)
           (eq? 'unquote (car expected-result)))
    (cadr expected-result)
    expected-result))


(define (equal-test? result expected-result)
  (if (and (pair? expected-result)
           (eq? 'unquote (car expected-result)))
    (string=? (format #f "~s" result) (format #f "~s" (cadr expected-result)))
    (equal? result expected-result)))


(define (run-tests-utf8b)
  (let* ((maxlen 1024)
         (s (make-string maxlen))
         (ok? #t))
    (do ((i 0 (fx+ i maxlen)))
        ((fx>? i #x10FFFF) ok?)
      (unless (run-test-utf8b s i)
        (set! ok? #f)))))


(define (run-test-utf8b s first-codepoint)
  (let ((maxlen    (string-length s))
        (codepoint first-codepoint))

    (do ((pos 0 (fx1+ pos)))
        ((fx>=? pos maxlen))
      (set! codepoint (adjust-codepoint codepoint))
      (string-set! s pos (integer->char* codepoint))
      (set! codepoint (fx1+ codepoint)))

    (let ((s2 (utf8b->string (string->utf8b s))))
      (do ((pos 0 (fx1+ pos)))
          ((or (fx>=? pos maxlen)
               (not (compare-chars (string-ref s pos) (string-ref s2 pos))))
            (fx>=? pos maxlen))))))

(define (adjust-codepoint codepoint)
  (cond
    ((fx<=? #xD800 codepoint #xDC80)
       #xDC80)
    ((fx<=? #xDD00 codepoint #xE000)
       #xE000)
    ((fx>=? codepoint #x110000)
      0)
    (else
      codepoint)))

(define (compare-chars ch1 ch2)
  (if (char=? ch1 ch2)
    #t
    (begin
      (format #t "test failed:\n    (utf8b->string (string->utf8b ...)) \n    evaluated to U+0x~x\n    expecting    U+0x~x\n"
              (char->integer ch1) (char->integer ch2))
      #f)))

) ; close library
