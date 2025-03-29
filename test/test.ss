;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh test (0 8 2))
  (export
      run-tests)
  (import
    (except (rnrs) file-exists? delete-file
                   get-bytevector-all get-bytevector-n get-bytevector-some
                   get-char get-datum get-line get-string-all get-string-n get-u8
                   put-bytevector put-char put-datum put-string put-u8)
    (rnrs base)
    (rnrs exceptions)
    (only (rnrs mutable-strings)  string-set!)
    (only (chezscheme)            console-output-port display-condition format fx1+ fx1- fx/)
    (schemesh))


(define (run-tests file-path)
  (let* ((tests  (sh-read-file file-path))
         (vec-n  (vector-length tests))
         (test-n (fx1+ (fx/ vec-n 2))) ; also count (run-tests-utf8b)
         (fail-n 0))

    (do ((i 0 (fx+ i 2)))
        ((fx>=? i vec-n))
      (unless (run-test (vector-ref tests i) (vector-ref tests (fx1+ i)))
        (set! fail-n (fx1+ fail-n))))

    (unless (run-tests-utf8b)
      (set! fail-n (fx1+ fail-n)))

    (cons test-n fail-n)))


(define (run-test form expected-result)
  ;; (format #t "test: ~s\n" form)
  (call/cc
    (lambda (return)
      (let-values (((comparison exp-result) (parse-expected-result expected-result)))
        (with-exception-handler
          (lambda (ex)
            (let-values (((port get-string) (open-string-output-port)))
              (display-condition ex port)
              (format #t "test failed:\n    Scheme code  ~s\n    exception    ~a\n    expecting    ~s\n"
                form (get-string) exp-result))
            (return #f))
          (lambda ()
            (test-ok? comparison form exp-result)))))))


(define (parse-expected-result expected-result)
  (cond
    ((and (pair? expected-result) (eq? 'unquote (car expected-result)))
      (values 'format-s (cadr expected-result)))
    ((and (pair? expected-result) (eq? 'unquote-splicing (car expected-result)))
      (values 'format-a (cadr expected-result)))
    (else
      (values 'equal expected-result))))


(define (test-ok? comparison form exp-result)
  (let* ((result                (sh-eval form))
         (comparable-result     (test->comparable comparison result))
         (comparable-exp-result (test->comparable comparison exp-result))
         (same? (comparable-equal? comparison comparable-result comparable-exp-result)))
    (unless same?
      (if (eq? 'format-s comparison)
        (format #t "test failed:\n    Scheme code  ~s\n    evaluated to ~a\n    expecting    ~a\n"
                form comparable-result comparable-exp-result)
        (format #t "test failed:\n    Scheme code  ~s\n    evaluated to ~s\n    expecting    ~s\n"
                form comparable-result comparable-exp-result)))
    same?))


(define (test->comparable comparison result)
  (case comparison
    ((format-s)
      (format #f "~s" result))
    ((format-a)
      (format #f "~a" result))
    (else
      result)))


(define (comparable-equal? comparison comparable-result comparable-exp-result)
  (case comparison
    ((format-s format-a)
      (string=? comparable-result comparable-exp-result))
    (else
      (equal? comparable-result comparable-exp-result))))


(define (run-tests-utf8b)
  (let* ((maxlen 1024)
         (s (make-string maxlen))
         (good? #t))
    (do ((i 0 (fx+ i maxlen)))
        ((fx>? i #x10FFFF) good?)
      (unless (run-test-utf8b s i)
        (set! good? #f)))))


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
      (format (console-output-port)
         "test failed:\n    (utf8b->string (string->utf8b ...)) \n    evaluated to U+0x~x\n    expecting    U+0x~x\n"
         (char->integer ch1) (char->integer ch2))
      #f)))

) ; close library
