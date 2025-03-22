
;; test file read by (sh-read-file) in tests executed by test.c
;; contains some random Scheme source that is only read, not compiled or evaluated

#!r6rs
(define (fib n)
  (let %fib ((i n))
    (if (fx>? i 2)
      (fx+ (%fib (fx1- i))
           (%fib (fx- i 2)))
      1)))

#!shell
FOO=bar
