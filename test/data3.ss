;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;; this file contains tests and should be loaded with (sh-read-file)
;;
;; odd elements are Scheme form to evaluate, even elements are expected result
#(
  ;; -------------------------- tty --------------------------------------------
  ;; (tty-size) returs a cons (width . height), or c_errno() < 0 on error
  (let ((sz (tty-size)))
    (if (pair? sz)
      (and (integer? (car sz)) (positive? (car sz))
           (integer? (cdr sz)) (positive? (cdr sz)))
      (and (integer? sz) (negative? sz))))             #t

  ;; ------------------------- posix -------------------------------------------
  (fx<=? (c-errno) 0)                                  #t

  (countdown 0)                                        0
  (countdown 0.0)                                      0
  (countdown '(0 . 0))                                 0
  (countdown (make-time 'time-duration 0 0))           0
  (fx<? (countdown -1) 0)                              #t
  (fx<? (countdown -1.0) 0)                            #t
  (fx<? (countdown '(0 . -1)) 0)                       #t
  (fx<? (countdown (make-time 'time-duration
                              999999999 -1)) 0)        #t

  (let-values (((rfd wfd) (pipe-fds #t #t)))
    (dynamic-wind
      void
      (lambda ()
        (fd-write-u8 wfd -1)
        (fd-read-u8 rfd))
      (lambda ()
        (fd-close wfd)
        (fd-close rfd))))                              255

  (let-values (((fd1 fd2) (socketpair-fds #t #t)))
    (dynamic-wind
      void
      (lambda ()
        (fd-write-u8 fd1 -2)
        (fd-read-u8 fd2))
      (lambda ()
        (fd-close fd2)
        (fd-close fd1))))                              254

  (file-type "." '(catch))                             dir
  (file-type "parser/parser.ss" '(catch))              file
  (directory-sort!
    (directory-list "parser" '(types)))      (("." . dir) (".." . dir) ("lisp-token.ss" . file)
                                              ("lisp.ss" . file) ("parser.ss" . file) ("r6rs.ss" . file)
                                              ("scheme.ss" . file) ("shell-token.ss" . file) ("shell.ss" . file))

  ;; -------------------- obj-reader and obj-writer ----------------------------

  (let ((rx (list-reader '(qwerty asdf !@$%^&))))
    (let*-values (((obj1 ok1) (obj-reader-get rx))
                  ((obj2 ok2) (obj-reader-get rx))
                  ((obj3 ok3) (obj-reader-get rx))
                  ((obj4 ok4) (obj-reader-get rx))
                  ((obj5 ok5) (obj-reader-get rx)))
      ;; ignore obj4 and obj5, they have unspecified values
      (list obj1 ok1 obj2 ok2 obj3 ok3 ok4 ok5)))       (qwerty #t asdf #t !@$%^& #t #f #f)


  (let ((tx (list-writer)))
    (obj-writer-put tx 97)
    (obj-writer-put tx 98)
    (obj-writer-close tx)
    (obj-writer-close tx))                              (97 98)


  ;; -------------------- list-reader, filter-reader, where ----------------------------

  (let* ((r (list-reader '(1 2 3 4 5)))
         (f (where r (fxodd? @@))))
    (let*-values (((obj1 ok1) (obj-reader-get f))
                  ((obj2 ok2) (obj-reader-get f))
                  ((obj3 ok3) (obj-reader-get f))
                  ((obj4 ok4) (obj-reader-get f)))
      ;; ignore obj4, it has unspecified value
      (list obj1 obj2 obj3 ok4)))                       (1 3 5 #f)


  ;; ------------------ queue-reader and queue-writer --------------------------

  (let* ((tx (make-queue-writer))
         (rx (make-queue-reader tx)))
    (queue-writer-put tx '(1/2 . 3/4+7i))
    (let*-values (((obj1 ok1) (queue-reader-get rx))
                  ((obj2 ok2) (queue-reader-try-get rx)))
      ;; ignore obj2, it has unspecified value
      (list obj1 ok1 ok2)))                             ((1/2 . 3/4+7i) #t timeout)

  ;; ------------------- wire-reader and wire-writer ---------------------------

  (let-values (((rx tx) (wire-pipe-pair)))
    (let ((datum1 (bitwise-arithmetic-shift 1 999))) ; serializes to 132 bytes, less than pipe buffer size = 512 bytes
      (wire-writer-put tx datum1)
      (let ((datum2 (first-value-or-void (wire-reader-get rx))))
        (wire-reader-close rx)
        (wire-writer-close tx)
        (list (eqv? datum1 datum2)
              (wire-reader-eof? rx)
              (wire-writer-eof? tx)))))                 (#t #t #t)


  (let-values (((out bv-proc) (open-bytevector-output-port)))
    (let ((tx     (make-wire-writer out))
          (datum1 (bitwise-arithmetic-shift -1 9999)))
      (wire-writer-put tx datum1)
      (wire-writer-close tx) ;; also closes out
      (let* ((in     (open-bytevector-input-port (bv-proc)))
             (rx     (make-wire-reader in))
             (datum2 (first-value-or-void (wire-reader-get rx))))
        (wire-reader-close rx) ;; also closes in
        (list (eqv? datum1 datum2)
              (wire-reader-eof? rx)
              (wire-writer-eof? tx)))))                 (#t #t #t)

  ;; ---------------------------- json-reader ----------------------------------

  ;; parse only whitespace. not a valid json, but accepted by (json-reader-get)
  ;; as zero top-level json values
  (let ((rx (make-json-reader
              (open-bytevector-input-port #vu8(9 10 13 32)))))
    (second-value (json-reader-get rx)))                #f


  ;; json-reader-get-value returns the next element, no matter if it's aggregate or not
  (let ((rx (make-json-reader
              (open-bytevector-input-port
                (string->utf8b
                  ;; we parse json numbers as inexact only if number contains "e..."
                  "[1, 2.3, 2.3e0, true, false] {\"a\": \"\\u20ac\"} \"foo\"")))))
    (let* ((obj1        (json-reader-get-value rx))
           (obj2 (begin (json-reader-restart rx)
                        (json-reader-get-value rx)))
           (obj3 (begin (json-reader-restart rx)
                        (json-reader-get-value rx)))
           (obj4        (json-reader-get-value rx)))
      (list (span? obj1) obj1 obj2 obj3
            (eof-object? obj4))))                       ,(#t (span 1 23/10 2.3 #t #f) (a "\x20ac;") "foo" #t)


  (with-output-to-string
    (lambda ()
      (let loop ((rx (make-json-reader
                       (open-bytevector-input-port
                         (string->utf8b
                           ;; we parse json numbers as inexact only if number contains "e..."
                           "[0.0, 0.0e0, {\"foo\": -1}, null]"))))
                 (tx (make-json-writer)))
        (let-values (((tok ok?) (json-reader-get rx)))
          (if ok?
            (begin
              (json-writer-put tx tok)
              (loop rx tx))
            (json-writer-close tx))))))                 "[0,\n0.0e0,\n{\"foo\":-1},\nnull]\n"


  ;; (json-reader-get) and (json-reader-skip) look inside top-level arrays and return their elements one by one.
  (let ((rx (make-json-reader
              (open-bytevector-input-port
                (string->utf8b
                  ;; we parse json numbers as inexact only if number contains "e..."
                  "[1, 2.3, true, [0], {}] {\"a\": \"\\u20ac\"} \"foo\"")))))
    (let*-values (((obj1 ok1) (json-reader-get rx))
                  ((obj2 ok2) (json-reader-get rx))
                  ((obj3 ok3) (json-reader-get rx))
                  ((obj4 ok4) (json-reader-get rx))
                  ((ok_)      (json-reader-skip rx))
                  ((obj5 ok5) (json-reader-get rx))
                  ((obj6 ok6) (json-reader-get rx))
                  ((obj7 ok7) (json-reader-get rx)))
      (list obj1 ok1 obj2 ok2 obj3 ok3 obj4 ok4 obj5 ok5
            obj6 ok6 #|obj7|# ok7)))                    ,(1 #t 23/10 #t #t #t (span 0) #t (a "\x20ac;") #t "foo" #t #f)


  ;; json-reader-get also looks inside json objects (at any depth) for key "@type" and,
  ;; if the value is registered into record-json-table,
  ;; calls the registered constructor passing as the only argument the json object, converted to a plist.
  (let ((rx (make-json-reader
              (open-bytevector-input-port
                (string->utf8b
                  "[{\"@type\":\"time-utc\",\"value\":1770224910.283978890}]")))))
    (let-values (((obj ok) (json-reader-get rx)))
      (list (time? obj) obj ok)))                       ,(#t (make-time-utc 1770224910 283978890) #t)

  (let-values (((port to-string) (open-string-output-port)))
    (let ((tx (make-json-writer port)))
      (json-writer-put tx (date 9999 12 31  23 59 59  999999999  +86400))
      (json-writer-close tx)
      (let* ((str (to-string))
             (rx  (make-json-reader (open-bytevector-input-port (string->utf8b str)))))
        (first-value (json-reader-get rx)))))           ,@"(date 9999 12 31  23 59 59  999999999 +86400)"


  ;; ---------------------------- lineedit io ----------------------------------
  (read
    (open-vlines-input-port
      (vlines
        "(urehg* (a . 'b) 12"
        ""
        "3.45e3 . #\\m\n)")))                          (urehg* (a quote b) 123450.0 . #\m)

  ;; ------------------------- posix patterns -----------------------------
  (sh-pattern "foo" '* ".bar" '? '% "[a-z]" '%! "A-Z") ,@(sh-pattern "foo" '* ".bar" '? '% "[a-z]" '%! "A-Z")
  (sh-pattern '* '% "ch")                              ,@(sh-pattern '* '% "ch")
  (try (sh-pattern "foo" ".bar") #f (catch (ex) #t))   #t
  (try (sh-pattern '%)  #f (catch (ex) #t))            #t
  (try (sh-pattern '%!) #f (catch (ex) #t))            #t
  (try (sh-pattern '+)  #f (catch (ex) #t))            #t
  (sh-pattern-match? (sh-pattern
    "foo" '? "bar") "foo.bar")                         #t
  (sh-pattern-match? (sh-pattern
    "asdf" '% "abc." '%! "a-pr-z" "werty" '?)
    "asdf.qwerty.")                                    #t
  (try (sh-pattern-match? (sh-pattern
    '* '% "ch") "main.c") (catch (ex) ex))            #t

  ;; initial wildcards never match an initial dot
  (sh-pattern-match? (sh-pattern '? "foo")      ".foo")    #f
  (sh-pattern-match? (sh-pattern '% " ~" "foo") ".foo")    #f
  (sh-pattern-match? (sh-pattern '% "." "foo")  ".foo")    #f
  (sh-pattern-match? (sh-pattern '%! "f" "foo") ".foo")    #f
  (sh-pattern-match? (sh-pattern '*)            ".foo")    #f
  (sh-pattern-match? (sh-pattern '* "foo")      ".my.foo") #f
  ;; match empty pattern
  (sh-pattern-match? (sh-pattern) "")                  #t
  (sh-pattern-match? (sh-pattern) "o")                 #f
  ;; match empty string
  (sh-pattern-match? (sh-pattern '*) "")               #t
  (sh-pattern-match? (sh-pattern '?) "")               #f
  (sh-pattern-match? (sh-pattern '% " -~") "")         #f
  (sh-pattern-match? (sh-pattern '% "!~") "")          #f
  ;; match string against '*
  (sh-pattern-match? (sh-pattern '*) "uiop.def..")     #t
  (sh-pattern-match? (sh-pattern '*) "")               #t
  (sh-pattern-match? (sh-pattern '* '*) "")            #t
  (sh-pattern-match? (sh-pattern '* '* '*) "")         #t
  (sh-pattern-match? (sh-pattern '* "bar") "foo.bar")  #t
  (sh-pattern-match? (sh-pattern
     "abc" '* "def") "abc...def")                      #t
  (sh-pattern-match? (sh-pattern
     '* "zzz" '? '*) "abc.zzz.def")                    #t
  (sh-pattern-match? (sh-pattern
    '* "xyz" '%! "x-z" "xyz" '*) "xyzxyz.xyz")         #t
  (sh-pattern-match? (sh-pattern
      '* '* "abc" '%! "." '* '* "abc" '* '*)
    "abc.zzz.abc^abc")                                 #t

  ;; ------------------------- wildcard expansion -------------------------
  (wildcard1+ #t "a" "bcd" "" "ef")                    ("abcdef")
  (wildcard1+ #t '* "f" '*)                            ("Makefile" "default.nix" "reflect" "srfi")
  (wildcard->sh-patterns '(*))                       ,@(span (sh-pattern '*))
  (wildcard->sh-patterns '("/" * ".so"))             ,@(span "/" (sh-pattern '* ".so"))
  (wildcard->sh-patterns '("//abc//" "//def//"))     ,@(span "/" "abc/" "def/")
  (wildcard->sh-patterns '("/foo/" * "/" "/bar"))    ,@(span "/" "foo/" (sh-pattern '* "/") "bar")
  (wildcard #t '* "/" '* ".c")                         ("containers/containers.c" "io/http.c" "os/process.c" "posix/posix.c"
                                                         "test/test.c" "utils/benchmark_async_signal_handler.c" "utils/countdown.c")
  (wildcard1+ #t "Makefile")                           ("Makefile")
  (wildcard1+ #t "_does_not_exist_")                   ("_does_not_exist_")
  (wildcard* #t '("_does_not_exist_"))                 ()
  (wildcard* #t '("_does_not_exist_")
               '(if-no-match? string))                 "_does_not_exist_"
  (wildcard* #t '("_does_not_exist_")
               '(if-no-match? string-list))            ("_does_not_exist_")
  ;; was bugged up to commit c683bae3f0520dccb58f9fc9f2482851004171f4
  ;; fixed in commit 067bc0cf5f76b04483f2c0989d3d955868fb554f before releasing v0.9.2
  ;;
  ;; ideally we do not want to try and access root's directory,
  ;; but "root" username exists almost everywhere
  (let ((username (sh-env-ref #f "USER" "root")))
    (string=?
      (car (wildcard1+ #t '~ username "/_does_not_exist_"))
      (string-append (sh-username->homedir username)
                     "/_does_not_exist_")))            #t
  (caddr (expand '{ls [ab]*}))                         ,@(sh-cmd* "ls" (lambda (job) (wildcard1+ job '% "ab" '*)))
  (caddr (expand '(shell-wildcard *)))                 ,@(lambda (job) (wildcard1+ job '*))
  (caddr (expand '(shell-wildcard ?)))                 ,@(lambda (job) (wildcard1+ job '?))
  (caddr (expand '(shell-wildcard ~)))                 ,@(lambda (job) (wildcard1+ job '~))
  (caddr (expand '(shell-wildcard
   "a" (shell-wildcard ~ "b/" *) ? % "def" %! "ghi"))) ,@(lambda (job) (wildcard1+ job "a" '~ "b/" '* '? '% "def" '%! "ghi"))

  ;; ------------------------- job execution ------------------------------
  (sh-run (shell "true"))                              ,@"#<void>"
  (sh-run (shell "false"))                             ,(failed 1)
  (sh-run (shell "echo0"))                             ,@"#<void>"
  (sh-run (shell "status" "210"))                      ,(failed 210)
  (sh-run (shell-subshell "true"))                     ,@"#<void>"
  (sh-run (shell-subshell "false"))                    ,(failed 1)
  (sh-run (shell-subshell "echo0"))                    ,@"#<void>"
  (sh-run (shell-subshell "status" "213"))             ,(failed 213)
  ;; (sh-run/string)
  (sh-run/string (shell "echo"
    (shell-wildcard (shell-env "FOO") "=123" )))       "=123\n"
  (sh-run/string (shell "echo" "a"  "b" "c"))          "a b c\n"
  (sh-run/string-rtrim-newlines
    (shell "echo" " abc "))                            " abc "
  (sh-run/string (shell "FOO" = "abc" \x3B;
                        "echo" (shell-env "FOO")))     "abc\n"
  (sh-run/string (shell "set" "FOO" "def" \x3B;
                        "set" "FOO"))                  "set FOO 'def'\n"
  (sh-run/string (shell "unset" "FOO" \x3B;
                        "set" "FOO"))                  ""
  (sh-run/string (shell "command" "echo" "abc" \x3B;
                        "echo" "def"))                 "abc\ndef\n"
  ;; test that overwriting existing environment variables works
  (sh-run/string (shell
      "FOO" = (shell-backquote "echo" "ghijk") \x3B;
      "echo" (shell-env "FOO")))                       "ghijk\n"
  (sh-run (shell
      "echo" "abc" > "DEL_ME" &&
      "cat" "DEL_ME" > "/dev/null" &&
      "rm" "DEL_ME"))                                  ,@"#<void>"
  (sh-run/string (shell
      "echo" "a" "b" "c" > "DEL_ME" &&
      "cat" "DEL_ME" &&
      "rm" "DEL_ME" &&
      "echo" "ok" \x7C;\x7C;
      "echo" "error"))                                 "a b c\nok\n"
  (sh-run/string (shell
     "echo" "foo  bar\n asdf" \x7C;
     "grep" "asd" \x3B;
     "echo" "ok"))                                     " asdf\nok\n"
  (sh-run (shell
    "echo" "xyz" \x7C;
    "grep" "abc" > "/dev/null"))                       ,(failed 1)
  (sh-run (shell
    "echo" "xyz" \x7C;
    (shell "command" "true" &&
           "grep" "abc" > "/dev/null")))               ,(failed 1)
  (sh-run/string (shell
    "echo0" "def" "gh" "i" ""))                        "def\x0;gh\x0;i\x0;\x0;"
  (sh-run/string (shell
    "split-at-0" "echo"
      (shell-backquote "echo0" "jkl" "mn" "o" "")))    "jkl mn o \n"

  (let* ((job   {grep xyz})
         (ports (sh-start/ports job))
         (in    (car ports))
         (out   (cadr ports))
         (err   (caddr ports)))
    (put-string in "_abc_\n")
    (put-string in "_xyz_\n")
    (close-port in)
    (let ((ret (get-string-all out)))
      (close-port out)
      (close-port err)
      (sh-wait job)
      ret))                                            "_xyz_\n"

  (sh-run {
     $(display "hello") | cat |
     $(utf8b->string (fd-read-all (sh-fd 0)))})        ,(ok "hello")
  (sh-run {
     $(display "greet") | cat |
     $(get-string-all)})                               ,(ok "greet")

  ;; run builtin in a subprocess
  (sh-run (sh-cmd "false") '(spawn? #t))               ,(failed 1)
  (let ((j (sh-cmd "false")))
    (sh-start j '(spawn? #t))
    (sh-wait j))                                       ,(failed 1)
  ;; run a pipe in current shell
  (sh-run (shell
    "command" "true" \x7C;
    "false"))                                          ,(failed 1)
  (sh-run (shell
    "false" \x7C;
    "command" "true" \x7C;
    "status" "17"))                                    ,(failed 17)
  ;; run a pipe in a subshell
  (sh-run (shell-subshell
    "builtin" "true" \x7C;
    "builtin" "command" "false" \x7C;
    "global"  "status" "19"))                          ,(failed 19)

  (sh-run/i (shell (shell-expr -1 (* 7 8 9))))         ,(ok 504)
  (sh-run/i {$(vector 1 2 3)})                         ,(ok #(1 2 3))
  (sh-run/i {$(values)})                               ,(ok)
  (sh-run/i {$(values (void))})                        ,@"#<void>"
  (sh-run/i {$(values #t)})                            ,(ok #t)
  (sh-run/i {$(values #f)})                            ,(failed #f)
  (sh-run/i {$(values 4 5 6)})                         ,(ok 4 5 6)

  (sh-run/i (sh-cmd "command" "true"))                 ,@"#<void>"
  (sh-run   (sh-cmd "command" "false"))                ,(failed 1)
  (sh-run   (sh-cmd "status" "0"))                     ,@"#<void>"
  (sh-run   (sh-cmd "status" "257"))                   ,(failed 257)
  (sh-run/i {false
             true})                                    ,@"#<void>"
  (sh-run   {true
             false})                                   ,(failed 1)
  (sh-run   {true A=1 \
B=2})                                                  ,@"#<void>"
  (sh-run   {true A=3 \
             B=4})                                     ,@"#<void>"
  (sh-run/i {true && false})                           ,(failed 1)
  (sh-run   {true && false})                           ,(failed 1)
  (sh-run/i {true || false})                           ,@"#<void>"
  (sh-run   {true || false})                           ,@"#<void>"
  (sh-run   {false || false})                          ,(failed 1)
  (sh-run/i {! true})                                  ,(failed 1)
  (sh-run   {! true})                                  ,(failed 1)
  (sh-run/i {! false})                                 ,@"#<void>"
  (sh-run   {! false})                                 ,@"#<void>"
  (let ((j {true && command false}))
    (sh-start j)
    (sh-bg j)
    (sh-fg j))                                         ,(failed 1)
  (let ((j {true |& command false}))
    (sh-start j)
    (sh-bg j)
    (sh-wait j))                                       ,(failed 1)
  ;; (sh-start) of a builtin, or a multijob containing (recursively) only builtins,
  ;; directly returns their exit status, as (sh-run) would do.
  ;; Reason: there is no spawned external process to wait for.
  (sh-start {true && false})                           ,(failed 1)
  (let ((j {sleep 1}))
    (sh-start j)
    (sh-bg j))                                         ,(running 1)
  (sh-run {[true
            false]})                                   ,(failed 1)
  (sh-run $(sh-run
    { {echo a
       sleep 0
       echo b
       sleep 0
       echo c} |
     $(get-string-all)}))                              ,(ok "a\nb\nc\n")

  ;; ------------------------- sh-read ------------------------------------
  (sh-read-string* "#!/some/path some-arg\n\
    (display (+ 1 2)) {hjk}" 'scheme #t)               (begin (display (+ 1 2)) (sh-run (shell "hjk")))
  (sh-read-string* "#!/some/other/path\n\
    (display (* 3 4)); bnm"  'shell #t)                (begin (display (* 3 4)) (sh-run (shell "bnm")))
  (sh-read-file "test/test_file.ss")
       (begin (define (fib n)
         (let %fib ((i n))
           (if (fx>? i 2) (fx+ (%fib (fx1- i)) (%fib (fx- i 2))) 1)))
          (sh-run (shell "FOO" = "bar" \x3B;
                         )))
  (sh-read-file "test/test_file.sh")
       (begin
         (sh-run (shell
           "BAR" = "" \x3B;
           "foo" "a b" "c" \x7C;
           "bar" (shell-env "BAR") &&
           (shell "echo"
             (shell-backquote "baz" "--quiet")
               < "/dev/null" 2 >& "1" \x7C;\x7C;
             "fail" "--verbose")  \x3B;
             ))
         (set! a 42))

  ;; ------------------------- repl ---------------------------------------
  ;; {"(expand-omit-library-invocations #t)           ; avoid, requires Chez Scheme >= 10.0.0

  (first-value (repl-parse (string->parsectx
    "(+ 2 3) (values 7 (cons 'a 'b))"
    (parsers)) 'scheme))                               ((+ 2 3) (values 7 (cons 'a 'b)))
  (first-value (repl-parse (string->parsectx
    "ls -l | wc -b && echo ok || echo error &"
    (parsers)) 'shell))                                ((shell "ls" "-l" \x7C;
                                                               "wc" "-b" && "echo" "ok" \x7C;\x7C;
                                                               "echo" "error" &))
  (first-value (repl-parse (string->parsectx
    "(values '{})" (parsers))
    'scheme))                                          ((values '(shell)))
  (first-value (repl-parse (string->parsectx
     "{ls; #!scheme 1 2 3}"                            ; ugly result, and not very useful
     (parsers)) 'scheme))                              ((shell "ls" \x3B;
                                                               1 2 3))
  (first-value (repl-parse (string->parsectx
     "(values '{ls; #!scheme 1 2 3})"                  ; ugly result, and not very useful
     (parsers)) 'scheme))                              ((values '(shell "ls" \x3B;
                                                                        1 2 3)))
  (first-value (repl-parse (string->parsectx
    "(1 2 3)" (parsers)) 'scheme))                     ((1 2 3))
  (first-value (repl-parse(string->parsectx
    "#!scheme 1 2 3" (parsers)) 'shell))               (1 2 3)
  (first-value (repl-parse(string->parsectx
     "1 2 3" (parsers)) 'shell))                       ((shell "1" "2" "3"))
  (first-value (repl-parse(string->parsectx            ; must return the same as parsing "(1 2 3)"
     "{#!scheme 1 2 3}" (parsers)) 'scheme))           ((1 2 3))
  ;; ideally would return the same as previous test,
  ;; but deciding to omit the (shell ...) wrapper is tricky
  (first-value (repl-parse (string->parsectx
     "{#!scheme 1 2 3}" (parsers)) 'shell))            ((shell (1 2 3)))

)
