;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;; this file contains tests and should be loaded with (sh-read-file)
;;
;; odd elements are Scheme form to evaluate, even elements are expected result
#(

  ;; -------------------------- tty ---------------------------------------
  ;; (tty-size) returs a cons (width . height), or c_errno() < 0 on error
  (let ((sz (tty-size)))
    (if (pair? sz)
      (and (integer? (car sz)) (positive? (car sz))
           (integer? (cdr sz)) (positive? (cdr sz)))
      (and (integer? sz) (negative? sz))))             #t

  ;; ------------------------- posix --------------------------------------
  (fx<=? (c-errno) 0)                                  #t
  (file-type "." 'catch)                               dir
  (file-type "parser/parser.ss" 'catch)                file
  (directory-sort!
    (directory-list "parser" 'types))        (("." . dir) (".." . dir) ("lisp-read-token.ss" . file)
                                              ("lisp.ss" . file) ("parser.ss" . file) ("r6rs.ss" . file)
                                              ("scheme.ss" . file) ("shell-read-token.ss" . file) ("shell.ss" . file))


  ;; ------------------------ lineedit io ---------------------------------
  (get-string-all
    (open-charline-input-port
      (string->charline* "58gu405gu*(&)\n")))          "58gu405gu*(&)\n"
  (get-string-all
    (open-charlines-input-port
      (charlines
        (string->charline* "085ug&^%}\n"))))           "085ug&^%}\n"
  (read
    (open-charline-input-port
      (string->charline*
        "(re8u (+ -) [* /] 'foo bar . baz)")))         (re8u (+ -) (* /) 'foo bar . baz)
  (read
    (open-charlines-input-port
      (charlines
        (string->charline* "(urehg* (a . 'b) 12")
        (charline)
        (string->charline* "3.45e3 . #\\m\n)"))))      (urehg* (a quote b) 123450.0 . #\m)

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
    '* '% "ch") "shell.c") (catch (ex) ex))            #t

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
  (sh-wildcard #t "a" "bcd" "" "ef")                   "abcdef"
  (sh-wildcard->sh-patterns '(*))                      ,@(span (sh-pattern '*))
  (sh-wildcard->sh-patterns '("/" * ".so"))            ,@(span "/" (sh-pattern '* ".so"))
  (sh-wildcard->sh-patterns '("//abc//" "//def//"))    ,@(span "/" "abc/" "def/")
  (sh-wildcard->sh-patterns '("/foo/" * "/" "/bar"))   ,@(span "/" "foo/" (sh-pattern '* "/") "bar")
  (sh-wildcard #t '* "/" '* ".c")                      ("containers/containers.c" "posix/posix.c" "shell/shell.c")
  (sh-wildcard #t "Makefile")                          ("Makefile") ; file exists => returned as list
  (sh-wildcard #t "_does_not_exist_")                  "_does_not_exist_" ; file does not exists => returned as string
  (caddr (expand '{ls [ab]*}))                         ,@(sh-cmd* "ls" (lambda (job) (sh-wildcard job '% "ab" '*)))
  (caddr (expand '(shell-wildcard *)))                 ,@(lambda (job) (sh-wildcard job '*))
  (caddr (expand '(shell-wildcard ?)))                 ,@(lambda (job) (sh-wildcard job '?))
  (caddr (expand '(shell-wildcard ~)))                 ,@(lambda (job) (sh-wildcard job '~))
  (caddr (expand '(shell-wildcard
   "a" (shell-wildcard ~ "b/" *) ? % "def" %! "ghi"))) ,@(lambda (job) (sh-wildcard job "a" '~ "b/" '* '? '% "def" '%! "ghi"))

  ;; ------------------------- builtin execution ------------------------------
  (sh-run (shell "true"))                              ,@"#<void>"
  (sh-run (shell "false"))                             (failed 1)
  (sh-run (shell "echo0"))                             ,@"#<void>"
  (sh-run (shell "value" "210"))                       (failed 210)
  (sh-run (shell-subshell "true"))                     ,@"#<void>"
  (sh-run (shell-subshell "false"))                    (failed 1)
  (sh-run (shell-subshell "echo0"))                    ,@"#<void>"
  (sh-run (shell-subshell "value" "210"))              (failed 210)
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
    (shell "command" "true" &&
           "grep" "abc" > "/dev/null")))               (failed 1)
  (sh-run/string (shell
    "echo0" "def" "gh" "i" ""))                        "def\x0;gh\x0;i\x0;\x0;"
  (sh-run/string (shell
    "split-at-0" "echo"
      (shell-backquote "echo0" "jkl" "mn" "o" "")))    "jkl mn o \n"
  (sh-run {
     $(display "hello") | cat |
     $(utf8b->string (fd-read-all (sh-fd 0)))})  ,(ok "hello")

  ;; run builtin in a subprocess
  (sh-run (sh-cmd "false") '(spawn? . #t))             (failed 1)
  (let ((j (sh-cmd "false")))
    (sh-start j '(spawn? . #t))
    (sh-wait j))                                       (failed 1)
  ;; run a pipe in current shell
  (sh-run (shell
    "command" "true" \x7C;
    "false"))                                          (failed 1)
  (sh-run (shell
    "false" \x7C;
    "command" "true" \x7C;
    "value" "17"))                                      (failed 17)
  ;; run a pipe in a subshell
  (sh-run (shell-subshell
    "builtin" "true" \x7C;
    "builtin" "command" "false" \x7C;
    "global"  "value" "19"))                            (failed 19)


  ;; ------------------------- job execution ---------------------------------

  (sh-run/i (shell (shell-expr -1 (* 7 8 9))))         ,(ok 504)
  (sh-run/i {$(vector 1 2 3)})                         ,(ok #(1 2 3))
  (sh-run/i {$(values)})                               ,(ok)
  (sh-run/i {$(values (void))})                        ,@"#<void>"
  (sh-run/i {$(values #t)})                            ,(ok #t)
  (sh-run/i {$(values #f)})                            ,(failed #f)
  (sh-run/i {$(values 4 5 6)})                         ,(ok 4 5 6)

  (sh-run/i (sh-cmd "command" "true"))                 ,@"#<void>"
  (sh-run   (sh-cmd "command" "false"))                (failed 1)
  (sh-run   (sh-cmd "value" "0"))                      ,@"#<void>"
  (sh-run   (sh-cmd "value" "257"))                    (failed 257)
  (sh-run/i {false
             true})                                    ,@"#<void>"
  (sh-run   {true
             false})                                   (failed 1)
  (sh-run/i {true && false})                           (failed 1)
  (sh-run   {true && false})                           (failed 1)
  (sh-run/i {true || false})                           ,@"#<void>"
  (sh-run   {true || false})                           ,@"#<void>"
  (sh-run   {false || false})                          (failed 1)
  (sh-run/i {! true})                                  (failed 1)
  (sh-run   {! true})                                  (failed 1)
  (sh-run/i {! false})                                 ,@"#<void>"
  (sh-run   {! false})                                 ,@"#<void>"
  (let ((j {true && command false}))
    (sh-start j)
    (sh-bg j)
    (sh-fg j))                                         (failed 1)
  (let ((j {true |& command false}))
    (sh-start j)
    (sh-bg j)
    (sh-wait j))                                       (failed 1)
  ;; (sh-start) of a builtin, or a multijob containing (recursively) only builtins,
  ;; directly returns their exit status, as (sh-run) would do.
  ;; Reason: there is no spawned external process to wait for.
  (sh-start {true && false})                           (failed 1)
  (let ((j {sleep 1}))
    (sh-start j)
    (sh-bg j))                                         (running 1)
  (sh-run {[true
            false]})                                   (failed 1)
  ;; if stopped, schemesh forgets to restore its process group as fg => lineedit read() fails => fatal error
  (sh-run $(sh-run
    { {echo a
       sleep 0
       echo b
       sleep 0
       echo c} |
     $(get-string-all (current-input-port))}))         (ok (ok "a\nb\nc\n"))

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
