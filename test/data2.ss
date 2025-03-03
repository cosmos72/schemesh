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
  ;; ------------------------ parser scheme -------------------------------
  ;; #; comments the next s-expr
  '((foo bar) #; (a b (c)) '(d [ef g] h))              ((foo bar) '(d (ef g) h))
  '(a (b c . d) . e)                                   (a (b c . d) . e)
  '(#\m #\x7e)                                         (#\m #\~)
  ;; Chez Scheme allows additional character names, and also octal sequences #\000 ... #\377
  (map char->integer
    '(#\rubout #\bel #\vt #\nel #\ls #\000 #\001 #\376 #\377))
                                                       (127 7 11 133 8232 0 1 254 255)
  ;; character literals #\xdc80 ... #\xdcff are allowed only by UTF-8b
  (map char->integer
    '(#\x20ac #\xdc80 #\xdcff #\xffff #\x10ffff))      (8364 56448 56575 65535 1114111)
  ;; string escape sequences #\xdc80; ... #\xdcff; are allowed only by UTF-8b
  (let ((ret '()))
    (string-iterate
        "\" \x20ac; \xdc80; \xdcff; \""
      (lambda (i ch)
        (set! ret (cons (char->integer ch) ret))))
    (reverse! ret))                                    (34 32 8364 32 56448 32 56575 32 34)
  (list #| '\' . #| ,`@# |# |# ; nested block comments
       '#(a 1.0 2/3) '#2(d) #vu8(1 2 3)
        #4vu8(9) '#vfx(-1 0 2) '#3vfx(4))              (#(a 1.0 2/3) #(d d) #vu8(1 2 3) #vu8(9 9 9 9) #vfx(-1 0 2) #vfx(4 4 4))

  ;; ------------------------ parser shell -------------------------------
  ;; invariant: {#!scheme ...} is always equivalent to (...)
  '{#!scheme 1 2 (3 . 4)}                              (1 2 (3 . 4))
  (eq? (void)
       (parse-shell-form1 (string->parsectx "")))      #t
  '{}                                                  (shell)
  '{{}}                                                (shell (shell))
  '{ls -l>/dev/null&}                                  (shell "ls" "-l" > "/dev/null" &)
  '{{;foo} <log 2>&1 && bar<>baz|wc -l;;}
                                                       (shell (shell \x3B;
                                                                "foo") < "log" 2 >& "1" && "bar" <> "baz" \x7C;
                                                                       "wc" "-l" \x3B; \x3B;)
  '{echo|{cat;{true}
     }&}                                               (shell "echo" \x7C;
                                                          (shell "cat" \x3B;
                                                            (shell "true") \x3B;) &)
  (sh-job->string
    {echo|{cat;{true}}&})
                                                       "{echo | {cat ; true} &}"
  '{ls ;
    [foo || bar &] & echo}                             (shell "ls" \x3B;
                                                         (shell-subshell "foo" \x7C;\x7C;
                                                           "bar" &) & "echo")
  '{ls && [A=1 foo || bar &] || [B=2 echo]}            (shell "ls" &&
                                                         (shell-subshell "A" = "1" "foo" \x7C;\x7C;
                                                           "bar" &) \x7C;\x7C;
                                                         (shell-subshell "B" = "2" "echo"))
  '{ls[A-Z]?[!ax-z] .}                                 (shell (shell-wildcard "ls" % "A-Z" ? %! "ax-z") ".")
  '{{{{echo|cat}}}}                                    (shell (shell (shell (shell "echo" \x7C;
                                                                                   "cat"))))
  '{a<>/dev/null||b>/dev/zero&&!c>&2}                  (shell "a" <> "/dev/null" \x7C;\x7C;
                                                              "b" > "/dev/zero" && ! "c" >& "2")
  ;; test fd number [N] before redirection, and test backslash-newline
  '{foo 0</dev/zero 1<>/dev/urandom 2<&- \
        3>>logfile 4>otherfile 5>&/dev/null}           (shell "foo" 0 < "/dev/zero" 1 <> "/dev/urandom" 2 <& "-"
                                                                    3 >> "logfile"  4 > "otherfile" 5 >& "/dev/null")
  '{ls "-l" '.'}                                       (shell "ls" "-l" ".")
  '{ls "some"'file'path}                               (shell "ls" (shell-wildcard "some" "file" "path"))
  '{ls `cmd1 && cmd2 || cmd3 -arg3`}                   (shell "ls" (shell-backquote "cmd1" && "cmd2" \x7C;\x7C;
                                                                                    "cmd3" "-arg3"))
  '{ls $vv1 "$vv2" '$vv3'}                             (shell "ls" (shell-env "vv1") (shell-env "vv2") "$vv3")
  '{ls $vv1"$vv2"'$vv3'}                               (shell "ls" (shell-wildcard (shell-env "vv1") (shell-env "vv2") "$vv3"))
  '{ls $v-1 "$v-2" '$v-3'}                             (shell "ls" (shell-wildcard (shell-env "v") "-1")
                                                                   (shell-wildcard (shell-env "v") "-2")
                                                                   "$v-3")
  '{ls $v-1"$v-2"'$v-3'}                               (shell "ls" (shell-wildcard (shell-env "v") "-1" (shell-env "v") "-2" "$v-3"))
  '{ls ${v 1} "${ v 2 }" '${ v 3 }'}                   (shell "ls" (shell-env "v 1") (shell-env " v 2 ") "${ v 3 }")
  '{ls ${v 1}"${ v 2 }"'${ v 3 }'}                     (shell "ls" (shell-wildcard (shell-env "v 1") (shell-env " v 2 ") "${ v 3 }"))

  '{ls $[cmd arg $var]}                                (shell "ls" (shell-backquote "cmd" "arg" (shell-env "var")))
  '{ls "$[cmd arg $var]"}                              (shell "ls" (shell-backquote "cmd" "arg" (shell-env "var")))
  '{ls '$[cmd arg $var]'}                              (shell "ls" "$[cmd arg $var]")
  ;; test () inside shell syntax
  '{echo a && (cons 1 2)}                              (shell "echo" "a" && (cons 1 2))

  ;; ------------------------ parse-forms ---------------------------------
  (parse-forms1 (string->parsectx
    "" (parsers)) 'scheme)                             ()
  (parse-forms1 (string->parsectx
    "+" (parsers)) 'scheme)                            (+)
  ;; #!eof is equivalent to end-of-file in the input port
  (parse-forms1 (string->parsectx
    "'(a . b) c #!eof . ) syntax error"
     (parsers)) 'scheme)                               ('(a . b) c)
  ;; test backslash-intraline-whitespace-newline-intraline-whitespace inside Scheme string
  (parse-forms1 (string->parsectx
    "\"foo\\ \n bar\" \"baz\\\nxcv\""
     (parsers)) 'scheme)                               ("foobar" "bazxcv")

  (parse-forms1 (string->parsectx
    "uiop asdf #!scheme xyz %%a"
     (parsers)) 'scheme)                               (uiop asdf xyz %%a)
  (parse-forms1 (string->parsectx
    "uiop asdf #!scheme (xyz %%a)"
    (parsers)) 'scheme)                                (uiop asdf (xyz %%a))
  ;; #! not followed by [0-9A-Za-z] skips the rest of line
  (parse-forms1 (string->parsectx
    "qwerty #!/some/path . bad | #syntax) \n asdf"
    (parsers)) 'scheme)                                (qwerty asdf)
  ;; ; skips the rest of line too
  (parse-forms1 (string->parsectx
    "uiop ; this is a comment\nnot a comment"
    (parsers)) 'scheme)                                (uiop not a comment)
  (parse-forms1 (string->parsectx
    "`('foo ,bar ,@baz) #`(#'sfoo #,sbar #,@sbaz)"
    (parsers)) 'scheme)                                (`('foo ,bar ,@baz) #`(#'sfoo #,sbar #,@sbaz))
  (parse-forms1 (string->parsectx
    "7 {echo hello >& /dev/null}"
    (parsers)) 'scheme)                                (7 (shell "echo" "hello" >& "/dev/null"))
  (parse-forms1 (string->parsectx
    "(values {echo hello >> log.txt})"
    (parsers)) 'scheme)                                ((values (shell "echo" "hello" >> "log.txt")))
  '(values {echo foo >> log.txt})                      (values (shell "echo" "foo" >> "log.txt"))

  (parse-forms1 (string->parsectx
    "" (parsers)) 'shell)                              ()
  (parse-forms1 (string->parsectx
    "{}" (parsers)) 'shell)                            ((shell))
  (parse-forms1 (string->parsectx
    "{{}}" (parsers)) 'shell)                          ((shell (shell)))
  '{}                                                  (shell)
  '{{}}                                                (shell (shell))
  (parse-forms1 (string->parsectx
    "foo && bar || baz &"
    (parsers)) 'shell)                                 ((shell "foo" && "bar" \x7C;\x7C;
                                                               "baz" &))
  (parse-forms1 (string->parsectx
    "true -foo >& log.txt"
    (parsers)) 'shell)                                 ((shell "true" "-foo" >& "log.txt"))
  ;; character { switches to shell parser
  ;; directive #!shell switches to shell parser
  {echo bar >> log.txt}                                ,@"(sh-cmd* \"echo\" \"bar\" 1 '>> \"log.txt\")"
  '(9 #!shell echo baz <> log.txt)                     (9 (shell "echo" "baz" <> "log.txt"))
  '(#!shell echo charlie >> log.txt)                   (shell "echo" "charlie" >> "log.txt")
  '(values foo bar
     #!shell baz >> log.txt ;
             wc -l log.txt)                            (values foo bar (shell "baz" >> "log.txt" \x3B;
                                                                              "wc" "-l" "log.txt"))
  (parse-forms1 (string->parsectx
    "ls ; #!shell echo" (parsers)) 'shell)             ((shell "ls" \x3B;
                                                               "echo"))
  ;; test multiple #!... directives
  '((+ a b) #!shell uiop -opt >> log.txt ;
            #!scheme foo bar)                          ((+ a b) (shell "uiop" "-opt" >> "log.txt" \x3B;
                                                                       foo bar))

  ;; character ( inside shell syntax switches to Scheme parser for a single Scheme form,
  ;; then continues parsing shell syntax.
  ;; If the character ( is the first token, and the parsed Scheme form is followed by one of:
  ;;   newline, semicolon, another #\( or eof,
  ;; then the Scheme form is compiled as-is, without wrapping it inside (shell ...)
  (parse-forms1 (string->parsectx
    "(+ 1 2)" (parsers)) 'shell)                       ((+ 1 2))
  (parse-forms1 (string->parsectx
    "(+ 2 3) ; echo" (parsers)) 'shell)                ((+ 2 3) (shell "echo"))
  (parse-forms1 (string->parsectx
    "(+ 4 5) (* 6 7) ; pwd" (parsers)) 'shell)          ((+ 4 5) (* 6 7) (shell "pwd"))

  ;; in shell syntax, an initial Scheme form ( ... ) gets wrapped
  ;; inside the overall (shell ...) together with subsequent forms
  ;; UNLESS it's followed by one of:
  ;;   newline, semicolon, another #\( or eof
  (parse-forms1 (string->parsectx
    "(sh-cmd \"true\") && echo" (parsers)) 'shell)     ((shell (sh-cmd "true") && "echo"))

  ;; Scheme forms can be inserted inside shell syntax
  (parse-forms1 (string->parsectx
    "echo (+ 6 7)" (parsers)) 'shell)                  ((shell "echo" (+ 6 7)))
  '{echo (+ 8 9)}                                      (shell "echo" (+ 8 9))
  '{qwert (apply + a `(,@b)) &}                        (shell "qwert" (apply + a `(,@b)) &)
  '{ls (my-dir) >> log.txt}                            (shell "ls" (my-dir) >> "log.txt")
  '{foo; bar}                                          (shell "foo" \x3B;
                                                              "bar")
  ;; open bracket [ at the beginning of a command starts a subshell, not a wildcard
  (parse-forms1 (string->parsectx
    "[foo; bar]" (parsers)) 'shell)                    ((shell-subshell "foo" \x3B;
                                                                        "bar"))
  '{[foo; bar]}                                        (shell (shell-subshell "foo" \x3B;
                                                                              "bar"))
  '{[foo] [bar]}                                       (shell (shell-subshell "foo") (shell-subshell "bar"))

  ;; open bracket [ not at the beginning of a command starts a wildcard, not a subshell
  '{""[foo] [bar]}                                     (shell (shell-wildcard % "foo") (shell-wildcard % "bar"))
  '{jkl ~ ;
      #!scheme (f a b)}                                (shell "jkl" (shell-wildcard ~) \x3B;
                                                         (f a b))


  ;; ------------------------ parse-paren --------------------------------
  (paren->list (string->paren "{"))                    (scheme #t 0 0 #t 1 0)
  (paren->list (string->paren "{[("))                  (scheme #t 0 0 #t 3 0)
  (paren->list (paren-inner-ref
    (string->paren "{") 0))                            (shell #\{ 0 0 #f 1 0)
  (paren->list (string->paren "{\n"))                  (scheme #t 0 0 #t 0 1)
  (paren->list (paren-inner-ref
    (string->paren "{\n") 0))                          (shell #\{ 0 0 #f 0 1)
  (paren->list (paren-inner-ref*
    (string->paren "{[(") 0 0 0))                      (scheme #\( 2 0 #f 3 0)
  (string->paren
    "(foo \"a()\" \"b[]\" \"c{}\" [* |2| 3])")         ,@"#<paren _(\"\" \"\" \"\" [||])_>"
  (string->paren
    "#\\newline #\\( #\\) #\\[ #\\] #\\{ #\\} #\\#")   ,@"#<paren __>"
  (string->paren "#| comment . , \\ |#")               ,@"#<paren _##_>"
  ;; [] are grouping tokens in shell syntax, and `` are not special in lisp syntax
  (string->paren "{[(``)]}")                           ,@"#<paren _{[()]}_>"

  ;; [] are grouping tokens in lisp syntax and `` are grouping tokens in shell syntax
  (string->paren "([{``}])")                           ,@"#<paren _([{``}])_>"
  ;; test $( $[ ${ shell syntax )
  (string->paren "{$(`{}()`)}")                        ,@"#<paren _{(`{} ()`)}_>"
  (string->paren "{$[$({${}})]}")                      ,@"#<paren _{[({{}})]}_>"
  ;; test single-quoted strings in shell syntax
  (string->paren "{'foo\"bar{}[]()``baz'}")            ,@"#<paren _{''}_>"
  ;; test double-quoted strings in shell syntax
  (string->paren "{\"foobar{}[]``${baz}\"}")           ,@"#<paren _{\"`` {}\"}_>"
  ;; paren are not special in shell syntax inside double quoted string
  (string->paren "{\"()\"}")                           ,@"#<paren _{\"\"}_>"
  ;; parse mismatched paren
  (string->paren "'" 'shell)                           ,@"#<paren _'\x1B;[30;1m'\x1B;[m_>"
  (string->paren "([{)]}")                             ,@"#<paren _([{\x1B;[30;1m(\x1B;[m) \x1B;[30;1m[\x1B;[m]}\x1B;[30;1m]\x1B;[m\x1B;[30;1m)\x1B;[m_>"
  (string->paren "(\" a\"")                            ,@"#<paren _(\"\"\x1B;[30;1m)\x1B;[m_>"
  ;; the code after #!scheme is inside a nested paren with name = 'scheme
  (string->paren "ls #!scheme 1 2 3" 'shell)           ,@"#<paren ____>"
  (string->paren "{ls ; #!scheme 1 2 3}")              ,@"#<paren _{{}}_>"
  (string->paren "(values '{ls; #!scheme 1 2 3})")     ,@"#<paren _({{}})_>"

  (let ((p (string->paren "{[a] && b]")))
    (list
      (paren-find/surrounds p 0 0)
      (paren-find/surrounds p 1 0)
      (paren-find/surrounds p 2 0)
      (paren-find/surrounds p 3 0)
      (paren-find/surrounds p 4 0)
      (paren-find/surrounds p 5 0)
      (paren-find/surrounds p 6 0)
      (paren-find/surrounds p 7 0)
      (paren-find/surrounds p 8 0)
      (paren-find/surrounds p 9 0)
      (paren-find/surrounds p 10 0)))                 ,@"(#<paren _{[] \x1B;[30;1m[\x1B;[m]\x1B;[30;1m}\x1B;[m_> \
    #<paren {[] \x1B;[30;1m[\x1B;[m]\x1B;[30;1m}\x1B;[m> #<paren []> #<paren []> \
    #<paren {[] \x1B;[30;1m[\x1B;[m]\x1B;[30;1m}\x1B;[m> \
    #<paren {[] \x1B;[30;1m[\x1B;[m]\x1B;[30;1m}\x1B;[m> #<paren {[] \x1B;[30;1m[\x1B;[m]\x1B;[30;1m}\x1B;[m> \
    #<paren {[] \x1B;[30;1m[\x1B;[m]\x1B;[30;1m}\x1B;[m> #<paren {[] \x1B;[30;1m[\x1B;[m]\x1B;[30;1m}\x1B;[m> \
    #<paren {[] \x1B;[30;1m[\x1B;[m]\x1B;[30;1m}\x1B;[m> #<paren {[] \x1B;[30;1m[\x1B;[m]\x1B;[30;1m}\x1B;[m>)"


  ;; -------------------------- parenmatcher -------------------------------
  (paren->list (parenmatcher-find/at
    (make-parenmatcher)
    (string->parsectx "([{``}] #| |# )" (parsers))
    'scheme
    6 0))                                              (scheme #\[ 1 0 #\] 6 0)
  (paren->list (parenmatcher-find/surrounds
    (make-parenmatcher)
    (string->parsectx "([{``)))" (parsers))
    'scheme
    6 0))                                              (shell #\{ 2 0 #f 8 0)

  ;; ------------------------- shell paths --------------------------------
  (sh-path-absolute? (string->charspan* "/foo"))       #t
  (sh-path-absolute? (string->charspan* "bar/"))       #f
  (sh-path "//foo///bar////")                          ,(string->charspan* "//foo///bar////")
  (sh-subpath "//foo///bar////")                       ,(string->charspan* "/foo/bar")
  (sh-subpath "/foo/bar/" "/aaa/" "../baz/bbbb/")      ,(string->charspan* "/foo/bar/baz/bbbb")
  (sh-path? (string->charspan* "../a//b/"))            #t
  (sh-path? (string->charspan* "\x0;"))                #f
  (sh-subpath? (string->charspan* "../a//b/"))         #f
  (sh-subpath? (string->charspan* "a//b"))             #f
  (sh-subpath? (string->charspan* "a/b/"))             #t

  ;; ------------------------- shell aliases ------------------------------
  (begin
    (sh-alias-set! "test-alias-foo" '("bar" "baz"))
    (sh-aliases-expand '("test-alias-foo" "123" "456")))
                                                      ("bar" "baz" "123" "456")

  ;; ------------------------- shell job ---------------------------------
  (begin
    (sh-env-set! #t "foo" "bar")
    (cons
      (sh-env-ref   #t "foo")
      (values->list (sh-env-visibility-ref #t "foo"))))
                                                      ("bar" "bar" private)
  (let ((j (sh-subshell (sh-cmd "sleep" "1") '\x3B;
                        (sh-cmd "echo" "done"))))
    (let-values (((port get-string) (open-string-output-port)))
      (sh-job-display j port)
      (newline          port)
      (sh-job-write   j port)
      (get-string)))                ,@"[sleep 1 ; echo done]\n\
                                       (sh-subshell (sh-cmd \"sleep\" \"1\") '\\x3B; \
                                                    (sh-cmd \"echo\" \"done\"))"

  (let ((j {{[sleep 1] || ls} && cd ..}))
    (let-values (((port get-string) (open-string-output-port)))
      (sh-job-display j port)
      (newline          port)
      (sh-job-write   j port)
      (get-string)))               ,@"{{[sleep 1] || ls} && cd ..}\n\
                                      (sh-and (sh-or (sh-subshell (sh-cmd \"sleep\" \"1\")) \
                                                     (sh-cmd \"ls\")) \
                                              (sh-cmd \"cd\" \"..\"))"
  (sh-cmd "echo"  "foo" " bar ")                       ,(sh-cmd "echo" "foo" " bar ")
  (sh-cmd* "ls" (lambda (j) "."))                      ,@"(sh-cmd* \"ls\" #<procedure>)"
  (sh-cmd* "A" '= "B" "echo")                          ,@"(sh-cmd* \"A\" '= \"B\" \"echo\")"
  (sh-find-job 0)                                      #f
  (sh-find-job 1)                                      #f
  (sh-find-job #t)                                     ,@(sh-globals)


  ;; ------------------------- shell syntax -------------------------------
  (sh-parse-datum
    '(shell "wc" "-l" "myfile" > "mylog" \x3B;
            "echo" "done"))                            (sh-list (sh-cmd* "wc" "-l" "myfile" 1 '> "mylog") '\x3B;
                                                                (sh-cmd "echo" "done"))
  (sh-parse-datum '(shell "find" "-type" "f" \x7C;&
                          "wc" &))                     (sh-list (sh-pipe* (sh-cmd "find" "-type" "f") '\x7C;&
                                                                          (sh-cmd "wc")) '&)
  ;; (sh-parse) does not alter nested (shell "foo") and returns it verbatim
  (sh-parse-datum '(shell (shell "foo") \x3B;
                          "bar"))                      (sh-list (shell "foo") '\x3B; (sh-cmd "bar"))
  (sh-parse-datum '(shell ! "foo" && "bar"))           (sh-and (sh-not (sh-cmd "foo")) (sh-cmd "bar"))
  ;; double negation is optimized away
  (sh-parse-datum '(shell ! ! "true"))                 (sh-cmd "true")
  (sh-parse-datum '(shell ! ! ! "false"))              (sh-not (sh-cmd "false"))
  (sh-parse-datum '(shell-subshell "abc" && "def"))    (sh-subshell (sh-and (sh-cmd "abc") (sh-cmd "def")))

  ;; ------------------------- shell macros -------------------------------
  (caddr (expand '{}))                                 (sh-cmd)
  (caddr (expand '(shell)))                            (sh-cmd)
  (caddr (expand '{2>& 1}))                            (sh-cmd* 2 '>& 1)
  (caddr (expand '{echo 2>& 1}))                       (sh-cmd* "echo" 2 '>& 1)
  (caddr (expand '(shell 2 >& 1)))                     (sh-cmd* 2 '>& 1)
  (caddr (expand '{ls -l && wc -b || echo error &}))   (sh-list (sh-or (sh-and (sh-cmd "ls" "-l") (sh-cmd "wc" "-b"))
                                                                       (sh-cmd "echo" "error")) '&)
  (caddr (expand '{(sh-or (sh-and {ls -l} {wc -b})
                          {echo error}) &}))           (sh-list (sh-or (sh-and (sh-cmd "ls" "-l") (sh-cmd "wc" "-b"))
                                                                       (sh-cmd "echo" "error")) '&)
  (caddr (expand '(shell "ls" "-l" && "wc" "-b" \x7C;\x7C;
                         "echo" "error" &)))           (sh-list (sh-or (sh-and (sh-cmd "ls" "-l") (sh-cmd "wc" "-b"))
                                                                       (sh-cmd "echo" "error")) '&)
  (caddr (expand '{true || ! false}))                  (sh-or (sh-cmd "true") (sh-not (sh-cmd "false")))
  (caddr (expand '{true || ! false}))                  (sh-or (sh-cmd "true") (sh-not (sh-cmd "false")))
  (caddr (expand '(shell "true" \x7C;\x7C;
                         ! "false")))                  (sh-or (sh-cmd "true") (sh-not (sh-cmd "false")))
  (caddr (expand '{{ls -al >> log.txt}}))              (sh-cmd* "ls" "-al" 1 '>> "log.txt")
  (caddr (expand '(shell-list
    (shell "ls" "-al" >> "log.txt"))))                 (sh-cmd* "ls" "-al" 1 '>> "log.txt")
  (caddr (expand '(shell-expr (if a b c))))            (sh-cmd* "builtin" "value" (lambda () (if a b c)))
  (caddr (expand '{{{{echo|cat}}}}))                   (sh-pipe* (sh-cmd "echo") '\x7C; (sh-cmd "cat"))
  (caddr (expand '(sh-pipe* {echo} '\x7C; {cat})))     (sh-pipe* (sh-cmd "echo") '\x7C; (sh-cmd "cat"))
  (caddr (expand (parse-shell-form1 (string->parsectx
    "{{{{echo|cat}}}}"))))                             (sh-pipe* (sh-cmd "echo") '\x7C; (sh-cmd "cat"))
  (caddr (expand '{echo|{cat;{true}}}))                (sh-pipe* (sh-cmd "echo") '\x7C; (sh-list (sh-cmd "cat") '\x3B;
                                                                                                 (sh-cmd "true")))
  (caddr (expand (parse-shell-form1 (string->parsectx
    "{echo|{cat;{true}}}"))))                          (sh-pipe* (sh-cmd "echo") '\x7C; (sh-list (sh-cmd "cat") '\x3B;
                                                                                                 (sh-cmd "true")))
  (caddr (expand '{{ls & echo}}))                      (sh-list (sh-cmd "ls") '& (sh-cmd "echo"))
  (caddr (expand '{(shell {ls} & {echo})}))            (sh-list (sh-cmd "ls") '& (sh-cmd "echo"))
  (caddr (expand '(shell (shell "ls" & "echo"))))      (sh-list (sh-cmd "ls") '& (sh-cmd "echo"))
  (caddr (expand '{{foo}
                     bar}))                            (sh-list (sh-cmd "foo") '\x3B; (sh-cmd "bar"))
  (caddr (expand '(shell (shell "foo") \x3B;
                         "bar")))                      (sh-list (sh-cmd "foo") '\x3B; (sh-cmd "bar"))
  (caddr (expand '(shell {ls & echo} 2 >& 1)))         (sh-redirect! (sh-list (sh-cmd "ls") '& (sh-cmd "echo")) 2 '>& 1)
  (caddr (expand '(shell (shell "ls" &
                                "echo") 2 >& 1)))      (sh-redirect! (sh-list (sh-cmd "ls") '& (sh-cmd "echo")) 2 '>& 1)
  (caddr (expand
    '{echo|{cat;{true}}&}))                            (sh-list (sh-pipe* (sh-cmd "echo") '\x7C;
                                                                          (sh-list (sh-cmd "cat") '\x3B;
                                                                                   (sh-cmd "true"))) '&)
  (caddr (expand '(shell \x3B;
     (shell "foo") \x3B;
     "bar")))                                          (sh-list '\x3B; (sh-cmd "foo") '\x3B; (sh-cmd "bar"))
  {
     (shell "foo")
     "bar"}                                            ,@"(sh-list (sh-cmd \"foo\") '\\x3B; (sh-cmd \"bar\"))"
  (caddr (expand
    '(shell (shell "ls" & "echo") 2 >& 1)))            (sh-redirect! (sh-list (sh-cmd "ls") '& (sh-cmd "echo")) 2 '>& 1)
  (parse-shell-form1 (string->parsectx "{{foo};bar}")) (shell (shell "foo") \x3B;
                                                              "bar")
  '{A=B ls}                                            (shell "A" = "B" "ls")
  (parse-shell-form1 (string->parsectx "A=B ls"))      (shell "A" = "B" "ls")
  (parse-shell-form1 (string->parsectx "{C=D echo}"))  (shell "C" = "D" "echo")
  (caddr (expand '{A=B ls}))                           (sh-cmd* "A" '= "B" "ls")
  (caddr (expand '(shell "A" = "B" "ls")))             (sh-cmd* "A" '= "B" "ls")
  (caddr (expand (parse-shell-form1 (string->parsectx
    "{A=B ls}"))))                                     (sh-cmd* "A" '= "B" "ls")
  '{FOO=$BAR/subdir echo}                              (shell "FOO" = (shell-wildcard (shell-env "BAR") "/subdir") "echo")
  (caddr (expand '{FOO=$BAR/subdir echo}))             ,@(sh-cmd* FOO '= (lambda (job) (sh-wildcard job
                                                           (lambda (job) (sh-env-ref job BAR)) /subdir)) echo)
  '{A=$[echo abc
        echo def]}                                     (shell "A" = (shell-backquote "echo" "abc" \x3B;
                                                                                     "echo" "def"))
  '{A=`echo abc
       echo def`}                                      (shell "A" = (shell-backquote "echo" "abc" \x3B;
                                                                                     "echo" "def"))
  (caddr (expand '(shell "A" =
    (shell-backquote "echo" "abc" \x3B;
                     "echo" "def"))))                  ,(sh-cmd* "A" '= (lambda () (sh-run/string-rtrim-newlines
                                                           (sh-list (sh-cmd "echo" "abc") '\x3B; (sh-cmd "echo" "def")))))
  (caddr (expand '{FOO=$BAR/subdir echo}))
                                                       ,@(sh-cmd* "FOO" '= (lambda (job) (sh-wildcard job (lambda (job)
                                                           (sh-env-ref job "BAR")) "/subdir")) "echo")
  (caddr (expand
    '(shell (shell-wildcard "l" "s"))))                (sh-cmd* "ls")
  (caddr (expand
    '(shell (shell-wildcard "l" "s") ".")))            (sh-cmd* "ls" ".")
  (caddr (expand
    '(shell (shell-backquote "echo" "ls"))))           ,(sh-cmd* (lambda () (sh-run/string-rtrim-newlines (sh-cmd "echo" "ls"))))
  ;; test wildcards and patterns [...]
  '{echo *}                                            (shell "echo" (shell-wildcard *))
  (caddr (expand '{echo *}))                           ,@(sh-cmd* "echo" (lambda (job) (sh-wildcard job '*)))
  '{echo .*[a-z]?.so}                                  (shell "echo" (shell-wildcard "." * % "a-z" ? ".so"))
  (caddr (expand '{echo .*[a-z]?.so}))                 ,@(sh-cmd* "echo" (lambda (job) (sh-wildcard job "." '* '% "a-z" '? ".so")))
  '{A=* B=* echo}                                      (shell "A" = "*" "B" = "*" "echo")
  ;; TODO: keep as is or prevent tilde expansion in B=~ ?
  ;; currently it's not usable because (shell-wildcard ~) expands to a list ("/home/user")
  '{A=* B=~ ls ~bar}                                   (shell "A" = "*" "B" = (shell-wildcard ~) "ls" (shell-wildcard ~ "bar"))
  '{echo ab'c'"d*?"*?[a-z]}                            (shell "echo" (shell-wildcard "ab" "c" "d*?" (shell-wildcard * ? % "a-z")))
  ;; in shell syntax, = is an operator only before command name
  '{echo A=B}                                          (shell "echo" "A=B")
  (caddr (expand '{echo A=B}))                         (sh-cmd "echo" "A=B")
  '{echo [ab]* ? [!z]}                                 (shell "echo" (shell-wildcard % "ab" *) (shell-wildcard ?) (shell-wildcard %! "z"))
  '{echo $[foo&&bar]}                                  (shell "echo" (shell-backquote "foo" && "bar"))
  (caddr (expand '{echo $[foo&&bar]}))                 ,(sh-cmd* "echo" (lambda () (sh-run/string-rtrim-newlines
                                                           (sh-and (sh-cmd "foo") (sh-cmd "bar")))))
  (caddr (expand '{{ls} > log.txt &}))                 ,@(sh-list* (sh-cmd "ls") 1 '> "log.txt" '&)
  (caddr (expand
    '{echo abc > DEL_ME && cat DEL_ME && rm DEL_ME}))  ,(sh-and (sh-cmd* "echo" "abc" 1 '> "DEL_ME")
                                                                (sh-cmd "cat" "DEL_ME")
                                                                (sh-cmd "rm"  "DEL_ME"))
  (caddr (expand
    '(shell "echo" "abc" > "DEL_ME" &&
            "cat" "DEL_ME" && "rm" "DEL_ME")))         ,(sh-and (sh-cmd* "echo" "abc" 1 '> "DEL_ME")
                                                                (sh-cmd "cat" "DEL_ME")
                                                                (sh-cmd "rm"  "DEL_ME"))
  {echo abc > DEL_ME && cat DEL_ME && rm DEL_ME}       ,@"(sh-and (sh-cmd* \"echo\" \"abc\" 1 '> \"DEL_ME\") \
                                                                (sh-cmd \"cat\" \"DEL_ME\") \
                                                                (sh-cmd \"rm\" \"DEL_ME\"))"
) #!eof
