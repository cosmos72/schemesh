# Mixing shell and Scheme syntax

The main [README.md](../../README.md) describes the four mechanism for switching between shell and Lisp (precisely, Scheme) syntax:

* open parenthesis `(` temporarily switches to Scheme syntax until the corresponding `)`.

* open brace i.e. `{` temporarily switches to shell syntax until the corresponding `}`.

* open bracket i.e. `[` starts a new sub-form in current syntax until the corresponding `]`.<br/>
  If found in Scheme syntax, it is equivalent to `(`.<br/>
  If found in shell syntax, it is similar to `{` with the difference that commands will be executed in a **subshell**.

* the directives `#!scheme` `#!chezscheme` and `#!r6rs` temporarily switch to Scheme syntax
  (with the appropriate flavor) until the end of current `( )`, `[ ]` or `{ }`.<br/>
  If entered at top level, they change the default syntax until another directive is entered at top level.

* the directive `#!shell` temporarily switches to shell syntax until the end of current `( )`, `[ ]` or `{ }`.<br/>
  If entered at top level, it changes the default syntax until another directive is entered at top level.

Syntax switching can be nested arbitrarily deep, i.e. you can write
```
{shell syntax (Scheme syntax {shell syntax (Scheme syntax {...} ...) ...} ...) ...}
```
with as many nesting levels as you want.

### REPL

At REPL start, the default syntax is shell.
If you want to change it, add `(repl-initial-parser 'scheme)` to your initialization file `~/.config/schemesh/repl_init.ss`
and restart schemesh.

As a special case for REPL, Scheme syntax is accepted and works as expected even if the current syntax is shell:
```
#!shell
(display "hello, ") (display "world!\n")
```
outputs
```
hello, world!
```
This special case is added for convenience, and is only guaranteed to work inside REPL - not in files.

### Files

Schemesh can execute files in several ways:
1. by running `schemesh FILE`
2. by running `schemesh --cmd-file FILE`
3. by running `schemesh --eval-file FILE`
4. by creating a file that starts with a line `#!/usr/bin/env schemesh` then marking it executable with `chmod +x FILE`
   and finally executing it with `./FILE`
5. inside schemesh using shell syntax, by executing `. FILE` or `source FILE`  
6. inside schemesh using Scheme syntax, by evaluating `(sh-eval-file "FILE")`
7. inside schemesh using Scheme syntax, by evaluating `(sh-eval-file "FILE" 'scheme)`
8. inside schemesh using Scheme syntax, by evaluating `(sh-eval-file "FILE" 'shell)`

In 2. and 8. FILE is assumed to start in shell syntax.
Obviously, the current syntax can be changed from inside the file itself with `#!scheme` `#!shell` `{...}` `(...)`.

In 3. and 7. FILE is assumed to start in Scheme syntax.
Again, the current syntax can be changed from inside the file itself as described above.

In all other cases, the initial syntax is deduced from FILE name:
* file names ending with `.sh`, or without dots in the name, are assumed to start in shell syntax
* all other files assumed to start in Scheme syntax

As usual, the current syntax can be changed from inside the file itself with `#!scheme` `#!shell` `{...}` `(...)`

In files, the special case described for [REPL](#REPL) that also allows Scheme expressions in shell syntax
is **not** guaranteed to work: if it does, it's accidental and may be removed in future releases.
This means:
* in scheme syntax, each top-level item must parse to an expression
* in shell syntax, each top-level item must parse to a job

For details about valid shell syntax, see [doc/shell/syntax.md](syntax.md).

### Mixed syntax: shell inside Scheme

From Scheme syntax, shell syntax `{...}` can be inserted in any place where a Scheme **expression** is expected (which is almost everywhere),
and the shell syntax will evaluate to a job object.

Examples:
```
(define job {ls -l /home})
(sh-run/i job)

(do ([i 0 (fx1+ i)])
    ((fx>=? i 10))
  (display i)
  (newline)
  (sh-run {sleep 1}))
```

Also, a directive `#!shell` can appear at top-level: it allows switching to shell syntax.

Finally, a directive `#!shell` can appear **inside** a Scheme expression.
This is usually unnecessary, as the syntax `{...}` is both shorter and more readable:
* `(#!shell foo bar)` is equivalent to `{foo bar}` and evaluates to a job object
   
   Example: `(#!shell alias m less)` is equivalent to `{alias m less}`

   Note: if you are at top-level, the job is also executed.
   
* `(foo #!shell bar)` is equivalent to `(foo {bar})`

   Example: `(sh-run #!shell echo $LANG)` is equivalent to `(sh-run {echo $LANG})`
   and both execute the job `{echo $LANG}` which displays the value of environment variable `$LANG`


### Mixed syntax: Scheme inside shell

From shell syntax, inserting a Scheme expression is powerful although slightly less regular.
There are several places where a Scheme expression `(...)` can be inserted:

* after a directive `#!scheme` appearing at top-level
  This is the most common usage, as it allows alternating shell syntax and Scheme expressions.
  It is commonly used in initialization file `~/.config/schemesh/repl_init.ss`

  Example:
  ```
  #!shell
  alias gdb gdb -q
  alias grep grep --color=tty
  global unset LS_COLORS

  #!scheme
  (define (hypot x y) (sqrt (+ (* x x) (* y y))))
  ```

* as a whole job.
  In this case, the Scheme expression must evaluate to a job object.  

  Examples:
  ```
  echo foo ; (sh-cmd "echo" "bar")
  echo more; $(begin (display "foo") (display "bar\n"))
  ```

  The last example uses Scheme job syntax `$(...)` - see [Scheme jobs](../../README.md#scheme-jobs)
  
  Note: in shell syntax, a newline is equivalent to `;` in most cases,
  thus a Scheme expression appearing on its own line must evaluate to a job object (unless you are at REPL).  
  
  Example:
  ```
  #!shell
  echo started
  (sh-cmd "echo" "done")
  ```

* as an argument for a shell command.

  Example:
  ```
  ls (string-append "/usr/" "local")
  ```
  In this case, the Scheme expression must evaluate to one of:
  1. a string
  2. a list of strings
  3. a closure accepting zero or one arguments, that returns a string
  4. a closure accepting zero or one arguments, that returns a list of strings
  
  The expression will be evaluated only once, when the job object is created.
  
  Any returned closure will be executed **each time** the job is started,
  and if the closure accepts one argument, it will receive the job itself.

* the last case is currently not documented: a directive `#!scheme` appearing not at top-level.
