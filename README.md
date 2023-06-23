# schemesh
## Fusion between a Unix shell and a Lisp REPL

### Current status: ALPHA.
### Some features work, many others are incomplete or missing.
### Use at your own risk!

Schemesh is a fusion between a Unix shell and a Lisp REPL - Chez Scheme REPL, to be exact.

It supports interactive line editing and familiar POSIX shell syntax for starting commands,
including redirections, pipelines, job concatenation with `&&` `||`, groups surrounded by `{ }`,
and managing foreground/background jobs.

For scripting and serious programming, it completely replaces the slow, clumsy scripting language
of a traditional shell (yes, the author has opinions) with a full-featured Lisp REPL, backed by
a fast open-source Lisp compiler that generates highly optimized native code.

This means you can use Lisp control structures, loops and functions such as
```lisp
(if (some_expression arg1 (sub_expression2))
  (then_run_here)
  (otherwise_run_here))
```
instead of typical shell syntax, which is error prone as it's based on string expansion and splitting,
and geared toward command execution, as for example:
```shell
if some_command "$arg1" "$(sub_command)"
then
  then_run_this_command
else
  else_run_this_command
fi
```

Switching between shell syntax and Lisp syntax is extremely simple, and can be done basically everywhere:
* open parenthesis i.e. `(` temporarily switches to Lisp syntax until the corresponding closed parenthesis i.e. `)`

* open brace i.e. `{` temporarily switches to shell syntax until the corresponding closed brace i.e. `}`

* the directives `#!scheme` `#!chezscheme` and `#!r6rs` temporarily switch to Lisp syntax
  (with the appropriate flavor) until the end of current list or group inside `( )` or `{ }`.
  If entered at top level, they change the default syntax until another directive is entered at top level.

* the directive `#!shell` temporarily switches to shell syntax until the end of current list or group inside `( )` or `{ }`.
  If entered at top level, it changes the default syntax until another directive is entered at top level.

Examples:

```shell
find (lisp-function-returning-some-path) -type f | grep ^lib | wc -l
```

```lisp
(define job {ls -l | grep some-file-name})
(sh-start job)
(sh-fg job)
```

Schemesh can be used as:
* a replacement for traditional interactive Unix shell, as for example bash/ksh/zsh etc.

* a Unix shell scriptable in Lisp

* a Lisp REPL with additional syntax and functions to start, redirect and manage Unix processes

* a Lisp library for starting, redirecting and managing Unix processes
