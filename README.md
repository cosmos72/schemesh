# schemesh
## Fusion between a Unix shell and a Lisp REPL

### Current status: ALPHA.
### Some features work, many others are incomplete or missing.
### Use at your own risk!

Schemesh is an interactive shell scriptable in Lisp.

It is primarily intended as a user-friendly Unix login shell, replacing bash, zsh, pdksh etc.

As such, it supports interactive line editing and familiar Unix shell syntax:
it can start commands, including redirections, pipelines, job concatenation with `&&` `||`,
groups surrounded by `{ }`, subshells surrounded by `[ ]`, and manage foreground/background jobs.

For more complex tasks, it seamlessly integrates a full Chez Scheme REPL.

Features:
- [x] REPL with multi-line editing, history and parentheses highlighting
- [ ] cut-and-paste
- [x] context-sensitive autocompletion
- [x] dual syntax parsing, allows mixing Scheme and shell expressions
- [x] shell commands, including `&&` `||` `{` `}` `[` `]`
- [x] shell job control
- [x] shell aliases
- [ ] shell builtins - in progress
- [x] shell environment variables
- [x] shell pipelines `|`
- [x] shell redirections `<` `>` `<>` `>>` `<&` `>&` `$()` ``` `` ```
- [ ] shell wildcard expansion

Schemesh can be used as:
* a replacement for traditional interactive Unix shell, as for example bash/zsh/pdksh etc.

* a Unix shell scriptable in Chez Scheme.

* a Scheme REPL with additional syntax and functions to start, redirect and manage Unix processes

* a Scheme library for starting, redirecting and managing Unix processes

For scripting and serious programming, schemesh completely replaces the slow, clumsy and error-prone
scripting language of a traditional shell (yes, the author has opinions) with a full-featured Lisp REPL,
backed by the fast open-source Chez Scheme compiler that generates highly optimized native code.

This means you can mix shell command execution with Lisp control structures, loops and functions as for example
```lisp
(if (some_expression arg1 (sub_expression2))
  (sh-run/i {then_run_this_command foo bar $VAR})
  (sh-run/i {else_run_this_command foo bar $VAR}))
```
instead of typical shell syntax, which is error prone as it's based on string expansion and splitting,
and geared toward command execution, as for example:
```shell
if some_command "$arg1" "$(sub_command)"
then
  then_run_this_command foo bar $VAR
else
  else_run_this_command foo bar $VAR
fi
```

Switching between shell syntax and Lisp syntax is extremely simple, and can be done basically everywhere:
* open parenthesis `(` temporarily switches to Lisp syntax until the corresponding closed parenthesis `)`

* open brace i.e. `{` temporarily switches to shell syntax until the corresponding closed brace i.e. `}`

* open bracket i.e. `[` starts a new sub-form in current syntax until the corresponding closed bracket i.e. `]`
  If found in Lisp syntax, it is equivalent to `(`
  If found in shell syntax, it is similar to `{` with the difference that commands will be executed in a subshell.

* the directives `#!scheme` `#!chezscheme` and `#!r6rs` temporarily switch to Scheme syntax
  (with the appropriate flavor) until the end of current list or group inside `( )`, `[ ]` or `{ }`.
  If entered at top level, they change the default syntax until another directive is entered at top level.

* the directive `#!shell` temporarily switches to shell syntax until the end of current list or group
  inside `( )`, `[ ]` or `{ }`.
  If entered at top level, it changes the default syntax until another directive is entered at top level.

* shell syntax creates Lisp (sh-cmd) and (sh-job) objects, which can be started/stopped/managed from both syntaxes

Examples:

```shell
find (lisp-function-returning-some-path) -type f | grep ^lib | wc -l &
fg
```

```lisp
(define job {ls -l > ls.out || echo "ls failed"})
(sh-start job)
(sh-fg job)
```
