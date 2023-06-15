# schemesh
## Unix shell embedded in a Chez Scheme REPL

### Current status: ALPHA.
### Some features work, many others are incomplete or missing.
### Use at your own risk!

Schemesh is a Unix shell, compatible with POSIX shell syntax
and powered by a Lisp REPL.

Think about it as an alternative to bash/ksh/zsh, but with the full Lisp language
always available - instead of the clumsy scripting language of a traditional shell
(yes, the author has opinions).

Schemesh can be used as:
* a replacement for traditional interactive Unix shell, as for example bash/ksh/zsh etc.

* a Unix shell scriptable in Lisp (to be precise, in Chez Scheme)

* a Lisp REPL with additional syntax and functions to start, redirect and manage Unix processes

* a Lisp library for starting, redirecting and managing Unix processes

Examples:

```shell
find /usr/lib -type f | grep ^lib | wc -l
```

```lisp
(define job {ls -l | grep some-file-name})
(sh-start job)
(sh-fg job)
```
