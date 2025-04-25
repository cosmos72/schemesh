

# Comparison between schemesh and other Lisp-based shells

Schemesh is intended as an interactive shell and REPL:
it supports line editing, autocompletion, searchable history, aliases, builtins,
a customizable prompt, and automatic loading of `~/.config/schemesh/repl_init.ss`.

Most importantly, it also has job control (CTRL+Z, `fg`, `bg` etc.)
and recognizes and extends Unix shell syntax for starting, redirecting and composing jobs.

Schemesh author is not aware of any other Lisp-based shell that supports *all* of these features.

All other known Lisp-based shells lack at least job control,
i.e. the ability to suspend a job and resume it in the background or foreground.

Some alternative shells also have additional limitations.


## Scsh

[Scsh](https://scsh.net/) is a Lisp programming environment built on top of [Scheme 48](https://www.s48.org/),
with additional Scheme functions for running programs and setting up pipelines and redirections,
and a syscall library for low-level access to the operating system.

As stated in [Scsh Reference manual - Caveats](https://scsh.net/docu/html/man-Z-H-2.html#node_sec_1.4)
> Scsh, in the current release, is primarily designed for the writing of shell scripts -- programming.
> It is not a very comfortable system for interactive command use:
> the current release lacks job control, command-line editing, a terse, convenient command syntax,
> and it does not read in an initialisation file analogous to .login or .profile

Also, scsh does not allow a lone dot `.`
* neither as a shell command (the traditional meaning is "load and execute the file specified as argument")
* nor as a shell command argument (it traditionally indicates the current directory)

The reason for this limitation is:
scsh syntax for creating shell commands is a DSL, i.e. a domain-specific language, built using macros that work on top of Scheme syntax.
And in Scheme, the dot `.` is a low-level syntactic token with its own meaning - it cannot be used as an identifier.

## Rash

[Rash](http://rash-lang.org/) is a shell language, library, and REPL for [Racket](https://racket-lang.org/).

As explained in [Rash github page](https://github.com/willghatch/racket-rash#getting-started) it can be used as:
* a repl that is as convenient for pipelining programs as Bash is, but has all the power of Racket.
* a scripting language with #lang rash.
* embedded in normal Racket files with (require rash), and mixed freely with any other Racket language or library.

And indeed it is very similar to schemesh in many aspects:

both aim to create an interactive, Unix-like shell scriptable in some dialect of Lisp,
and both support line editing, autocompletion, and Unix shell syntax for starting, redirecting and composing jobs.

Rash has several limitations, sometimes due to design choices, that schemesh solves:

1. no job control
2. multi-line editing is limited
3. shell commands are Lisp functions, not Lisp objects.
   Inspecting and redirecting them after they have been created is difficult
4. being written in Racket, has larger RAM footprint than schemesh running on vanilla Chez Scheme:
   at startup, ~160MB vs. ~40MB
5. support for multi-language at REPL is limited: once you do `#lang racket`, you cannot go back to `#lang rash`
   This is a limitation imposed by the underlying Racket system, and Rash can do relatively little to remove it.


## Eshell

[Eshell](https://www.gnu.org/software/emacs/manual/html_mono/eshell.html) is a shell written in EmacsLisp.

It is included in most versions of Emacs, and can be started from inside Emacs with `M-x eshell`.

Eshell is a shell-like command interpreter, with an interface similar to command shells such as bash, zsh, rc, or 4dos.
As such, it has line editing, autocompletion, history, aliases, builtins,
and recognizes Unix shell syntax for starting, redirecting and composing jobs.

The main limitations are:

1. no job control
2. Eshell runs *inside* Emacs, so it's difficult to use as a login shell.
3. pipelines are supported, but in some cases they work differently from POSIX shell semantics:

   For example, `ls | less` shows `ls` output in one Emacs buffer i.e. without piping it into `less`,
   while in another Emacs buffer it shows an error message "Missing filename" from `less`


# Comparison between schemesh and other non-Lisp-based shells

To be written
