# Frequently asked questions

* [How does schemesh differ from other Lisp-based shells?](#how-does-schemesh-differ-from-other-lisp-based-shells)

* [How does schemesh differ from other non-Lisp shells?](#how-does-schemesh-differ-from-other-non-lisp-shells)

* [How to set environment variables?](#how-to-set-environment-variables)

* [How to mix shell and Scheme syntax in a script?](../shell/mixed_syntax.md#files)

* [Why another shell?](#why-another-shell)

* [Why a shell scriptable in Chez Scheme?](#why-a-shell-scriptable-in-chez-scheme)

* [Why not a Lisp-like syntax for shell jobs?](#why-not-a-lisp-like-syntax-for-shell-jobs)


## How does schemesh differ from other Lisp-based shells?

Schemesh is intended as an interactive shell and REPL:
it supports line editing, autocompletion, searchable history, aliases, builtins,
customizable prompts, and automatic loading of `~/.config/schemesh/repl_init.ss`.

Most importantly, it also has job control (CTRL+Z, `fg`, `bg` etc.)
and recognizes and extends Unix shell syntax for starting, redirecting and composing jobs.

Schemesh author is not aware of any other Lisp-based shell that supports **all** of these features.

All other known Lisp-based shells lack at least job control,
i.e. the ability to suspend a job and resume it in the background or foreground.

Some alternative shells also have additional limitations.


### Scsh

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

### Rash

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


### Eshell

[Eshell](https://www.gnu.org/software/emacs/manual/html_mono/eshell.html) is a shell written in EmacsLisp.

It is included in most versions of Emacs, and can be started from inside Emacs with `M-x eshell`.

Eshell is a shell-like command interpreter, with an interface similar to command shells such as bash, zsh, rc, or 4dos.
As such, it has line editing, autocompletion, history, aliases, builtins,
and recognizes Unix shell syntax for starting, redirecting and composing jobs.

The main limitations are:

1. no job control
2. Eshell runs **inside** Emacs, so it's difficult to use as a login shell.
3. pipelines are supported, but in some cases they work differently from POSIX shell semantics:

   For example, `ls | less` shows `ls` output in one Emacs buffer i.e. without piping it into `less`,
   while in another Emacs buffer it shows an error message "Missing filename" from `less`


## How does schemesh differ from other non-Lisp shells?

To be written


## How to mix shell and Scheme syntax in a script?

See [Mixing shell and Scheme syntax: FILES](../shell/mixed_syntax.md#files)


## How to set environment variables?

The general shell syntax is:
```shell
global set NAME VALUE
```
possibly followed by
```shell
global export NAME
```
The prefix `global` is needed because each job has its own environment variables,
and `global` instructs to modify the top-most job, i.e. the shell itself.
For more details, see [Shell builtins: set](../shell/builtins.md#set)

At REPL top-level, the shorter `set NAME VALUE` optionally followed by `export NAME` suffices too.


## Why another shell?

The author was dissatisfied with traditional shells: they are more or less adequate for launching programs and pipelines,
including job control, but their features for general-purpose programming varies from non-existent to awful.

In the author's opinion, traditional shell scripts are an exercise in compulsive quoting (it's never enough)
and in frustration for the lack of a reasonable type system:
most traditional shells only know about strings, and even **returning** a string is often not supported.

Dealing with numbers, vectors, maps, timestamps etc. in a traditional shell quickly becomes unfeasible.<br/>
The usual reply to such an objection is "for general-purpose programming, use a programming language - not a shell".<br/>
The issue with such solution is that launching and controlling jobs and pipelines from a general-purpose programming language
is non-trivial and extremely verbose.<br/>
Also, most general-purpose programming languages are not interactive.

Schemesh purpose is to fill this gap, i.e. it aims at being **both** a shell and an interactive general-purpose programming environment,
where the two aspects blend seamlessly: launching and controlling jobs and pipelines is simple and robust,
both from shell syntax and from Scheme syntax - see [README: Job control](../../README.md#job-control)


## Why a shell scriptable in Chez Scheme?

The reasons for creating a shell scriptable in a general-purpose programming language
are described in the previous question [FAQ: Why another shell?](#why-another-shell)

The answer to "why **Chez Scheme** specifically?" follows.

When implementing a shell, there are at least two programming languages involved:
1. the "host" language: a shell is internally implemented in some programming language
2. the "guest" language: a shell is a programmable environment, and can be scripted in some language

Ideally, the "host" and "guest" language would be the same:
this allows exposing (parts of) the internal implementation to the scripting environment,
enriching it without having to write lots of boilerplate for translating between "host" and "guest" languages.

It also ensures that the "guest" language is pre-existing and has some community around it:
creating a new ad-hoc language understood only by a single program (the new shell being created)
is both a large endeavour, and a significant obstacle to adoption.

This idea "host and guest language are the same" requires a REPL,
i.e. an interactive prompt that understands the same language used to implement it.

In most languages, a REPL that understands the language itself is not provided,
and implementing it is extremely difficult and time consuming:<br/>
think about implementing in C an **interactive** C interpreter or compiler,<br/>
or implementing in Rust an **interactive** Rust interpreter or compiler,<br/>
or implementing in Go an **interactive** Go interpreter or compiler.<br/>
where "interactive" means that code executed at REPL is immediately available for execution,<br/>
without having to load it from disk, and that the interactive environment exposes the **full** host language - not a subset.

Some languages natively offer a REPL that understands the language itself. Most notably:
* Python
* Haskell
* most Lisp dialects: Clojure, Common Lisp, Racket, Scheme...

Python would be a good choice for implementing a shell, if it wasn't so slow at runtime (it's an interpreter, after all)
and if its syntax was easier to extend - indeed, [XONSH](https://xon.sh/) is a popular shell scriptable in Python.

Haskell is a purely functional language, where side effects are strictly contained (and constrained).
It also has a rich library for creating parsers,
and a very sophisticated static type system - the latter can be quite heavyweight for interactive use.

Regarding Lisp dialects, the author looked for the following characteristics:
* open source license
* compiler (**not** an interpreter) generating optimized native code
* powerful FFI for interfacing with low-level POSIX API
* small memory footprint

[Clojure](https://clojure.org/) is quite memory hungry because it runs inside a JVM,
and it also restricts side effects **only** to memory transactions.<br/>
Calling C from Clojure/JVM is feasible, although it requires some boilerplate - see
[Java Native Access (JNA)](https://github.com/java-native-access/jna).

[Sbcl](https://www.sbcl.org/) is one of the best open-source Common Lisp optimizing compilers.
It has good FFI support and only consumes 20-22 MB RAM at startup,
at least if you don't load quicklisp and slime (then it grows to at least 100 MB).

[Racket](https://racket-lang.org/) is a multi-language environment:
nowadays it is backed by Chez Scheme and it shares Chez Scheme's strengths (see below),
except for RAM usage: it uses ~140 MB at startup, and more if you load some of its many libraries.

[Chez Scheme](https://cisco.github.io/ChezScheme/) is is one of the best open-source Scheme R6RS optimizing compilers, 
with sophisticated C FFI and only 22-25 MB RAM usage in its default configuration.
As a bonus, it can also be embedded into a user-created C executable.

In the end, the author decided for Chez Scheme.


## Why not a Lisp-like syntax for shell jobs?

This comes up periodically, mostly from Lisp programmers - especially the ones that know about [Scsh](#scsh).

An equivalent question is "What about a homoiconic shell syntax?"

The author considered a homoiconic i.e. Lisp-like syntax for creating and running shell jobs:
that's what scsh does, and it was one of their first attempts.

If implemented, it would look similar to:
```
(| (gzip -d) (tar tvf -) (wc -l))
```
i.e. shell commands would resemble Scheme forms, with some macro-like syntax for joining them (pipelines, and, or, ...).

Such approach has three main issues:

* the author wants schemesh to be a shell, and to accept as much as possible the traditional shell syntax.
  This greatly helps migration, as one does not have to learn a new language for simple use cases:
  the bash/zsh/... user base is much larger than the Lisp/Scheme user base.

* traditional shell syntax and semantics for jobs, redirections and pipelines is already terse and clean:
  let's reinvent the wheel only where really necessary.
  On the other hand, the syntax and semantics for general purpose programming in  traditional shells varies from non-existent to awful.

* the characters `.` `'` `` ` `` and `|` have special meaning in (Chez) scheme,
  and cannot be changed to their shell meaning in a scheme macro.


Interestingly, schemesh shell syntax **is** homoiconic in a non-trivial (and useful) way:
one can `(quote)` and `(expand)` it, and it evaluates to Scheme objects. Examples:
```
(begin '{ls | wc -l})
```
evaluates to the s-expr
```
(shell "ls" \x7C; "wc" "-l")
```
(if one squints hard enough, this is the homoiconic syntax peole are asking for)
and expanding it yields an s-expr containing calls to plain Scheme functions:
```
(expand '{ls | wc -l})
```
evaluates to the s-expr
```
(begin
   ; chez-scheme library stuff... then:
   (sh-pipe* (sh-cmd "ls") '\x7C; (sh-cmd "wc" "-l")))
```
Final step: removing the `quote`,
```
(begin {ls | wc -l})
```
evaluates to a Scheme datum (a `sh-job` record-type) that can be inspected, modified, launched, etc. and pretty-prints to
```
(sh-pipe (sh-cmd "ls") (sh-cmd "wc" "-l"))
```
The last fragment above is also machine-readable: if evaluated, it creates and returns an equivalent `sh-job` datum.
