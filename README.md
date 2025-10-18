# schemesh
# Fusion between a Unix shell and a Lisp REPL

Schemesh is an interactive shell scriptable in Lisp.

It is primarily intended as a user-friendly Unix login shell, replacing bash, zsh, pdksh etc.

As such, it supports interactive line editing, autocompletion, history and the familiar Unix shell syntax:
it can start commands, including redirections, pipelines, job concatenation with `;` `&&` `||`,
groups surrounded by `{ }`, subshells surrounded by `[ ]` (traditional shells use `( )`),
command substitution surrounded by `$[ ]` (traditional shells use `$( )`),
and manage foreground/background jobs.

For more complex tasks, it seamlessly integrates a full Lisp REPL backed by Chez Scheme.

Schemesh can be used as:
* a replacement for traditional interactive Unix shell, as for example bash/zsh/pdksh etc.

* a Unix shell scriptable in Chez Scheme:<br/>
  just execute the command `schemesh PATH-TO-SOME-FILE`.

  You can also create a schemesh script file, let's say `my_script`,
  write `#!/usr/bin/env schemesh` in its first line, then  `chmod +x my_script`
  and launch it as an executable by typing `./my_script`

* a Scheme REPL with additional syntax and functions to start, redirect and manage Unix processes.

* a Scheme library for starting, redirecting and managing Unix processes:
  see [Loading schemesh from plain Chez Scheme](doc/schemesh_as_library.md)

For scripting and serious programming, schemesh completely replaces the slow, clumsy and error-prone
scripting language of a traditional shell (yes, the author has opinions) with a full-featured Lisp REPL,
backed by the fast and open-source Chez Scheme compiler that generates highly optimized native code.

## How to use

As a traditional Unix shell: type a command and press Enter.<br/>
As a Lisp REPL: type an expression starting with `(` and press Enter.<br/>
![](doc/screenshot-1.png)

If the parentheses/braces/brackets/quotes are balanced,<br/>
schemesh will execute the command and show any failure, or evaluate the expression and pretty-print its value.

If the parentheses/braces/brackets/quotes are *not* balanced,<br/>
schemesh will create a second line where you can continue typing.<br/>
You can move between lines with the cursor keys, and use all the classical line-editing features including cut-and-paste.<br/>
![](doc/screenshot-2.png)

### Switching syntax shell <-> Lisp

Switching between shell syntax and Lisp syntax is extremely simple, and can be done basically everywhere:
* open parenthesis `(` temporarily switches to Lisp syntax until the corresponding `)`.

* open brace i.e. `{` temporarily switches to shell syntax until the corresponding `}`.

* open bracket i.e. `[` starts a new sub-form in current syntax until the corresponding `]`.<br/>
  If found in Lisp syntax, it is equivalent to `(`.<br/>
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

### Job control

Shell syntax creates first-class Lisp `sh-job` objects, which can be started/stopped/managed from both syntaxes.

The most common mechanisms to start/stop/manage jobs from shell syntax are:
* CTRL+C        interrupt the current foreground job
* CTRL+Z        suspend the current foreground job
* `bg job-id`   resume a job in background, return immediately
* `fg job-id`   resume a job in foreground, wait until job finishes or is suspended
* `wait job-id` resume a job in foreground, wait until job finishes

The analogous job control mechanisms from Scheme syntax are:
* CTRL+C      as above
* CTRL+Z      as above
* `(sh-start job-object)` start a job in background, return immediately
* `(sh-run/i job-object)` start a job in foreground, wait until job finishes or is suspended
* `(sh-run   job-object)` start a job in foreground, wait until job finishes
* `(sh-bg    job-or-id)` resume a job in background, return immediately
* `(sh-fg    job-or-id)` resume a job in foreground, wait until job finishes or is suspended
* `(sh-wait  job-or-id)` resume a job in foreground, wait until job finishes

Some more advanced Scheme functions:
* `(sh-run/bytevector job-object)` start a job in foreground, wait until job finishes, return its output as a Scheme bytevector
* `(sh-run/string job-object)` start a job in foreground, wait until job finishes, return its output as a Scheme string
* `(sh-start/fd-stdout job-object)` start a job in background, return a file descriptor fixnum<br/>
  for reading job's standard output - for example with `(open-fd-input-port fd)` or `(fd-read-some fd bytevector)`
  File descriptor must be closed with `(fd-close)` when no longer needed.
* `(sh-start/fds job-object)` start a job in background, return a list containing file descriptors<br/>
  connected to the spawned subprocess.<br/>
  File descriptors must be closed with `(fd-close)` when no longer needed.
* `(sh-start/ports job-object)` start a job in background, return a list containing binary or textual ports<br/>
  connected to the spawned subprocess.<br/>
  Ports must be closed with `(close-port)` when no longer needed.
* `(sh-redirect job-object fd dir to [more-redirections ...])` add redirections to an existing job, without starting it.

For more information about functions for redirecting jobs input and output, see [doc/shell/redirect.md](doc/shell/redirect.md)

### Subshells and command substitution

From shell syntax, commands can be executed in a subshell by surrounding them in `[ ]` as for example:
```shell
grep -q old *.txt && [ sed -i -e 's/old/new/g' -- *.txt ]
```
traditional shells typically start subshells with `( )`, which has a different meaning in schemesh.

Command substitution, i.e. using output of a first command as argument for a second command,
can be performed by surrounding the first command in ``` `` ``` or `$[ ]` - example:
```shell
> NOW=$[date]
> echo $NOW
Mon Mar 31 12:00:01 PM GMT 2025
```
traditional shells typically perform command substitution with ``` `` ``` or `$( )`:
the former is supported, while the latter has a different meaning in schemesh - see Scheme jobs below.

### Scheme jobs

Shell jobs can also contain arbitrary Scheme code:

from shell syntax or Scheme syntax, type `$` before a Scheme expression in parentheses,
and it gets encapsulated in a job that can be started, stopped and resumed just like any other job.
Example:
```shell
> $(begin (repeat 1000000000 (void)) "done!\n")

CTRL+Z
; job  1+            (stopped sigtstp)  $(begin (repeat 1000000000 (void)) "done!\n")
(stopped sigtstp)

> fg 1
; job  1+            (running 1)        $(begin (repeat 1000000000 (void)) "done!\n")
; job  1+            (ok "done!\n")     $(begin (repeat 1000000000 (void)) "done!\n")
(ok "done!\n")
```

This feature is named "Scheme jobs" and can also be written in long form `(shell-expr ...)`
both from shell syntax and from Scheme syntax.

If used from Scheme syntax, the job created by short or long form can be executed immediately,
or saved in a variable for later execution, or passed to a function, etc.
Example:
```lisp
> (define j $(begin (repeat 1000000000 (void)) "done too!\n"))
> (sh-run/i j)
(ok "done too!\n")
```

By default, Scheme jobs cannot run in background - they stop themselves if you try.

If you need to run a Scheme job in background, place it inside a subshell
or inside a pipeline (exception: if the last job in a pipeline is a Scheme job,
it cannot run in background).

Example:
```shell
> sleep 5 | $(display "hello") | cat | $(get-string-all)
CTRL+Z
; job  1+            (stopped sigtstp)  {sleep 5 | $(display "hello") | cat | $(get-string-all)}
(stopped sigtstp)

> fg 1
; job  1             (ok "hello")       {sleep 5 | $(display "hello") | cat | $(get-string-all)}
(ok "hello")
```

The examples above also show an additional feature of Scheme jobs:
they can exit with a status containing an arbitrary Scheme value
or even multiple values, not just an 8-bit exit status.

To inspect a job status, use `(status->kind)` and `(status->value)`, as for example:
```lisp
> (define j {ls /does-not-exist})
> (define status (sh-run j))
ls: cannot access '/does-not-exist': No such file or directory

> (display status)
(failed 2)

> (status->kind status)
failed

> (status->value status)
2
```

If a job exited successfully, `(status->kind status)` will return the symbol `'ok` and `(ok? status)` will return `#t`.
In the case of Scheme jobs that return multiple values, use `(ok->list)` or `(ok->values)`
to access all the values, because `(status->value)` returns only the first one:
```lisp
> (define j $(values "a" 'b #\c))
> (define status (sh-run j))
> (display status)
(ok "a" b #\c)

> (status->kind status)
ok

> (status->value status)
"a"

> (ok->list status)
("a" b #\c)

> (ok->values status)
"a"
b
#\c
```

### Scheme ports redirection

Standard Scheme textual ports `(current-input-port)` `(current-output-port)` `(current-error-port)`
automatically honor job redirections. Example:
```shell
> $(display "hello from Scheme!\n") > greet.txt

> cat greet.txt
hello from Scheme!
```

If you prefer binary ports, you can use `(sh-stdin)` `(sh-stdout)` and `(sh-stderr)` instead:<br/>
they automatically honor job redirections too. Example:
```lisp
> (put-bytevector (sh-stdout) #vu8(72 105 33 10))
Hi!
```

See [doc/shell/redirect.md](doc/shell/redirect.md) for more details.

### Shell wildcards

Shell wildcards and other shell syntax that expands to strings can also be used from Scheme:
* macro `(shell-glob {...})` returns a list of strings
* macro `(shell-string {...})` returns a single string

Examples:
```lisp
> (shell-glob {/*})
("/bin" "/boot" "/dev" "/etc" "/home" "/lib" "/lost+found" "/mnt"
 "/proc" "/root" "/run" "/sbin" "/srv" "/sys" "/tmp" "/usr" "/var")

> (for-list ((i (shell-glob {*.[ch]})))
    (format #t "~s " i))
"chezscheme.h" "eval.c" "eval.h" "main.c"

> (shell-string {$PATH:$HOME/.local/bin})
"/usr/local/bin:/usr/bin:/bin:/home/user/.local/bin"

> (shell-string {abc`echo def`})
"abcdef"
```

Together, the macros `(shell-glob)` and `(shell-string)` can be used to replace most traditional shell-based loops
with faster Scheme-based iteration that is immune to classic shell scripting pitfalls:
spaces or newlines in file names, paths starting with `-`, misconfigured environment variable `$IFS`, etc.


## Full Scheme REPL

Schemesh contains a **full** Chez Scheme REPL:<br/>
you can define variables, functions, macros, libraries, modules and use them with the classic Scheme syntax
```lisp
(define (add a b) (+ a b))
(add 7/3 (add 7/6 7/2))
7

(define-syntax while
  (syntax-rules ()
    ((_ pred body ...) (do () ((not pred)) body ...))))

(library (hello world)
  (export greet)
  (import (rnrs)
          (only (chezscheme) format))
  (define (greet who)
    (format #t "Hello, ~a!\n" who)))

(import (hello world))

(greet "User")
Hello, User!
```

You can compile and load Scheme files and libraries,
including third-party libraries as the ones packaged by [https://akkuscm.org/](https://akkuscm.org/)
by following the same instructions as for Chez Scheme.


## Examples

You can mix shell command execution with Lisp control structures, loops and functions as for example:
```lisp
(if (some_expression arg1 (sub_expression2))
  (sh-run/i {then_run_this_command foo bar $VAR})
  (sh-run/i {else_run_this_command foo bar $VAR}))
```
instead of typical shell syntax, which is error prone as it's based on string expansion and splitting,
and geared toward command execution, as for example:
```shell
# Note: this is POSIX shell syntax for `if-then-else`. It will NOT work in schemesh.
if some_command "$arg1" "$[sub_command]"
then
  then_run_this_command foo bar $VAR
else
  else_run_this_command foo bar $VAR
fi
```
other examples mixing shell and Lisp syntax:
```shell
find (lisp-function-returning-some-string) -type f | grep ^lib | wc -l &
fg 1
```

```lisp
(define job {ls -l > ls.out || echo "ls failed"})
(sh-start job)
(sh-fg job)
```

```lisp
(define txt (sh-run/string {git log}))
(display txt)
```

#### More examples

```shell
sed -i -e 's/old/new/g' -- (directory-list ".")
```
or, if you want to run a separate `sed` command for each file,
```lisp
(for-each
  (lambda (f)
    (sh-run {sed -i -e 's/old/new/g' -- (values f)}))
  (directory-list "."))
```

Also, `(sh-run/string-split-after-nuls)` combines well with `{find ... -print0}`. Example:
```lisp
(for-each
  (lambda (f)
    (file-rename f (string-replace-suffix f ".old" ".bak")))
  (sh-run/string-split-after-nuls {find -type f -print0}))
```
which can also be written as
```lisp
(for-list ((f (sh-run/string-split-after-nuls {find -type f -print0})))
  (file-rename f (string-replace-suffix f ".old" ".bak")))
```
or even
```lisp
(for ((f (in-list (sh-run/string-split-after-nuls {find -type f -print0}))))
  (file-rename f (string-replace-suffix f ".old" ".bak")))
```
the example above has the advantage that `for` can iterate simultaneously
on multiple heterogenous containers: lists, strings, vectors, hashtables, etc. ...

#### Even more examples

As a last example, all the following are equivalent:
```shell
{
    TEMP=my-temp-dir
    mkdir $TEMP                   &&
    cd    $TEMP                   &&
    tar xvf ../hello-world.tar.xz &&
    cd    hello-world             &&
    ./configure                   &&
    make -j`nproc`
}
```
note: in schemesh versions >= 0.8.0, the character `\` at the end of a line,
for continuing a shell command in the next line,
is optional after `{` `[` `!` `;` `&` `&&` `||` `|` `|&` `<` `<>` `>` `>>` `<&` or `>&`
```lisp
(let ((temp "my-temp-dir"))
  (sh-run/i
    {
      mkdir (values temp)           &&
      cd    (values temp)           &&
      tar xvf ../hello-world.tar.xz &&
      cd    hello-world             &&
      ./configure                   &&
      make -j`nproc`
    }))
```
```lisp
(let ((temp "my-temp-dir"))
  (sh-run/i
    (sh-and
      {mkdir (values temp)}
      {cd    (values temp)}
      {tar xvf ../hello-world.tar.xz}
      {cd    hello-world}
      {./configure}
      {make -j`nproc`}
    )))
```
```lisp
(let ((temp "my-temp-dir"))
  (sh-run/i
    (sh-and
      (sh-cmd "mkdir" temp)
      (sh-cmd "cd"    temp)
      (sh-cmd "tar" "xvf" "../hello-world.tar.xz")
      (sh-cmd "cd"    "hello-world")
      (sh-cmd "./configure")
      (sh-cmd "make" (shell-wildcard "-j" (shell-backquote "nproc")))
    )))
```

Note: by design, `cd` builtin and setting environment variables have local scope:<br/>
their effect is limited to the surrounding `{ }` or `[ ]` or parent `(sh-...)` job.<br/>
If you want them to have global effect, use `global cd SOME-DIR` or `global set NAME VALUE`

There is no need to memorize the correspondence between shell syntax<br/>
and its corresponding Scheme counterpart. You can type one of:
* `(values '{shell syntax})`
* `(expand '{shell syntax})`
* `(values {shell syntax})` note: **no** quotes in this last expression

and inspect the Scheme source or objects generated by shell syntax:
```lisp
> (values '{make > log && make install >> log && echo done})
(shell "make" > "log" && "make" "install" >> "log" && "echo" "done")
```
```lisp
> (expand '{make > log && make install >> log && echo done})
(begin
  (#3%$invoke-library
    '(schemesh shell job)
    '(0 9 2)
    '#{job c35q9golxfpwdr5y269nhygk1-54})
  (sh-and
    (sh-cmd* "make" 1 '> "log")
    (sh-cmd* "make" "install" 1 '>> "log")
    (sh-cmd "echo" "done")))
```
```lisp
> (values {make > log && make install >> log && echo done})
(sh-and (sh-cmd* "make" 1 '> "log")
        (sh-cmd* "make" "install" 1 '>> "log")
        (sh-cmd "echo" "done"))
```

### Features
- [x] REPL with multi-line editing and parentheses highlighting
- [x] dual syntax parsing, allows mixing Scheme and shell expressions
- [x] shortcuts to execute commands by pressing a single key: KP/ KP* KP-
- [x] history searchable with PageUp and PageDown keys
- [x] cut-and-paste
- [x] context-sensitive autocompletion - some improvements pending
- [x] UTF-8b for losslessly converting file names and environment variables that are not valid UTF-8
- [x] shell commands, including `;` `&` `!` `&&` `||` `{` `}` `[` `]`
- [x] shell job control
- [x] shell aliases
- [x] shell builtins
- [x] shell environment variables
- [x] shell pipelines `|` `|&`
- [x] shell subshells `[ ]`
- [x] shell command substitution `$[]`  ``` `` ```
- [x] shell redirections `<` `>` `<>` `>>` `<&` `>&`
- [x] shell wildcard expansion
- [x] shell jobs executing arbitrary Scheme code `$()` or `some-command (lambda () ...)`
- [x] each job has its own current directory and environment variables,
      initially inherited from parent job
- [x] customizable prompt, set by environment variable `$SCHEMESH_PS1` or by calling a user-defined function
- [x] if the directory `$HOME/.cache/schemesh/` exists,<br/>
      history is automatically saved to and loaded from a file `history.txt` inside such directory
- [x] if the file `$HOME/.config/schemesh/repl_init.ss` exists,<br/>
      it is automatically executed when starting the REPL
- [x] if the file `$HOME/.config/schemesh/repl_quit.ss` exists,<br/>
      it is automatically executed when exiting the REPL


## Build instructions

#### Debian Linux
```shell
sudo apt update
sudo apt install build-essential chezscheme-dev liblz4-dev libncurses-dev git uuid-dev zlib1g-dev
git clone https://github.com/cosmos72/schemesh
cd schemesh
git checkout -f v0.9.1
make -j

# try schemesh without installing it
./schemesh --library-dir .

# install schemesh
sudo make install
```

#### Ubuntu Linux
Follow the same instructions as for [Debian Linux](#debian-linux) above.

If you get errors similar to
```
lto1: fatal error: bytecode stream in file '/usr/lib/csv.../ta6le/kernel.o' generated with LTO version NNN instead of the expected MMM
```
then replace the command `make -j` with `make -j CC='gcc -fno-lto'`

#### Fedora Linux
```shell
sudo dnf install gcc make chez-scheme-devel lz4-devel ncurses-devel git libuuid-devel zlib-devel
git clone https://github.com/cosmos72/schemesh
cd schemesh
git checkout -f v0.9.1
make -j

# try schemesh without installing it
./schemesh --library-dir .

# install schemesh
sudo make install
```

#### Windows
On Windows 10 or later, install [Windows Subsystem for Linux (WSL)](https://learn.microsoft.com/en-us/windows/wsl/)
then, at Linux shell prompt, follow the Linux instructions above matching the distribution that you installed:
by default, WSL installs [Ubuntu Linux](#ubuntu-linux)

#### FreeBSD
```shell
pkg install chez-scheme gcc git gmake  # must be executed as root
git clone https://github.com/cosmos72/schemesh
cd schemesh
git checkout -f v0.9.1
gmake -j

# try schemesh without installing it
./schemesh --library-dir .

# install schemesh
gmake install  # must be executed as root
```

#### Mac OS X

*WARNING* support for Mac OS X is only slightly tested.

```shell
sudo xcode-select --install # only needed if you don't already have XCode Command Line Tools
brew install chezscheme lz4
git clone https://github.com/cosmos72/schemesh
git checkout -f v0.9.1  # version v0.8.3 does not support Mac OS X
cd schemesh
make -j

# try schemesh without installing it
./schemesh --library-dir .

# install schemesh
sudo make install
```

#### Nix/NixOS
```shell
# Build and run by cloning source repository
git clone https://github.com/cosmos72/schemesh
cd schemesh
nix-build && ./result/bin/schemesh
```

```nix
# Install on NixOS as package
{ pkgs, ... }:
let
  schemesh = pkgs.fetchgit {
    url = "https://github.com/cosmos72/schemesh.git";
    rev = "refs/tags/v0.9.1"; # or: "refs/heads/main"
    sha256 = ""; # insert sha256 when ready
  };
{
  environment.systemPackages = [
    # ...
    pkgs.chez
    (callPackage schemesh {})
  ];
}
```

#### other systems
For Unix-like systems not listed above, the instructions above can (hopefully) be adapted as needed.

If all went well, you can execute `schemesh`

In case your environment variable `$PATH` does not contain `/usr/local/bin`,
the command `schemesh` will not suffice - you will need to run `/usr/local/bin/schemesh`

#### Troubleshooting

If `make -j` fails, do not panic :) Some issues are relatively minor and can be fixed easily:

1. error messages like `scheme.h: No such file or directory` or `missing path after '-I'`
   or `Usage: ./utils/find_chez_scheme_kernel.sh CHEZ_SCHEME_DIR`
   indicate that `make` failed to autodetect Chez Scheme installation directory.

   In such case you can manually specify it, as for example `make -j CHEZ_SCHEME_DIR="/usr/local/lib/csv10.0.0/ta6le"`

2. on Ubuntu, you may get the error message
   `lto1: fatal error: bytecode stream in file ‘/usr/lib/csv.../ta6le/kernel.o’ generated with LTO version NNN instead of the expected MMM`

   This can be worked around by running `make -j CC="gcc -fno-lto"`

If get some other error and you correctly installed the required dependencies,
feel free to open a [GitHub issue](https://github.com/cosmos72/schemesh/issues)
describing the issue. Remember to include at least:
* the exact `make ...` command you executed, and its output - especially the error messages
* the output of the command `./utils/show_system_info.sh`

## RECENT CHANGES

See [doc/recent_changes.md](doc/recent_changes.md)

## Documentation

Shell builtins can be listed by typing `help` at the prompt,
and help for each builtin can be viewed by typing `help NAME` at the prompt.

The documentation currently include:

* Prompt customization, see [doc/lineedit/ansi.md](doc/lineedit/ansi.md)

* Key bindings customization, see [doc/lineedit/key.md](doc/lineedit/key.md)

* Job environment variables, see [doc/shell/env.md](doc/shell/env.md)

* Job redirection, see [doc/shell/redirect.md](doc/shell/redirect.md)

* Loading schemesh as a library from plain Chez Scheme, see [doc/schemesh_as_library.md](doc/schemesh_as_library.md)



Most other scheme functions are not (yet) documented.

## FREQUENTLY ASKED QUESTIONS

* how does schemesh differ from other Lisp-based shells as
  [Scsh](https://scsh.net/),
  [Rash](http://rash-lang.org/),
  [Eshell](https://www.emacswiki.org/emacs/CategoryEshell#Eshell),
  or (pick favorite shell) ?

  In extreme summary:
  none of them have job control, i.e. the ability to suspend a job and resume it in the background or foreground;
  some also have additional limitations.

  Scsh lacks line editing, autocompletion and shell-like syntax - see [Scsh Reference manual - Caveats](https://scsh.net/docu/html/man-Z-H-2.html#node_sec_1.4)

  Eshell runs *inside* Emacs, so it's difficult to use as a login shell.

  For more details, see [doc/comparison_with_other_shells.md](doc/comparison_with_other_shells.md)


## TO DO

* autocomplete partial usernames after ~
* maybe add missing shell builtins "kill"
* implement function `(string->sh-patterns)`
