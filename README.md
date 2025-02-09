# schemesh
## Fusion between a Unix shell and a Lisp REPL

Schemesh is an interactive shell scriptable in Lisp.

It is primarily intended as a user-friendly Unix login shell, replacing bash, zsh, pdksh etc.

As such, it supports interactive line editing, autocompletion, history and the familiar Unix shell syntax:
it can start commands, including redirections, pipelines, job concatenation with `;` `&&` `||`,
groups surrounded by `{ }`, subshells surrounded by `[ ]`, and manage foreground/background jobs.

For more complex tasks, it seamlessly integrates a full Lisp REPL backed by Chez Scheme.

Schemesh can be used as:
* a replacement for traditional interactive Unix shell, as for example bash/zsh/pdksh etc.

* a Unix shell scriptable in Chez Scheme.

* a Scheme REPL with additional syntax and functions to start, redirect and manage Unix processes

* a Scheme library for starting, redirecting and managing Unix processes

For scripting and serious programming, schemesh completely replaces the slow, clumsy and error-prone
scripting language of a traditional shell (yes, the author has opinions) with a full-featured Lisp REPL,
backed by the fast and open-source Chez Scheme compiler that generates highly optimized native code.

### How to use

As a traditional Unix shell: type a command and press Enter.<br/>
As a Lisp REPL: type an expression starting with `(` and press Enter.

If the parentheses/braces/brackets/quotes are balanced,
schemesh will execute the command and show any failure, or evaluate the expression and pretty-print its value.

If the parentheses/braces/brackets/quotes are *not* balanced,
schemesh will create a second line where you can continue typing - you can move between lines with cursor keys.

Switching between shell syntax and Lisp syntax is extremely simple, and can be done basically everywhere:
* open parenthesis `(` temporarily switches to Lisp syntax until the corresponding `)`.

* open brace i.e. `{` temporarily switches to shell syntax until the corresponding `}`.

* open bracket i.e. `[` starts a new sub-form in current syntax until the corresponding `]`.<br/>
  If found in Lisp syntax, it is equivalent to `(`.<br/>
  If found in shell syntax, it is similar to `{` with the difference that commands will be executed in a subshell.

* the directives `#!scheme` `#!chezscheme` and `#!r6rs` temporarily switch to Scheme syntax
  (with the appropriate flavor) until the end of current `( )`, `[ ]` or `{ }`.<br/>
  If entered at top level, they change the default syntax until another directive is entered at top level.

* the directive `#!shell` temporarily switches to shell syntax until the end of current `( )`, `[ ]` or `{ }`.<br/>
  If entered at top level, it changes the default syntax until another directive is entered at top level.

* shell syntax creates first-class Lisp `sh-job` objects, which can be started/stopped/managed from both syntaxes.

* `(sh-job)` objects are discoverable and pretty-printable:<br/>
  `(values '{SOME-SHELL-SYNTAX})` shows how shell syntax is converted to `shell...` macros,<br/>
  `(expand '{SOME-SHELL-SYNTAX})` shows how `shell...` macros are expanded to `sh...` functions for creating jobs,<br/>
  `(values  {SOME-SHELL-SYNTAX})` - *without* quotes - pretty-prints the created `sh-job` objects.


The most common mechanisms to start/stop/manage jobs from shell syntax are:
* CTRL+C      interrupt the current foreground job
* CTRL+Z      suspend the current foreground job
* `bg job-id` resume a job in background
* `fg job-id` resume a job in foreground

The most common mechanisms to start/stop/manage jobs from Lisp syntax are:
* CTRL+C      as above
* CTRL+Z      as above
* `(sh-start job-object)` start a job in background, return immediately
* `(sh-run/i job-object)` start a job in foreground, wait until job finishes or is suspended
* `(sh-run   job-object)` start a job in foreground, wait until job finishes
* `(sh-bg    job-or-id)` resume a job in background, return immediately
* `(sh-fg    job-or-id)` resume a job in foreground, wait until job finishes or is suspended
* `(sh-wait  job-or-id)` resume a job in foreground, wait until job finishes

### Examples

You can mix shell command execution with Lisp control structures, loops and functions as for example:
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

```shell
find (lisp-function-returning-some-string) -type f | grep ^lib | wc -l &
fg
```

```lisp
(define job {ls -l > ls.out || echo "ls failed"})
(sh-start job)
(sh-fg job)
```

### Features
- [x] REPL with multi-line editing and parentheses highlighting
- [x] dual syntax parsing, allows mixing Scheme and shell expressions
- [x] shortcuts to execute commands by pressing a single key: KP- KP+
- [x] history searchable with PageUp and PageDown keys
- [x] cut-and-paste
- [x] context-sensitive autocompletion - some improvements pending
- [x] UTF-8b for losslessly converting byte sequences that are not valid UTF-8
- [x] shell commands, including `&&` `||` `{` `}` `[` `]`
- [x] shell job control
- [x] shell aliases
- [x] shell builtins
- [x] shell environment variables
- [x] shell pipelines `|` `|&`
- [x] shell redirections `<` `>` `<>` `>>` `<&` `>&` `$()` ``` `` ```
- [x] shell wildcard expansion
- [x] if the directory `$HOME/.cache/schemesh/` exists, history is automatically saved to and loaded from a file `history.txt` inside such directory
- [x] if the file `$HOME/.config/schemesh/repl_init.ss` exists, it is automatically executed when starting the REPL
- [x] if the file `$HOME/.config/schemesh/repl_quit.ss` exists, it is automatically executed when exiting the REPL


## Build instructions

On Debian Linux, execute the following commands:
```
sudo apt update
sudo apt install build-essential chezscheme-dev liblz4-dev libncurses-dev git uuid-dev
git clone https://github.com/cosmos72/schemesh
cd schemesh
make -j
sudo make install
```

For other systems, the instructions above can (hopefully) be adapted as needed.

If all went well, you can execute `schemesh`

In case your environment variable `$PATH` does not contain `/usr/local/bin`,
the command `schemesh` will not suffice - you will need to run `/usr/local/bin/schemesh`

## RECENT CHANGES

See [doc/recent_changes.md](doc/recent_changes.md)

## TO DO

* autocomplete shell paths and scheme strings: unescape stems before searching for completions, escape completions
* autocomplete shell paths and scheme strings: when autocompleting inside single or double quotes, the stem starts at the quotes.
* autocomplete shell paths starting with ~
* maybe add missing shell builtins "kill"
* implement function `(string->sh-patterns)`
* improve function `(include/lang)` to save and restore parameters `(optimize-level)` `(debug-level)` etc.
