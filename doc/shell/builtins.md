# shell builtins

In shell syntax, the following shell builtins are recognized:

### Alphabetical index

* [`.`](#dot) read a file and execute the contained shell script or Scheme source code
* [`:`](#colon) do nothing and return success, ignoring all arguments
* [`alias`](#alias) define or display aliases
* [`answers`](#answers) display as structured data the values returned by recent expressions or commands executed at repl
* [`bg`](bg) resume a job and move a job to the background
* [`builtin`](#builtin) execute a builtin, even if shadowed by an alias
* [`cd`](#cd) change the current directory
* [`cd-`](#cd-) change back to previous current directory
* [`command`](#command) execute an external command, even if shadowed by an alias or by a builtin
* [`dir`](#dir) display specified files and directories as structured data, or current directory by default
* [`echo`](#echo) write space-separated arguments to standard output, terminated by a newline
* [`echo0`](#echo0) write NUL-terminated arguments to standard output
* [`exec`](#exec) replace the current shell or subshell with the specified command
* [`exit`](#exit) exit the current shell or subshell with specified exit status, or 0 by default
* [`export`](#export) show or export environment variables
* [`false`](#false) do nothing and return failure, ignoring all arguments
* [`fg`](#fg) resume a job, move it to the foreground, and wait for it to finish or stop
* [`first`](#first) copy only the first N structured data from stdin to stdout, or 1 datum by default
* [`from`](#from) parse structured data from stdin with specified format, and write such data to stdout
* [`global`](#global) execute a builtin, making it affect global environment variables or global current directory
* [`help`](#help) display help about a builtin, or display all builtins by default
* [`history`](#history) display history
* [`jobs`](#jobs) display jobs and their status as structured data
* [`parent`](#parent) execute a builtin, making it affect granparent job's environment variables or current directory
* [`parse`](#parse) read a file, parse structured data from it, and write such data to stdout
* [`proc`](#proc) display active processes as structured data
* [`pwd`](#pwd) display current directory, or the current directory of specified job
* [`select`](#select) copy only the specified fields of structured data from stdin to stdout
* [`set`](#set) display or set environment variables
* [`skip`](#skip) skip the first N structured data, or 1 datum by default, and copy the following ones from stdin to stdout
* [`sort-by`](#sort-by) read structured data from stdin, sort elements by specified fields, and write them to stdout
* [`source`](#source) read a file and execute the contained shell script or Scheme source code
* [`split-at-0`](#split-at-0) split every argument after each NUL character, and execute the resulting alias, builtin or command
* [`status`](#status) do nothing and return specified status, or success by default
* [`threads`](#threads) display known threads and their status
* [`to`](#to) copy structured data from stdin to stdout, writing it with specified format
* [`true`](#true) do nothing and return success, ignoring all arguments
* [`ulimit`](#ulimit) display or modify shell resource limits
* [`unalias`](#unalias) remove specified aliases
* [`unexport`](#unexport) mark specified environment variables as not exported
* [`unset`](#unset) remove specified environment variables
* [`wait`](#wait) move a job to the foreground and wait for it to finish
* [`where`](#where) copy only structured data matching specified criteria from stdin to stdout

### Classic builtins

##### <span id="dot">.</span>
Syntax: `. FILE`

Same as [source](#source)

##### <span id="colon">:</span>
Syntax: `: [ARGS ...]`

Same as [true](#true)

##### alias
Syntax: `alias [NAME [EXPANSION ...]]`

Define or display aliases.

* without arguments, `alias` writes the list of defined aliases to standard output.
* with a single argument, `alias NAME` writes the definition of alias `NAME` to standard output.
* with two or more arguments, `alias NAME EXPANSION ...` defines an alias `NAME` such that,
  when `NAME ARGS ...` executed, it is substituted with `EXPANSION ... ARGS ...`

Return success, or failure if `alias NAME` is executed and no such alias is defined.

##### bg
Syntax: `bg [JOB_ID]`

Resume specified job, or last job by default, and move it to the background.

Return success, or failure if job is not found.

##### builtin
Syntax: `builtin [BUILTIN_NAME [ARG ...]]`

Execute a builtin with specified arguments, even if it's shadowed by an alias.

Return exit status of executed builtin, or failure if no such builtin was found.

##### cd
Syntax: `cd [DIR]`

Change the current directory of parent job.

* without arguments, `cd` sets the current directory of parent job
  to the value of its `HOME` environment variable.
* with one argument, `cd DIR` sets the current directory of parent job to `DIR`.

Return success if the directory is successfully changed, otherwise raise an exception.

Note: merely returning failure was considered too lenient, because executing commands in the wrong directory
can easily have unintended effects, such as removing the wrong files or directories.

##### cd-
Syntax: `cd-`

Change the current directory of parent job, setting it to previous current directory.

Return success if the directory is successfully changed, otherwise raise an exception.

##### command
Syntax: `command [COMMAND_NAME [ARG ...]]`

Execute a command with specified arguments, even if it's shadowed by an alias or by a builtin.

Return exit status of executed command, or failure if no such command was found.

##### echo
Syntax: `echo [ARG ...]`

Display arguments to standard output, separating them with a single space, and terminating them with a single newline.

Return success.

##### echo0
Syntax: `echo0 [ARG ...]`

Display arguments to standard output, separating them with a single NUL, and terminating them with a single NUL.

Return success.




### Structured Pipelines

##### answers
Syntax: `answers [--to-FORMAT]`

Display as structured data the values returned by recent expressions or commands executed at repl.
Options:
* `--to-FORMAT` display answers using specified `FORMAT` instead of autodetecting it

Return success, or failure if `FORMAT` is not supported.

##### dir
Syntax: `dir [OPTIONS] [PATH ...]`

Display specified files and directories as structured data, or current directory by default.
Options:
* `-a` also display entries starting with `.`
* `-d` display directories themselves, not their contents
* `-l` display more details for each entry
* `-v` display even more details for each entry
* `--to-FORMAT` display entries in given `FORMAT` instead of autodetecting it

Return success, or failure some `PATH` could not be displayed or if `FORMAT` is not supported.
