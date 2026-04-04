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

#### <span id="dot">.</span>
Syntax: `. FILE`

Same as [source](#source)

Read a file and execute the contained shell script or Scheme source code.

Return exit status of last executed command, or value of last evaluated expression.

#### <span id="colon">:</span>
Syntax: `: [ARGS ...]`

Same as [true](#true)

Do nothing and return success. Ignores all arguments.

#### alias
Syntax: `alias [NAME [EXPANSION ...]]`

Define or display aliases.

* without arguments, `alias` writes the list of defined aliases to standard output.
* with a single argument, `alias NAME` writes the definition of alias `NAME` to standard output.
* with two or more arguments, `alias NAME EXPANSION ...` defines an alias `NAME` such that,
  when `NAME ARGS ...` executed, it is substituted with `EXPANSION ... ARGS ...`

Return success, or failure if `alias NAME` is executed and no such alias is defined.

#### bg
Syntax: `bg [JOB_ID]`

Resume specified job, or last job by default, and move it to the background.

Return success, or failure if job is not found.

#### builtin
Syntax: `builtin [BUILTIN_NAME [ARG ...]]`

Execute a builtin with specified arguments, even if it's shadowed by an alias.

Return exit status of executed builtin, or failure if no such builtin was found.

#### cd
Syntax: `cd [DIR]`

Change the current directory of parent job.

* without arguments, `cd` sets the current directory of parent job
  to the value of its `HOME` environment variable.
* with one argument, `cd DIR` sets the current directory of parent job to `DIR`.

Return success if the directory is successfully changed, otherwise raise an exception.

Note: merely returning failure was considered too lenient, because executing commands in the wrong directory
can easily have unintended effects, such as removing the wrong files or directories.

#### cd-
Syntax: `cd-`

Change the current directory of parent job, setting it to previous current directory.

Return success if the directory is successfully changed, otherwise raise an exception.

As for `cd` builtin, merely returning failure was considered too lenient, because executing commands in the wrong directory
can easily have unintended effects, such as removing the wrong files or directories.

#### command
Syntax: `command [COMMAND_NAME [ARG ...]]`

Execute a command with specified arguments, even if it's shadowed by an alias or by a builtin.

Return exit status of executed command, or failure if no such command was found.

#### echo
Syntax: `echo [ARG ...]`

Display arguments to standard output, separating them with a single space, and terminating them with a single newline.

Return success.

#### echo0
Syntax: `echo0 [ARG ...]`

Display arguments to standard output, separating them with a single NUL, and terminating them with a single NUL.

Return success.

#### exec
Syntax: `exec [CMD [ARG ...]]`

Replace the current shell with the command `CMD ARG ...`

If `CMD ARG ...` are specified, on success does not return. On failure, returns failure error code.

If `CMD ARG ...` are not specified, applies any following redirection the current shell,
then return success if the redirections are successful, otherwise return failure.

#### exit
Syntax: `exit [INT]`

Exit the shell with exit status `INT`, or 0 by default.

Does not return.

#### export
Syntax: `export [VAR ...]`

Display or export environment variables.

* without arguments, `export` displays all exported variables of parent job.
* with one or more arguments, `export VAR ...` marks specified environment variables
  as exported in parent job.

#### false
Syntax: `false [ARG ...]`

Do nothing and return failure. Ignores all arguments.

#### fg
Syntax: `fg [JOB_ID]`

Resume a job, move it to the foreground, and wait for it to finish or stop.
If `JOB_ID` is not specified, it defaults to last job.

Return job exit status, or success if job is stopped, or failure if job was not found.

#### global
Syntax: `global [BUILTIN_NAME [ARGS ...]]`

Execute a builtin, setting its parent job to the shell itself.<br/>
This means the executed builtin can affect global environment variables and global current directory.

Return exit status of executed builtin, or failure if no such builtin was found.

#### help
Syntax: `help [BUILTIN_NAME]`

Display help about a builtin, or display all builtins.

* without arguments, `help` displays all builtins.
* with one argument, `help BUILTIN_NAME` displays help about the specified builtin.

Return success, unless a builtin name is specified but not found.

#### history
Syntax: `history [ARGS ...]`

Display history to standard output. Ignores all arguments.

Return success.

#### parent
Syntax: `parent [BUILTIN_NAME [ARGS ...]]`

Execute a builtin, setting its parent job to the parent's parent.<br/>
This means the executed builtin can affect grandparent's environment variables and grandparent's current directory.

Return exit status of executed builtin, or failure if no such builtin was found.

#### pwd
Syntax: `pwd [JOB_ID]`

Display the current directory of specified job, or current directory by default.

Return success, unless a job is specified but not found.


#### set
Syntax: `set [VAR [VALUE]]`

Display or set environment variables.

* without arguments, `set` displays all exported and private environment variables of parent job
* with one argument, `set VAR` displays specified environment variable of parent job
* with two arguments, `set VAR VALUE` sets specified environment variable of parent job

Return success, unless `set VAR` is executed and no such variable is found.

#### source
Syntax: `source FILE`

Same as [.](#dot)

Read a file and execute the contained shell script or Scheme source code.

Return exit status of last executed command, or value of last evaluated expression.

#### split-at-0
Syntax: `split-at-0 ALIAS_OR_BUILTIN_OR_COMMAND [ARG ...]`

Split every `ARG` after each NUL character, and execute the resulting alias, builtin or command
with arguments set to the result of such splitting.

Useful to pass as arguments the NUL-terminated filenames produced by another command,
as for example `split-at-0 editor $(find -name \*.txt -print0)`

Return exit status of executed alias, builtin or command,
or failure if no such alias, builtin or command is found.

#### status
Syntax: `status [ARG ...]`

Do nothing and return specified status, or success by default.

Ignores second and further arguments.

#### true
Syntax: `true [ARGS ...]`

Same as [:](#colon)

Do nothing and return success. Ignores all arguments.

#### unalias
Syntax: `unalias [NAME ...]`

Remove specified aliases.

Return success.

#### unexport
Syntax: `unexport [VAR ...]`

Mark specified environment variables as private i.e. *not* exported in parent job.

Return success.

#### unset
Syntax: `unset [VAR ...]`

Remove specified environment variables from parent job.

Return success.

#### wait
Syntax: `wait [JOB_ID]`

move a job to the foreground and wait for it to finish

Resume a job, move it to the foreground, and wait for it to finish.
Does *not* return if job is suspended.
If `JOB_ID` is not specified, it defaults to last job.

Return job exit status, or failure if job was not found.

------------------------------------------------------------------------------------------------------
### Structured Pipelines

#### answers
Syntax: `answers [--to-FORMAT]`
<br/>Added in 1.0.0

Display as structured data the values returned by recent expressions or commands executed at repl.<br/>
Options:
* `--to-FORMAT` display answers using specified `FORMAT` instead of autodetecting it

Return success, or failure if `FORMAT` is not supported.

Example:
```shell
answers
```
possible output:
```
┌───┬─────────────────┐
│idx│     answer      │
├───┼─────────────────┤
│  0│(failed 7)       │
│  1│(stopped sigtstp)│
│  2│1073741824       │
│  3│#(foo bar)       │
└───┴─────────────────┘
```

#### dir
Syntax: `dir [OPTIONS] [PATH ...]`
<br/>Added in 1.0.0

Display specified files and directories as structured data, or current directory by default.<br/>
Options:
* `-a` also display entries starting with `.`
* `-d` display directories themselves, not their contents
* `-l` display more details for each entry
* `-v` display even more details for each entry
* `--to-FORMAT` display entries in given `FORMAT` instead of autodetecting it

Return success, or failure if some `PATH` could not be displayed or if `FORMAT` is not supported.

Example:
```shell
dir -l repl
```
possible output:
```
┌──────────┬────┬─────┬──────────┬──────────┬─────────┬────┬─────┐
│   name   │type│size │ modified │ accessed │  mode   │user│group│
├──────────┼────┼─────┼──────────┼──────────┼─────────┼────┼─────┤
│answers.ss│file│ 2639│2026-03-04│2026-03-04│rw-r--r--│max │users│
│easy.ss   │file│41945│2026-03-14│2026-03-14│rw-r--r--│max │users│
│repl.ss   │file│29965│2026-03-14│2026-03-14│rw-r--r--│max │users│
└──────────┴────┴─────┴──────────┴──────────┴─────────┴────┴─────┘
```

#### first
Syntax: `first [OPTIONS] [COUNT]`
<br/>Added in 1.0.0

Read structured data from standard input,
then write only the first `COUNT` elements to standard output, or 1 element by default.<br/>
Options:
* `--from-FORMAT` read structured data using specified `FORMAT` instead of autodetecting it
* `--to-FORMAT` write structured data using specified `FORMAT` instead of autodetecting it

Return success, or failure if `FORMAT` is not supported.

#### from
Syntax: `from [OPTIONS] FORMAT`
<br/>Added in 1.0.0

Read structured data from standard input using specified `FORMAT`,
then write each element to standard output.<br/>
Options:
* `--to-FORMAT` write structured data using specified `FORMAT` instead of autodetecting it

Arguments:
* `auto` autodetect input format
* `csv`  assume input format is CSV
* `json` assume input format is JSON or NDJSON
* `wire` assume input format is WIRE

Return success, or failure if `FORMAT` is not supported.

#### jobs
Syntax: `jobs [OPTIONS]`
<br/>Added in 1.0.0

Display jobs and their status as structured data.<br/>
Options:
* `--to-FORMAT` write structured data using specified `FORMAT` instead of autodetecting it

Return success, or failure if `FORMAT` is not supported.

Example:
```shell
jobs
```
possible output:
```
┌──┬────┬────┬───────────┬─────────┐
│id│pid │pgid│  status   │ cmdline │
├──┼────┼────┼───────────┼─────────┤
│ 1│1234│1234|(running 1)│{sleep 5}│
└──┴────┴────┴───────────┴─────────┘
```

#### last
Syntax: `last [OPTIONS] [COUNT]`
<br/>Added in 1.0.1

Read structured data from standard input,
then write only the last `COUNT` elements to standard output, or 1 element by default.<br/>
Options:
* `--from-FORMAT` read structured data using specified `FORMAT` instead of autodetecting it
* `--to-FORMAT` write structured data using specified `FORMAT` instead of autodetecting it

Return success, or failure if `FORMAT` is not supported.

#### parse
Syntax: `parse [OPTIONS] FILE`
<br/>Added in 1.0.0

Read specified file, parse structured data from it, then write each element to standard output.<br/>
Options:
* `--from-FORMAT` read structured data using specified `FORMAT` instead of autodetecting it
* `--to-FORMAT` write structured data using specified `FORMAT` instead of autodetecting it

Return success, or failure if `FILE` cannot be read or if `FORMAT` is not supported.

#### proc
Syntax: `proc [OPTIONS] [auvx]`
<br/>Added in 1.0.0

Display active processes as structured data.<br/>
Options:
* `--to-FORMAT` write structured data using specified `FORMAT` instead of autodetecting it

Arguments:
* `a` also display processes started by other users
* `u` display more details for each process
* `v` display even more details for each process
* `x` also display processes running without a terminal

Return success, or failure if `FORMAT` is not supported.

Example:
```shell
proc u
```
possible output:
```
┌────┬────┬─────────┬────────┬─────┬─────┬──────────┬────────┐
│user│pid │user-time│mem-rss │ tty │state│start-time│  name  │
├────┼────┼─────────┼────────┼─────┼─────┼──────────┼────────┤
|root│2586│     0.14│49745920│pts/0│R    │  12:01:02│schemesh│
└────┴────┴─────────┴────────┴─────┴─────┴──────────┴────────┘
```

#### select
Syntax: `select [OPTIONS] FIELD_NAME ...`
<br/>Added in 1.0.0

Read structured data from standard input,
then write only the specified fields of each element to standard output, keeping the order of specified fields.<br/>
Options:
* `--from-FORMAT` read structured data using specified `FORMAT` instead of autodetecting it
* `--to-FORMAT` write structured data using specified `FORMAT` instead of autodetecting it

Return success, or failure if `FORMAT` is not supported.

Example:
```shell
proc v | select name state pid pgid mem-virtual min-fault tty
```
possible output:
```
┌────────┬─────┬─────┬─────┬───────────┬─────────┬─────┐
│  name  │state│ pid │pgid │mem-virtual│min-fault│ tty │
├────────┼─────┼─────┼─────┼───────────┼─────────┼─────┤
│schemesh│S    │ 2586│ 2586│   70946816│    26503│pts/0│
│schemesh│R    │15675│15675│   70946816│      122│pts/0│
└────────┴─────┴─────┴─────┴───────────┴─────────┴─────┘
```

#### skip
Syntax: `skip [OPTIONS] [COUNT]`
<br/>Added in 1.0.0

Read structured data from standard input,
skip the first `COUNT` elements (or 1 element by default),
and write the remaining elements to standard output.<br/>
Options:
* `--from-FORMAT` read structured data using specified `FORMAT` instead of autodetecting it
* `--to-FORMAT` write structured data using specified `FORMAT` instead of autodetecting it

Return success, or failure if `FORMAT` is not supported.

Example:
```shell
dir repl | skip 1
```
possible output:
```
┌───────┬────┬─────┬──────────┐
│ name  │type│size │ modified │
├───────┼────┼─────┼──────────┤
│easy.ss│file│41945│2026-03-14│
│repl.ss│file│29965│2026-03-14│
└───────┴────┴─────┴──────────┘
```

#### sort-by
Syntax: `sort-by [OPTIONS] FIELD_NAME ...`
<br/>Added in 1.0.0

Read structured data from standard input,
then sort all elements by specified fields,
and write sorted elements to standard output.<br/>
Options:
* `-r` sort in reverse order
* `--from-FORMAT` read structured data using specified `FORMAT` instead of autodetecting it
* `--to-FORMAT` write structured data using specified `FORMAT` instead of autodetecting it

Return success, or failure if `FORMAT` is not supported.

Example:
```shell
dir repl -l | sort-by -r size
```
possible output:
```
┌──────────┬────┬─────┬──────────┬──────────┬─────────┬────┬─────┐
│   name   │type│size │ modified │ accessed │  mode   │user│group│
├──────────┼────┼─────┼──────────┼──────────┼─────────┼────┼─────┤
│easy.ss   │file│41945│2026-03-14│2026-03-14│rw-r--r--│max │users│
│repl.ss   │file│29965│2026-03-14│2026-03-14│rw-r--r--│max │users│
│answers.ss│file│ 2639│2026-03-04│2026-03-04│rw-r--r--│max │users│
└──────────┴────┴─────┴──────────┴──────────┴─────────┴────┴─────┘
```

#### threads
Syntax: `threads [OPTIONS]`
<br/>Changed in 1.0.0

Display known threads and their status as structured data.<br/>
Options:
* `--to-FORMAT` write structured data using specified `FORMAT` instead of autodetecting it

Return success.

Example:
```shell
threads
```
possible output:
```
┌──┬───────────┬──────┐
│id│  status   │ name │
├──┼───────────┼──────┤
│ 0│(running)  │      │
│ 1│(failed 11)│worker│
└──┴───────────┴──────┘
```

#### to
Syntax: `to [OPTIONS] FORMAT`
<br/>Added in 1.0.0

Read structured data from standard input,
and write each element to standard output using specified `FORMAT`.<br/>
Options:
* `--from-FORMAT` read structured data using specified `FORMAT` instead of autodetecting it

Arguments:
* `auto` choose output format depending on standard output: terminal => ascii-art table, socket => WIRE, else => NDJSON
* `csv` write data using CSV format
* `json` write data using NDJSON format
* `json1` write data using JSON format
* `table` write data as ascii-art table
* `wire` write data using WIRE format

Return success, or failure if `FORMAT` is not supported.

Example:
```shell
proc | to json1
```
possible output:
```
[{"<type>":"process-entry","pid":20253,"tty":"pts/0","start-time":{"<type>":"time-utc","value":1773602938.959103923},"name":"schemesh"},
{"<type>":"process-entry","pid":101362,"tty":"pts/0","start-time":{"<type>":"time-utc","value":1773605353.809103923},"name":"schemesh"}]
```

Example:
```shell
ip -j route | to table
```
possible output:
```
┌──────────────┬───────────┬────┬───────────────┬────────┬─────┬───────────┐
│     dst      │  gateway  │dev │     flags     │protocol│scope│  prefsrc  │
├──────────────┼───────────┼────┼───────────────┼────────┼─────┼───────────┤
│default       │192.168.0.1│eth0│(span "onlink")│        │     │           │
│192.168.0.0/24│           │eth0│(span)         │kernel  │link │192.168.0.2│
└──────────────┴───────────┴────┴───────────────┴────────┴─────┴───────────┘
```

#### ulimit
Syntax: `ulimit [OPTION ...]`
<br/>Changed in 1.0.0

Display shell resource limits as structured data, possibly after modifying them.<br/>
Options:
* `--to-FORMAT` write structured data using specified `FORMAT` instead of autodetecting it
* `-H` set the `hard' resource limit
* `-S` set the `soft' resource limit (default)
* `-a` display all resource limits
* `-c [LIMIT]` display or set the maximum size of core files created
* `-d [LIMIT]` display or set the maximum size of a process's data segment
* `-e [LIMIT]` display or set the maximum scheduling priority (`nice')
* `-f [LIMIT]` display or set the maximum size of files written by the shell and its children
* `-i [LIMIT]` display or set the maximum number of pending signals
* `-k [LIMIT]` UNIMPLEMENTED. display or set the maximum number of kqueues allocated for this process
* `-l [LIMIT]` display or set the maximum size a process may lock into memory
* `-m [LIMIT]` display or set the maximum resident set size
* `-n [LIMIT]` display or set the maximum number of open file descriptors
* `-p [LIMIT]` display or set the pipe buffer atomic size
* `-q [LIMIT]` display or set the maximum number of bytes in POSIX message queues
* `-r [LIMIT]` display or set the maximum real-time scheduling priority
* `-s [LIMIT]` display or set the maximum stack size
* `-t [LIMIT]` display or set the maximum amount of cpu time in seconds
* `-u [LIMIT]` display or set the maximum number of user processes
* `-v [LIMIT]` display or set the maximum size of virtual memory
* `-x [LIMIT]` display or set the maximum number of file locks
* `-R [LIMIT]` display or set the maximum time a real-time process can run before blocking

If `LIMIT` is specified, the corresponding resource limit is set, otherwise is displayed.
Each `LIMIT` must be an unsigned integer, or the string `unlimited`.

Return success, unless an invalid option is supplied or an error occurs.

Example:
```shell
ulimit -c -d -e -f --to-csv
```
possible output:
```
"<type>","name","soft","hard"
"rlimit","coredump-size",0,"unlimited"
"rlimit","data-size","unlimited","unlimited"
"rlimit","nice",0,0
"rlimit","file-size","unlimited","unlimited"
```

#### where
Syntax: `where [OPTIONS] EXPRESSION`
<br/>Added in 1.0.0

Read structured data from standard input,
and write to standard output only the elements satisfying specified expression.<br/>
Options:
* `--from-FORMAT` read structured data using specified `FORMAT` instead of autodetecting it
* `--to-FORMAT` write structured data using specified `FORMAT` instead of autodetecting it

Argument `EXPRESSION` must be one of:
* `EXPRESSION -or EXPRESSION` accept elements satisfying either expression
* `EXPRESSION -and EXPRESSION` accept elements satisfying both expressions
* `-not EXPRESSION` accept elements *not* satisfying expression
* `FIELD_NAME OPERATOR VALUE` accept elements whose field `FIELD_NAME` satisfies expression
* `(` `EXPRESSION` `)` accept elements satisfying expression

Supported `OPERATOR`s are:
`-lt` `-le` `-gt` `-ge` `-eq` `-ne` `-contains` `-starts` `-ends`

As in most languages, `-or` has lower precedence than `-and`, which has lower precedence than `-not`.
To alter their precedence, use parentheses (which must be escaped, because they are special characters in shell syntax).

Also, operators `-or` and `-and` implement short-circuit logic: the right expression is evaluated only if needed.

Example:
```shell
dir -l posix | where name -ends .h -and -not size -ge 10000
```
possible output:
```
┌────────┬────┬────┬──────────┬──────────┬─────────┬────┬─────┐
│  name  │type│size│ modified │ accessed │  mode   │user│group│
├────────┼────┼────┼──────────┼──────────┼─────────┼────┼─────┤
│posix.h │file│1506│2026-01-15│2026-02-05│rw-r--r--│max │users│
│socket.h│file│6413│2026-01-15│2026-02-05│rw-r--r--│max │users│
│tty.h   │file│5361│2026-03-02│2026-03-02│rw-r--r--│max │users│
└────────┴────┴────┴──────────┴──────────┴─────────┴────┴─────┘
```
