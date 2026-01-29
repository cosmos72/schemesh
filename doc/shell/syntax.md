# shell job syntax

Ordered from highest priority to lowest priority, job syntax is:

0. subshell: `[shell_commands]` executes commands in a subshell. See below for what `shell_commands` can contain

1. command with arguments: that's fundamental, `cmd_name /some/dir --some-options --other-options` executes the command, if found in `$PATH`

   The command, and also each argument, can contain zero or more single quotes `"..."` and single quotes `'...'`
   that are useful to disable some of the steps below. Example: `ls  path" with "spaces  path'<with>lots$of&special[chars]*'`

2. shell glob and substitution, which are performed together
   i.e. the expansion produced by each one is **not** parsed again for special characters recognized by the other one.

* shell glob: command name and its arguments can contain the special sequences `*` `?` and a list of characters surrounded by `[]`
    which are expanded to match file system contents.
    Shell glob is disabled both inside double quotes `"..."` and inside single quotes `'...'`
    and can also be disabled for a single character by preceding it with backslash `\`

* substitution: command name and its arguments can contain the special sequences:
    `$NAME` which expands to the value of the corresponding environment variable
    `$[shell_commands]` which expands to the output produced by running the subshell commands in `$[...]`
    See below for what `shell_commands` can contain.
    Substitution works normally inside double quotes `"..."` while it is disabled inside single quotes `'...'`
    and can also be disabled for a single character by preceding it with backslash `\`

3. file redirections: `[FD]< FILE`, `[FD]<> FILE`, `[FD]> FILE` and `[FD]>> FILE`
   If `FD` is specified, spaces must **not** be present after it, otherwise it `FD` would be parsed as an argument.

   File redirection is disabled both inside double quotes `"..."` and inside single quotes `'...'`
   and can also be disabled for a single character by preceding it with backslash `\`

4. file descriptor redirections:  `[FD1]<& FD2` and  `[FD1]>& [FD2]`  where FD2 is an unsigned integer, or -1 which causes FD1 to be closed.
   Again, if `FD1` is specified, spaces must **not** be present after it, otherwise it `FD1` would be parsed as an argument.
   Note that command arguments and redirections **can** be interleaved after the command name in **arbitrary** order.
   Bash syntax `>& FILE` that redirects both stdout and stderr is **not** supported, one has to write `> FILE 2>&1`

   File descriptor redirection is disabled both inside double quotes `"..."` and inside single quotes `'...'`
   and can also be disabled for a single character by preceding it with backslash `\`

5. per-command environment variables: `NAME1=VALUE1 [NAME2=VALUE2 ...] CMD_ARGS_AND_REDIRECTIONS`.
   If command is omitted, i.e. only `NAME=VALUE1 [NAME2=VALUE2 ...]` is present, the environment variables are set into the parent job.

6. negation: `! ENVS_CMD_ARGS_AND_REDIRECTIONS`. Multiple negations are allowed, as for example `!!!ENVS_CMD_ARGS_AND_REDIRECTIONS`

7. pipeline: `CMD_NOT1 | CMD_NOT2 ...` or `CMD_NOT1 |& CMD_NOT1 ...`

8. and: `CMD_PIPE1 && CMD_PIPE2 ...` also implements logic short-circuit i.e. exits at the first failure and returns it

9. or: `CMD_AND1 || CMD_AND2 ...` also implements logic short-circuit i.e. exits at the first success and returns it

10. list: `CMD_OR1 ; [CMD_OR2 ...]` executes the commands sequentially.
    Instead `CMD_OR1 & [CMD_OR1 ...]` starts the first command in background,
    then immediately proceeds to the remaining commands, if any.
    Note that newline is treated as `;`

In this context, `shell_commands` is the most general syntax i.e. any of the syntaxes listed above.

Also, each command name, argument, or redirection file name can be replaced with a shell expression `( ... )`
that evaluates to a string, or to a list of strings, or to a closure that accepts zero or one arguments (the job itself)
and returns a string or a list of strings.

Finally, file descriptor numbers can be replaced with a shell expression `( ... )` that evaluates to a fixnum.

-----------------------------------------------------------------------------

Other notable features:

Environment variables and current directory can be different in each job.

When setting an env variable or the current directory, it is stored in a job (by default, the job itself).

Children jobs inherit env variables and current directory from their parent, and can override them.

See also [doc/shell/env.md](env.md) for managing environment variables through scheme functions.

-----------------------------------------------------------------------------

Intentionally missing features:

there are **no** shell builtins `if` `case` `func` `for` `while` that introduce control flow, iteration or functions.

The rationale is: scheme syntax is better suited for such tasks.
