# Shell: macros and functions

In schemesh, the shell syntax is just that: a syntax.
It is parsed to Scheme macros, that expand to Scheme functions, that in turn create Scheme `sh-job` record types.
One could say ["there is no shell"](https://www.reddit.com/r/matrix/comments/y3hprb/there_is_no_spoon/)
as it's Scheme all the way down: the shell is just the default syntax.

Most of the time the shell syntax is enough, and there is no need to look behind the veil.
In the few cases where it's needed, or if you are curious, the following describes the Scheme macros and functions
produced by parsing shell syntax.

### Macros

##### (shell)
`(shell [ARGS])` is the main macro produced by parsing shell syntax `{...}`.

You can see how shell syntax is parsed to `(shell)` macro by evaluating `(begin '{some shell command})`:

* Simple commands `{cmd arg1 arg2 ...}` are parsed to `(shell "cmd" "arg1" "arg2" ...)`

* File redirections `{cmd [n1]<path1 [n2]>path2 [n3]<>path3 [n4]>>path4}`
  are parsed to `(shell "cmd" "arg" [n1] < "path1" [n2] > "path2" [n3] <> "path3" [n4] >> "path4")`
  where `[n1] ... [n4]` are optional file descriptor numbers.
  Shell syntax does **not** allow spaces between a file descriptor number and its following redirection operator.

* Fd redirections `{cmd [n1]<&m1 [n2]>m2}` are parsed to `(shell "cmd" "arg" [n1] <& m1 [n2] >& m2)`
  where `[n1]` and `[n2]` are optional "from" file descriptor numbers,
  and `m1` and `m2` are either "to" file descriptor numbers or `-` to close the redirection.
  Again, shell syntax does **not** allow spaces between a file descriptor number and its following redirection operator.

Note that arguments, file redirections and fd redirections **can** be interleaved.

* Wildcards are special characters in simple command arguments (either unquoted or double quoted)
  that contain one or more special characters `*` `?` `[...]` or `~`.<br/>
  The syntax `[...]` has an ambiguity: it may indicate either a subshell or a wildcard.<br/>
  At the beginning of a simple command, it is parsed as a subshell.<br/>
  In all other cases, `[...]` is parsed as a wildcard: if you need it at the beginning of a simple command, prefix it with `""`.
  
  Wildcards `{cmd ~user/*foo?[a-z]bar[!0-9]}` are parsed to `(shell cmd (shell-wildcard ~ "user/" * "foo" ? % "a-z" "bar" %! "0-9"))`.
  In detail:
  - `~` can only appear at the beginning of an argument. It expands to the home directory of specified user (by default, current user)
  - `*` matches any string, and is parsed to the symbol `*`
  - `?` matches any character, and is parsed to the symbol `?`
  - `[...]` matches any character present in brackets, and is parsed to the symbol `%` followed by the characters in brackets
  - `[...]` matches any character **not** present in brackets, and is parsed to the symbol `%!` followed by the characters in brackets

* Substitutions `$[subcommand args]` and `` `subcommand args` `` can appear anywhere in simple command arguments
  (either unquoted or double quoted) and indicate that specified subcommand must be executed,
  and its output must be inserted as a **single** string where the substitution appears.<br/>
  Note: traditional shells also split the output of subcommand to multiple strings, unless it's in double quotes.
  
  Both substitution syntaxes `{cmd $[subcommand arg]}` and ``{cmd `subcommand arg`}``
  are parsed to `(shell "cmd" (shell-backquote "subcommand" "arg"))`
  and can contain **any** shell syntax, not only simple commands.

* Negation `{! cmd args}` is parsed to `(shell ! "cmd" "args")`. Multiple consecutive negations are allowed.

* Pipelines `{cmd1 arg1 | cmd2 arg2 |& cmd3 arg3}` are parsed to
  `(shell "cmd1" "arg1" \x7C; "cmd2" "arg2" \x7C;& "cmd3" "arg3")` where `\x7C;` indicates the symbol `|`
  because the latter has a special meaning in Chez Scheme.

* And `{cmd1 arg1 && cmd2 arg2}` is parsed to `(shell "cmd1" "arg1" && "cmd2" "arg2")`

* Or `{cmd1 arg1 || cmd2 arg2}` is parsed to `(shell "cmd1" "arg1" \x7C;\x7C; "cmd2" "arg2")`
  Again, `\x7C;` indicates the symbol `|`

* List `{cmd1 arg1 ; cmd2 arg2 &}` is parsed to `(shell "cmd1" "arg1" \x3B; "cmd2" "arg2" &)`
  Note that both `;` and `&` are command **terminators**.
  Also, `\x3B;` indicates the symbol `;` because the latter has a special meaning in Scheme: it starts a comment.

Negation, pipelines, and, or, list are in order of decreasing precedence, as described in [doc/shell/syntax.md](syntax.md).

##### (shell-subshell)
`(shell-subshell [ARGS])` is the macro produced by parsing shell syntax `[...]`

It can contain **any** shell syntax as the `(shell)` macro does: simple commands, file redirections, fd redirections,
wildcards, substitutions, negation, pipelines, and, or.
