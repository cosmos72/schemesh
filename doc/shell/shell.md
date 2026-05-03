# Shell: macros and functions

["There is no shell"](https://www.reddit.com/r/matrix/comments/y3hprb/there_is_no_spoon/)

In schemesh, the shell syntax is just that: a syntax.
It is parsed to Scheme macros, that expand to Scheme functions, that in turn create Scheme `sh-job` record types.

Most of the time the shell syntax is enough, and there is no need to look behind the veil.
In the few cases where it's needed, or if you are curious, the following describes the Scheme macros and functions
produced by parsing shell syntax.

### Macros

##### (shell)
`(shell [ARGS])` is the main macro produced by parsing shell syntax.

You can see how shell syntax is parsed to `(shell)` macro by evaluating `(begin '{some shell command})`:

Simple commands `{cmd arg1 arg2 ...}` are parsed to `(shell "cmd" "arg1" "arg2" ...)`

File redirections `{cmd [n1]<path1 [n2]>path2 [n3]<>path3 [n4]>>path4}`
are parsed to `(shell "cmd" "arg" [n1] < "path1" [n2] > "path2" [n3] <> "path3" [n4] >> "path4")`
where `[n1] ... [n4]` are optional file descriptor numbers.
Shell syntax does **not** allow spaces between a file descriptor number and its following redirection operator.

Fd redirections `{cmd [n1]<&m1 [n2]>m2}` are parsed to `(shell "cmd" "arg" [n1] <& m1 [n2] >& m2)`
where `[n1]` and `[n2]` are optional "from" file descriptor numbers,
and `m1` and `m2` are either "to" file descriptor numbers or `-` to close the redirection.
Again, shell syntax does **not** allow spaces between a file descriptor number and its following redirection operator.

Note that arguments, file redirections and fd redirections **can** be interleaved.



