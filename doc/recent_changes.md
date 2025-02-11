## RECENT CHANGES

### up to 2025-02-09

* at startup, (sh-eval-file) initialization file ~/.config/schemesh/repl_init.ss if it exists
* at exit, (sh-eval-file) shutdown file ~/.config/schemesh/repl_quit.ss if it exists
* at startup, load history from ~/.cache/schemesh/history.txt
* at exit, save history to the same file
* implement (include/lang) and (include/lang*)
* implement pipeline operator |&
* implement shell builtins: bg fg exec exit export unexport global help set unset source
* extend (sh-cmd* "ENV_VAR" '= "VALUE") to set environment variables in *parent* job
* modify builtin "cd" to change current directory of *parent* job
* modify builtin "pwd" to print current directory of *parent* job
* implement builtin "global", for running another builtin with its parent job set to (sh-globals)
* implement shell builtin "unsafe", for creating (sh-cmd*) commands whose first argument - the program name -
  is not a string but a closure, as for example the output of a subshell, a wildcard etc.
* extend builtin "alias", without arguments now lists existing aliases
* mark and hide temporary redirections created by (sh-pipe) and (sh-pipe*)
* improve (shell-backquote), now expands to a closure that accepts a job
  and calls (sh-run/string-rtrim-newlines) on a new job with the same parent as the job argument
* fix (sh-read...) exception while parsing "#!/some/absolute/path" at the beginning of input
* fix hang in {history | foo} due to builtins being fully executed when they start:
  pipe fd becomes full and blocks further writes, preventing builtin "history" from finishing
  and causing a deadlock: "foo" is never started.
  The solution was: modify (sh-pipe) to always start builtins and multijobs in a subprocess
* consume received signals, i.e. (sh-repl-lineedit) calls (sh-consume-sigchld),
  which calls C waitpid(-1, WNOHANG) for any child process, updates (sh-pid-table)
  and calls (sh-job-status) on all parents of each job that changes status.
