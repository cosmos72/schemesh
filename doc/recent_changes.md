## RECENT CHANGES

### release v0.7.6, 2025-03-01

* add new syntax for shell command substitution `$[ ]`  and deprecate the old syntax `$( )`
* add string-related functions (string-any) (string-contains) (string-count) (string-every)
  (string-index) (string-index-right) (string-join) (string-map) (string-replace-all)
  (string-replace-prefix) (string-replace-suffix) (string-prefix?) (string-suffix?)
* export string-related functions (in-string) (string-fill-range!) (string-is-unsigned-base10-integer?)
  (string-is-signed-base10-integer?) (string-iterate) (string-list?) (string-list-split-after-nuls)
  (string-prefix/char?) (string-range-count=) (string-range=?) (string-range<?) (string-replace/char!)
  (string-rtrim-newlines!) (string-split) (string-split-after-nuls) (string-suffix/char?) (string-trim-split-at-blanks)
* automatically (import (schemesh)) at REPL
* rename existing functions to follow r7rs naming conventions
* fix (parse-shell-forms) to parse to nothing backslash-newline outside quotes
* fix (parse-lisp-forms) to correctly parse backslash-whitespace-newline-whitespace inside double quotes
* fix C function c_tty_setraw() to retrieve the current tty configuration at every call,
  because one of the executed commands may have changed it: we want to preserve such changes for future commands
* fix bugs in (on-list) and (in-fixnum-range)
* add functions (directory-list) (directory-list-type) (file-delete) (in-exact-range)
  (mkdir) (ok?) (string-replace-prefix) (string-replace-suffix)
* refactor function (in-range) to call one of (in-fixnum-range) (in-exact-range) (in-flonum-range)
* rename shell builtin "test" -> "expr" and rename macro (shell-test) -> (shell-expr)
* implement builtin "cd-" changes current directory of *parent* job to its previous value.
* implement builtin "parent" executes another builtin with its parent job temporarily set to its grandparent job.
  if used multiple times, as for example "parent parent cd ..", the effects are cumulative.

### release v0.7.5, 2025-02-14

* fix `utils/find_chez_scheme_dir.sh` for Alpine linux
* fix a pair of bugged assertions that were triggered by valid code {echo $A=}
* fix support for Chez Scheme down to 9.5.8
* rename shell builtin "error" -> "test"
* add command line option `--load-file FILE` and support for loading compiled Scheme libraries *.so
* add functions (alist->eq-hashtable) (alist->eqv-hashtable) (alist->hashtable) (file-rename)
  (in-bytevector) (in-bytespan) (in-chargbuffer) (in-charline) (in-charlines) (in-charspan)
  (in-gbuffer) (in-hashtable) (in-list) (in-span) (in-string) (in-vector)
  (in-fixnum-range) (in-flonum-range) (in-range) (on-list)
  (sh-bool) (sh-run/string-split-after-nuls) (sh-version) (string-replace)
* add macros (for) (for*) (library-reexport) (shell-test)
* add Fedora Linux build instructions to README.md
* add "Troubleshooting" section to README.md
* add more examples to README.md

### release v0.7.4, 2025-02-10

* update Makefile to follow GNU conventions for installation directories,
  installation programs and Makefile variable names
* add shell builtins "help copyright" and "help warranty"
* improve shell builtin "fg" to show job status if it's not finished yet when "fg" exits
* when entering and leaving (lineedit-read), sort by id then by pid the queued job status change notifications
* mention (sh-run/string) and (sh-start/fd-stdout) in README.md

### release v0.7.3, 2025-02-09

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
* implement builtin "global", for running another builtin with its parent job temporarily set to (sh-globals)
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
* consume received signals, i.e. (repl-lineedit) calls (sh-consume-sigchld),
  which calls C waitpid(-1, WNOHANG) for any child process, updates (sh-pid-table)
  and calls (sh-job-status) on all parents of each job that changes status.
