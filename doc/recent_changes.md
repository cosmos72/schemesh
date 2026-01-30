## RECENT CHANGES

### TO DO

* extend `reflect` library with new functions `(equiv?)` `(less?)` `(less-equiv?)` `(greater?)` `(greater-equiv?)` `(unordered?)`
  that work on as many comparable types as possible: at least booleans, characters, numbers, strings, symbols, times, dates
* extend library `(scheme2k io wire)` to serialize/deserialize stencil vectors
* add functions `(wire-get)` and `(wire-put)` that read/write from binary input ports
* remove function `(parsectx-unread-char)` and update functions that use it

* investigate more serialization formats: CBOR, MessagePack, possible ZeroMQ-compatible framing

### git main branch

* add type `ordered-hash` a hashtable that preserves insertion order, and functions to use it
* add type `json-reader` and its functions `(json-read-token)` `(json-read-value)` `(json-skip-token)` `(json-skip-value)`
  `(make-json-reader)` as well as standalone functions `(json-write-token)` `(json-write-value)` in new library `(scheme2k io json)`
* add functions `(field)` `(field-names)` in new library `(scheme2k reflect)`
* pretty-print types `time` and `date` and add convenience functions to create them
* rename library `(scheme2k wire)` -> `(scheme2k io wire)` and extend it serialize/deserialize `date` objects and `ordered-hash` objects
* rename functions `(wire-get)` -> `(wire-get-from-bytevector)` and `(wire-put)` -> `(wire-put-to-bytespan)`
* add function `(wire-get-from-bytespan)`

### release v0.9.3, 2025-12-26

* change license GPLv2+ -> LGPLv2+ in all sources except `parser/*` `shell/*` `repl/*` `main.c` `test/*`
* rename all LGPLv2+ C functions and LGPLv2+ Scheme libraries `schemesh*` -> `scheme2k*`
* add shell builtin `ulimit`
* add key bindings for more xterm keys
* add macro `(for-glob)`
* rename function `(channel-fd)` -> `(channel)` and extend it to also accept binary ports in alternative to file descriptors
* rename function `(fd-setnonblock)` -> `(fd-nonblock?-set!)`
* rename function `(in-shell-glob)` -> `(in-glob)`
* rename function `(open-pipe-fds)` -> `(pipe-fds)`
* rename function `(open-socketpair-fds)` -> `(socketpair-fds)`
* rename function `(wildcard)` -> `(wildcard1+)` and add function `(wildcard)`
* add functions `(fd-nonblock?)` `(linectx-key-to-history)` `(socket-fd)`
* improve documentation

### release v0.9.2, 2025-10-18

* fix issue #30: bug in `(string-join)`
* fix wildcards `*FOO` to also match directories whose name ends with `FOO`, instead of only matching non-directories
* fix an off-by-one error in wildcard expansion
* implement autocomplete for shell paths starting with `~` - supersedes pull request #29
* modify functions `(file->port)` and `(sh-port)` to return textual ports by default
* improve `(sh-eval)` to better handle forms containing `(begin ...)` followed by alternating functions and macros
* add support for keywords `while` and `until` inside macros `(for)` `(for*)` `(for-...)` `(forever)` `(repeat)` `(until)` `(while)`
* add more SRFI 18 compatible functions
* add key bindings for more xterm keys
* allow calling `(sh-redirect)` on running jobs, and document its behavior in such case
* subsume functions `(sh-binary-port)` and `(sh-textual-port)` into the new function `(sh-port)`
  for accessing redirections of Scheme jobs
* export more `linectx` functions
* improve documentation, add files [lineedit/ansi.md](lineedit/ansi.md) [lineedit/key.md](lineedit/key.md) [lineedit/linectx.md](lineedit/linectx.md) [shell/env.md](shell/env.md) [shell/redirect.md](shell/redirect.md)

### release v0.9.1, 2025-05-09

* fix saving history to `~/.cache/schemesh/history.txt`
* fix issue #26: flush all open C `FILE*` streams before `fork()`ing a subshell
* add shell builtin `threads`
* add functions `(sh-start/fds)` `(sh-start/ports)`
* add functions `(get-thread)` `(thread)` `(thread-alive?)` `(thread-count)` `(thread-find)`
  `(thread-id)` `(thread-initial-bindings)` `(thread-kill)` `(threads)` `(threads-status)`
* add macro `(for-alist)` and function `(in-alist)`


### release v0.9.0, 2025-05-02

* fix job control bugs on macOS, fixes issue #17
* fix bugs in replacement functions `(get-bytevector-some) (putenv) (bytevector-sint-ref)`
* fix signal handlers to correctly react to signals received by secondary threads
* display colored job exit status
* add build instructions for Windows using WSL and for NixOS
* add per-job ports, needed for buffering input/output of redirected Scheme jobs
* add macro `(==>)`
* add functions `(sh-binary-port) (sh-textual-port)` for accessing redirections of Scheme jobs
* add functions `(fork-process) (repl-answers) (repl-answers-display) (repl-history) (repl-history-display)`
* add functions
  `(ansi-text) (ansi-text?) (ansi-text-bytes) (ansi-text-clear!) (ansi-text-visible-length) (make-ansi-text) (string+) (color)`
  `(black) (black+) (red) (red+) (green) (green+) (yellow) (yellow+) (blue) (blue+) (magenta) (magenta+) (cyan) (cyan+) (white) (white+)`
  for creating customized, colored prompt.
* add Racket-compatible functions
  `(port->list) (port->string) (port->bytes) (port->lines) (port->bytes-lines)`
  `(read-line) (read-bytes-line) (lines->port) (byte-lines->port)`
* add functions `(make-consumer) (consumer?) (consumer-get) (consumer-eof?) (consumer-timed-get) (consumer-try-get)`
* improve shell builtins `fg` `bg` and `wait`: job-id is now optional
* delete shell builtin `unsafe`, fixes issue #23
* refactor type `vscreen` to contain colored chars: will allow implementing syntax highlighting
* document prompt customization
* improve autocompletion inside single or double quotes
* rename several undocumented functions
* replace `(console-input-port)` `(console-output-port)` and `(console-error-port)`
  with interruptible, unbuffered UTF-8b ports
* sanity: do not allow closing standard ports
  `(console-input-port) (console-output-port) (console-error-port)`
  `(current-input-port) (current-output-port) (current-error-port)`


### release v0.8.3, 2025-04-06

* fix a visual glitch when pasting multiple lines at REPL, and last line does not end with a #\newline
* remove Apache-licensed file containers/hashtable-types.ss that was copy-pasted from Chez Scheme sources.
  Replace it with accessor functions obtained from record-type-descriptor objects.
* define optimized versions of standard - but currently slow - functions
  `(bytevector-sint-ref) (bytevector-sint-set!) (bytevector-uint-ref)(bytevector-uint-set!)`
* add functions `(channel?) (channel-fd) (channel-pipe-pair) (channel-socket-pair)`
  `(channel-close) (channel-get) (channel-eof?) (channel-put) (in-channel)`
  for exchanging data between different processes through a socket, pipe or analogous file descriptor.
  They internally serialize and deserialize data using library `(schemesh wire)`
* improve `(sh-run/string)` and `(sh-run/bytevector)` behavior if job being executed receives a signal
* add function `(wire-register-rtd-fields)` to quickly create serializer and deserializer
  for user-defined record types.
* add support for serializing and deserializing Chez Scheme `time` object to library `(schemesh wire)`

### release v0.8.2, 2025-03-29

* fix issue #12: bug in shell syntax when parsing assignment after backslash+newline
* fix macro `(in-hash-pairs)`
* implement shell builtin `wait`
* add standalone executable `countdown` as an alternative to `sleep`
* add macros `(shell-glob)` and `(shell-string)` for expanding shell syntax to, respectively,
  a list of strings or a single string.
* add macro `(in-shell-glob)` as a shortcut for `(in-list (shell-glob))`
* add functions `(bytespan->bytevector0) (countdown) (chargbuffer->charspans*) (chargbuffer->string)`
  `(charlines->charspan) (charlines->string) (fd-seek) (socketpair-fds)`
  and macro `(with-raw-tty)`
* rename function `(sh-version)` -> `(sh-version-number)`
  and add function `(sh-version)` that returns a string
* add functions `(getenv) (putenv)`
  `(get-bytevector-all) (get-bytevector-n) (get-bytevector-some)`
  `(get-char) (get-datum) (get-line) (get-string-all) (get-string-n) (get-u8)`
  `(put-bytevector) (put-bytevector-some)`
  `(put-char) (put-datum) (put-string) (put-string-some) (put-u8)`
  they are exported only by libraries `(schemesh)` or `(schemesh posix replacements)`
  because they intentionally conflict with with R6RS and Chez Scheme functions with the same names:
  they are intended as replacements.
  They are equivalent to their R6RS or Chez Scheme counterparts,
  with the addition that textual-port is optional and defaults respectively
  to `(current-input-port)` for `(get-...)` functions
  and to `(current-output-port)` for `(put-...)` functions
* add functions `(datum->wire) (wire->datum) (wire-get) (wire-length) (wire-put)`
  for serializing/deserializing arbitrary data from/to a bytevector
* add functions `(wire-register-rtd) (wire-reserve-tag) (wire-inner-len) (wire-inner-get) (wire-inner-put)`
  for registering and implementing custom serializers and deserializers
* extend functions `(sh-find-job)` and `(sh-job)` to accept `#f`
  as a shortcut for `(or (sh-current-job) (sh-globals))`
* improve functions `(sh-fg) (sh-bg) (sh-wait)` to re-raise exceptions thrown by Scheme jobs
* allow getting/setting port position in ports returned by functions
  `(open-fd-redir-...-port) (open-file-binary-input-port) (open-file-utf8b-input-port)`
  `(current-input-port) (current-out-port) (current-error-port) (sh-stdin) (sh-stdout) (sh-stderr)`
  if the underlying file descriptor supports seeking.
* improve the following functions to also accepts bytespan paths in addition to bytevector, string and charspan:
  `(delete-directory) (delete-file) (directory-list) (directory-list-type) (mkdir)`
  `(file-delete) (file-directory?) (file-exists?) (file-rename) (file-regular?) (file-type)`
  `(open-file-fd) (open-file-utf8b-input-port) (text->bytevector) (text->bytevector0)`
* rewrite macro `(for)`
* improve source code conformance to r6rs, and start creating an - incomplete - Racket compatibility layer
  to use when compiling schemesh from Racket #!r6rs mode

### release v0.8.1, 2025-03-15

* fix job control on Scheme jobs `$()` running inside a pipeline
* replace standard Scheme textual ports `(current-input-port)` `(current-output-port)` `(current-error-port)`
  with interruptible ones that honor job redirections and use UTF-8b
* update Scheme binary ports `(sh-stdin)` `(sh-stdout)` `(sh-stderr)`
  to be interruptible and honor job redirections
* export several new functions to iterate on lists, hashtables, strings, etc.
* internally rewrite and cleanup POSIX signal handlers
* add script `utils/show_system_info.sh`

### release v0.8.0, 2025-03-04

* add job-creating syntax `$( )` and macro `(shell-expr)` and corresponding function `(sh-expr)`
* implement job control also on arbitrary Scheme code running inside a `(shell-expr)`
* update both shell parser and Scheme parser to expand `$( )` to `(shell-expr ( ))`
* ignore newlines at the beginning of shell syntax:
  allows writing Scheme code in a new line *under* the REPL prompt
* ignore newlines after shell syntax tokens
  `{` `[` `!` `;` `&` `&&` `||` `|` `|&` `<` `<>` `>` `>>` `<&` `>&`
  in most cases, this removes the need to end a line with `\`
* always queue job status change notifications for later displaying them,
  instead of sometimes displaying them immediately
* remove function `(sh-resume)` and subsume it into `(sh-wait)`
* rename shell builtin `expr` -> `value`
* remove function `(sh-bool)`

### release v0.7.7, 2025-03-01

* fix `(vhistory-save-to-path)` not to raise exceptions: it was preventing schemesh
  from exiting if directory `~/.cache/schemesh/` does not exist or is not writable

### release v0.7.6, 2025-03-01

* add new syntax for shell command substitution `$[ ]`  and deprecate the old syntax `$( )`
* add string-related functions `(string-any) (string-contains) (string-count) (string-every)
  (string-index) (string-index-right) (string-join) (string-map) (string-replace-all)
  (string-replace-prefix) (string-replace-suffix) (string-prefix?) (string-suffix?)`
* export string-related functions `(in-string) (string-fill-range!) (string-is-unsigned-base10-integer?)
  (string-is-signed-base10-integer?) (string-iterate) (string-list?) (string-list-split-after-nuls)
  (string-prefix/char?) (string-count=) (substring=?) (substring<?) (string-replace/char!)
  (string-rtrim-newlines!) (string-split) (string-split-after-nuls) (string-suffix/char?) (string-trim-split-at-blanks)`
* automatically `(import (schemesh))` at REPL
* rename existing functions to follow r7rs naming conventions
* fix `(parse-shell-forms)` to parse to nothing backslash-newline outside quotes
* fix `(parse-lisp-forms)` to correctly parse backslash-whitespace-newline-whitespace inside double quotes
* fix C function `c_tty_setraw()` to retrieve the current tty configuration at every call,
  because one of the executed commands may have changed it: we want to preserve such changes for future commands
* fix bugs in `(on-list)` and `(in-fixnum-interval)`
* add functions `(directory-list) (directory-list-type) (file-delete) (in-exact-interval)
  (mkdir) (ok?) (string-replace-start) (string-replace-end)`
* refactor function `(in-interval)` to call one of `(in-fixnum-interval) (in-exact-interval) (in-flonum-interval)`
* rename shell builtin `test` -> `expr` and rename macro `(shell-test)` -> `(shell-expr)`
* implement builtin `cd-` changes current directory of *parent* job to its previous value.
* implement builtin `parent` executes another builtin with its parent job temporarily set to its grandparent job.
  if used multiple times, as for example `parent parent cd ..`, the effects are cumulative.

### release v0.7.5, 2025-02-14

* fix `utils/find_chez_scheme_dir.sh` for Alpine linux
* fix a pair of bugged assertions that were triggered by valid code `{echo $A=}`
* fix support for Chez Scheme down to 9.5.8
* rename shell builtin "error" -> "test"
* add command line option `--load-file FILE` and support for loading compiled Scheme libraries *.so
* add functions `(alist->eq-hashtable) (alist->eqv-hashtable) (alist->hashtable) (file-rename)
  (in-bytevector) (in-bytespan) (in-chargbuffer) (in-charline) (in-charlines) (in-charspan)
  (in-gbuffer) (in-hash) (in-list) (in-span) (in-string) (in-vector)
  (in-fixnum-interval) (in-flonum-interval) (in-interval) (on-list)
  (sh-bool) (sh-run/string-split-after-nuls) (sh-version) (string-replace)`
* add macros `(for) (for*) (library-reexport) (shell-test)`
* add Fedora Linux build instructions to README.md
* add "Troubleshooting" section to README.md
* add more examples to README.md

### release v0.7.4, 2025-02-10

* update Makefile to follow GNU conventions for installation directories,
  installation programs and Makefile variable names
* add shell builtins "help copyright" and "help warranty"
* improve shell builtin "fg" to show job status if it's not finished yet when "fg" exits
* when entering and leaving `(lineedit-read)`, sort by id then by pid the queued job status change notifications
* mention `(sh-run/string)` and `(sh-start/fd-stdout)` in README.md

### release v0.7.3, 2025-02-09

* at startup, `(sh-eval-file)` initialization file `~/.config/schemesh/repl_init.ss` if it exists
* at exit, `(sh-eval-file)` shutdown file `~/.config/schemesh/repl_quit.ss` if it exists
* at startup, load history from ~/.cache/schemesh/history.txt
* at exit, save history to the same file
* implement `(include/lang)` and `(include/lang*)`
* implement pipeline operator `|&`
* implement shell builtins: `bg fg exec exit export unexport global help set unset source`
* extend `(sh-cmd* "ENV_VAR" '= "VALUE")` to set environment variables in *parent* job
* modify builtin `cd` to change current directory of *parent* job
* modify builtin `pwd` to print current directory of *parent* job
* implement builtin `global`, for running another builtin with its parent job temporarily set to (sh-globals)
* implement shell builtin `unsafe`, for creating `(sh-cmd*)` commands whose first argument - the program name -
  is not a string but a closure, as for example the output of a subshell, a wildcard etc.
* extend builtin `alias`, without arguments now lists existing aliases
* mark and hide temporary redirections created by `(sh-pipe)` and `(sh-pipe*)`
* improve `(shell-backquote)`, now expands to a closure that accepts a job
  and calls `(sh-run/string-rtrim-newlines)` on a new job with the same parent as the job argument
* fix `(sh-read...)` exception while parsing `#!/some/absolute/path` at the beginning of input
* fix hang in `{history | foo}` due to builtins being fully executed when they start:
  pipe fd becomes full and blocks further writes, preventing builtin "history" from finishing
  and causing a deadlock: "foo" is never started.
  The solution was: modify `(sh-pipe)` to always start builtins and multijobs in a subprocess
* consume received signals, i.e. `(repl-lineedit)` calls `(sh-consume-sigchld)`,
  which calls C `waitpid(-1, WNOHANG)` for any child process, updates (sh-pid-table)
  and calls `(sh-job-status)` on all parents of each job that changes status.
