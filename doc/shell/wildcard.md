# shell wildcards

Shell wildcards, i.e. searching for file system entries that match a pattern, is implemented by library
`(schemesh shell job)` which is also included in `(schemesh shell)` and `(schemesh)`.

### Wildcard patterns

The supported wildcard patterns are equivalent to the ones found in most shells:
* `*` matches zero or more arbitrary characters excluding `/`
* `?` matches a single arbitrary character excluding `/`
* `[...]` matches a single character among the listed ones.
  It also allows ranges, as for example `c-h` or `"fop-xbar"`, and `/` is not allowed among the listed characters.
* `[!...]` matches a single character **not** among the listed ones.
  It also allows ranges, as for example `c-h` or `"fop-xbar"`, and `/` is not allowed among the listed characters
  (it is also never matched).

Note that as in most other shells, file system entries whose name starts with `.` are treated specially:
they are considered hidden, and the initial `.` in their name must be matched explicitly, i.e. **not** by a wildcard pattern.

The mechanism to specify a wildcard pattern depends on the function or macro being invoked:
see each function or macro's documentation below for details.


### Alphabetical index
* [`(shell-glob)`](#shell-glob)
* [`(shell-glob*)`](#shell-glob-star)
* [`(shell-string)`](#shell-string)
* [`(shell-string*)`](#shell-string-star)
* [`(wildcard)`](#wildcard)


##### (shell-glob)
Macro `(shell-glob {...})` accepts a subset of shell syntax as argument `{...}`:

only a single argument is allowed, i.e. a possibly partially quoted string, containing:
* zero or more wildcard patterns `*` `?` `[...]` `[!...]`
* zero or more environment variables `$NAME`

At least one wildcard pattern or environment variable must be present.

It expands to a list of strings: the file system entries that match the pattern, sorted lexicographically.

Examples:
```scheme
(shell-glob {/*})
("/bin" "/boot" "/dev" "/etc" "/home" "/lib" "/lost+found" "/mnt"
 "/proc" "/root" "/run" "/sbin" "/srv" "/sys" "/tmp" "/usr" "/var")
```

```scheme
(shell-glob {*[cp-sS]})
("asm_embed.S" "bootstrap" "c" "doc" "eval.c" "ipc" "main.c" "parser")
```

```scheme
FOO=me
(shell-glob {*"$FOO"*[ch]})
("chezscheme.h" "schemesh")
```

```scheme
(shell-glob {x})
;; raises exception: missing wildcard patterns or environment variables
```

Note: shell syntax has an ambiguity parsing `[` at the beginning of a command,
because it may indicate either a subshell or a wildcard pattern.
Currently `[` in such position is parsed as a subshell - to parse it as a wildcard pattern,
insert a quoted empty string before the `[`.
Examples:
```
(shell-glob {''[a-c]*})
("asm_embed.S" "bootstrap" "c" "chezscheme.h" "containers" "containers.o" "conversions" "countdown")
```
```
(shell-glob {""[bar]*})
("asm_embed.S" "bootstrap" "reflect" "repl")
```


##### <span id="shell-glob-star">(shell-glob*)</span>
Macro `(shell-glob* {...})` accepts a larger subset of shell syntax as argument `{...}`:

only a single argument is allowed, i.e. a possibly partially quoted string, containing:
* zero or more wildcard patterns `*` `?` `[...]` `[!...]`
* zero or more environment variables `$NAME`
* zero or more command substitutions ``` `cmd args ...` ``` or `$(cmd args ...)`

At least one wildcard pattern, environment variable or command substitution must be present.

It expands to a list of strings: the file system entries that match the pattern, sorted lexicographically.


##### (shell-string)
Macro `(shell-string {...})` accepts a subset of shell syntax as argument `{...}`:

only a single argument is allowed, i.e. a possibly partially quoted string, containing:
* zero or more wildcard patterns `*` `?` `[...]` `[!...]`
* zero or more environment variables `$NAME`

At least one wildcard pattern or environment variable must be present.

The result depends on how many file system entries match the pattern:
* no matches: expands to a string containing the value of environment variables, and the wildcard patterns are copied verbatim
* one match: expands to the path of the matching file system entry
* two or more matches: raises an exception

Examples:
```scheme
(shell-string {$PATH})
"/usr/local/bin:/usr/bin:/bin"
```

```scheme
(shell-string {$PATH:$HOME/.local/bin})
"/usr/local/bin:/usr/bin:/bin:/home/user/.local/bin"
```

```scheme
(shell-string {v*})
"vscreen"
```

```scheme
(shell-string {x})
;; raises exception: missing wildcard patterns or environment variables
```

```scheme
(shell-string {x*})
;; raises exception: matches multiple file system entries
```

##### <span id="shell-string-star">(shell-string*)</span>
Macro `(shell-string* {...})` accepts a larger subset of shell syntax as argument `{...}`:

only a single argument is allowed, i.e. a possibly partially quoted string, containing:
* zero or more wildcard patterns `*` `?` `[...]` `[!...]`
* zero or more environment variables `$NAME`
* zero or more command substitutions ``` `cmd args ...` ``` or `$(cmd args ...)`

At least one wildcard pattern, environment variable or command substitution must be present.

As for `shell-string`, the result depends on how many file system entries match the pattern:
* no matches: expands to a string containing the value of environment variables, and the wildcard patterns are copied verbatim
* one match: expands to the path of the matching file system entry
* two or more matches: raises an exception


##### (wildcard)
Function `(wildcard job-or-id ...)` accepts as arguments an already tokenized wildcard pattern.

First argument `job-or-id` is the job to use, containing the current directory where to start searching.<br/>
Usual values are `#f` which means "whatever is the current job", or `#t` which means "the top-level job i.e. the shell itself".

Each subsequent argument must be one of:
* a string - matches literally, including any `/` or `.`
* the symbol `'*` - means the wildcard pattern `*` i.e. matches zero or more arbitrary characters excluding `/` and initial `.`
* the symbol `'?` - means the wildcard pattern `?` i.e. matches a single arbitrary character excluding `/` and initial `.`
* the symbol `'%` followed by a string - means the wildcard pattern `[...]` i.e. matches a single character among the listed ones
  (`/` is not allowed, and `.` is ignored if it's the first character of a file name).
  It also allows ranges, as for example `c-h` or `"fop-xbar"`
* the symbol `'%!` followed by a string - means the wildcard pattern `[...]` i.e. matches a single character **not** among the listed ones
  (again, `/` is not allowed, and `.` is ignored if it's the first character of a file name).
  It also allows ranges, as for example `c-h` or `"fop-xbar"`

It returns a list of strings: the file system entries that match the pattern, sorted lexicographically.

This function is useful when the wildcard pattern to match is known only at runtime,
because the equivalent `shell-glob` is a macro and its arguments are parsed at compile time.

Examples:
```scheme
(wildcard #f "/tmp/." '*)
("/tmp/.ICE-unix" "/tmp/.X0-lock" "/tmp/.X11-unix")
```

```scheme
(wildcard #f '% "a-c" '*)
("asm_embed.S" "bootstrap" "c" "chezscheme.h" "containers" "containers.o" "conversions" "countdown")
```

```scheme
(wildcard #f '% "_" '*)
() ; no matches
```

```scheme
(wildcard #f)
() ; no matches
```

Since `(wildcard)` returns a list of strings, it's also suitable for inserting it inside shell syntax.
Example:
```shell
(define temp-dir "/tmp/dir/")
(sh-run {rm -fr (wildcard #f temp-dir '*)})
```
this has the advantage that `(wildcard)` arguments can also be computed at run-time,
because `(wildcard)` is a function rather than a macro.

Note: such command is equivalent to `rm -fr /tmp/dir/*` and does **not** remove all files from `/tmp/dir/`:
it skips hidden files, i.e file names starting with `.`
