# schemesh shell job redirection

Shell jobs are implemented by library `(schemesh shell job)` which is also included in `(schemesh shell)` and `(schemesh)`.

Shell syntax to create and redirect shell jobs is the same as POSIX shells,
and differences are documented in the main [README.md](../../README.md).

Scheme functions to **create** shell jobs are not documented yet.

Scheme functions to **redirect** existing shell jobs, and to access redirected file descriptors of a Scheme job, are documented below.

### Index
* [`(current-input-port)`](#current-input-port)
* [`(current-error-port)`](#current-error-port)
* [`(current-output-port)`](#current-output-port)
* [`(sh-fd)`](#sh-fd)
* [`(sh-port)`](#sh-port)
* [`(sh-run/bytevector)`](#sh-runbytevector)
* [`(sh-run/string)`](#sh-runstring)
* [`(sh-start/fd-stdout)`](#sh-startfd-stdout)
* [`(sh-start/fds)`](#sh-startfds)
* [`(sh-start/ports)`](#sh-startports)
* [`(sh-stdin)`](#sh-stdin)
* [`(sh-stderr)`](#sh-stderr)
* [`(sh-stdout)`](#sh-stdout)

### Run a job with redirections

The following functions add redirections to an existing job, start the job in foreground,
wait for it to finish, and return the output produced by the job.

The added redirections are *temporary* i.e. they are automatically removed when the job finishes.

##### (sh-run/bytevector)
`(sh-run/bytevector job)` or `(sh-run/bytevector job options)` starts a job in foreground and waits for it to exit.<br/>
Does NOT return early if job is stopped, use `(sh-run/i)` for that. Options are the same as `(sh-start)`.<br/>
Reads job's standard output and returns it converted to bytevector.<br/>
Note: if job finishes with a status `(exception ...)` `(killed 'sigint)` `(killed 'sigquit)` tries to kill `(sh-current-job)` then raises exception.

Optional argument `options` is described in `(sh-start)` and defaults to the empty list.

##### (sh-run/string)
`(sh-run/string job)` or `(sh-run/string job options)` same as [`(sh-run/bytevector)`](#sh-runbytevector), except that job's output is converted to a string:<br/>
starts a job in foreground and waits for it to exit.<br/>
Does NOT return early if job is stopped, use `(sh-run/i)` for that. Options are the same as `(sh-start)`.<br/>
Reads job's standard output and returns it converted to string.<br/>
Note: if job finishes with a status `(exception ...)` `(killed 'sigint)` `(killed 'sigquit)` tries to kill `(sh-current-job)` then raises exception.

Optional argument `options` is described in `(sh-start)` and defaults to the empty list.

### Start a job with redirections

The following functions add redirections to an existing job, start the job in background,
return *immediately* without waiting for the job to finish,
and return ports or file descriptors connected to the started job.

The returned ports or file descriptors must be explicitly closed when no longer needed,
because each one consumes an OS-level file descriptor.

The added redirections are *temporary* i.e. they are automatically removed when the job finishes.

##### (sh-start/fd-stdout)
`(sh-start/fd-stdout job)` or `(sh-start/fd-stdout job options)` starts a job in background, returns a file descriptor fixnum<br/>
for reading job's standard output - for example with `(open-fd-input-port fd)` or `(fd-read-some fd bytevector)`.

Optional argument `options` is described in `(sh-start)` and defaults to the empty list.

Returned file descriptor must be closed with `(fd-close)` when no longer needed.

##### (sh-start/ports)
`(sh-start/ports job [redirections [transcoder-sym [buffer-mode [options]]]]))` starts a job in background,<br/>
returns a list of binary or textual ports connected to the job.<br/>

Again, each returned port must be explicitly closed when no longer needed, because it consumes an OS-level file descriptor.

Optional arguments are:
* `redirections` a property list i.e. an even-sized list containing zero or redirections.
  Each redirection is a file descriptor fixnum followed by a direction symbol `'<&` `'>&` or `'<>&`.
  Examples:

  `'(1 >&)` redirect job's standard output i.e. file descriptor `1` to write to a pipe.
     The function will return a list containing one element: the read side of such pipe, converted to an input port.

  `'(0 <& 1 >& 2 >&)`
     redirect job's standard input i.e. file descriptor `0` to read from a pipe,
     redirect job's standard output i.e. file descriptor `1` to write to a pipe,
     redirect job's standard error i.e. file descriptor `2` to write to a pipe.
     this is also the default if redirections are not specified.
     The function will return a list containing three element: the other side of such pipes, appropriately converted to input or output ports.

  `'(0 <& 17 <>&)`
     redirect job's file descriptors 0 to read from a pipe, and file descriptor `17` to read and write from a socketpair.
     The function will return a list containing two elements: the write side of job's file descriptor `0`, converted to output port,
     and the socketpair connected to job's file descriptor `17`, converted to input/output port.

* `transcoder-sym` must be one of: `'binary` `'text` or `'utf8b`.<br/>
  If `'binary`, returned ports will be binary.<br/>
  In all other cases - not specified or one of `'text` `'utf8b` - returned ports will be textual
  and will use UTF-8b for converting between bytes and characters.

* `buffer-mode` must be a valid port buffer-mode, i.e. one of `'block` `'line` or `'none`.<br/>
  If not specified, defaults to `'block`.<br/>
  The usual Scheme convention applies: we recommend writing it as one of `(buffer-mode block)` `(buffer-mode line)` `(buffer-mode none)`
  as they better describe the meaning and check for validity at compile-time.

* `options` is described in `(sh-start)` and defaults to the empty list.


##### (sh-start/fds)
`(sh-start/fds job [redirections [options]])` is a lower-level alternative to [`(sh-start/ports)`](#sh-startports):<br>
starts a job in background, returns a list of file descriptors connected to the job.<br/>

Optional arguments are:
* `redirections` is described in [`(sh-start/ports)`](#sh-startports) above and again defaults to `'(0 <& 1 >& 2 >&)`
* `options` is described in `(sh-start)` and defaults to the empty list.

Each returned file descriptor must be closed with `(fd-close)` when no longer needed,
because each one consumes an OS-level file descriptor.

### Access redirected ports from a Scheme job

As described in the main [README.md](../../README.md#scheme-jobs), Scheme jobs are wrappers around arbitrary Scheme code and are treated as jobs:<br/>
their file descriptors can be redirected with the usual shell syntax, they can be stopped and resumed, they can be used in a pipeline, etc.

The syntax for creating a Scheme jobs is trivial: just prefix `$` to any parenthesized Scheme expression or declaration.<br/>
Examples:
```lisp
$(display "hello\n") > greet.txt

$(display
  (let %fib ((i 10))
     (if (fx>? i 2)
       (fx+ (%fib (fx1- i))
            (%fib (fx- i 2)))
       1))) | cat
```

Inside a Scheme job, the standard ports `(current-input-port)` `(current-output-port)` and `(current-error-port)`
automatically honor redirections:<br/>
in the examples above, `$(display "hello\n") > greet.txt` writes to the file `greet.txt` in current directory<br/>
and `$(display (let %fib ...)) | cat` writes the 10th Fibonacci number to the pipe connected to `cat`.

If you prefer binary ports, you can use `(sh-stdin)` `(sh-stdout)` and `(sh-stderr)` or, more in general, `(sh-port N)`.

If you want OS-level file descriptors, there's also `(sh-fd 0)` `(sh-fd 1)` and `(sh-fd 2)` or, more in general, `(sh-fd N)`.

##### (sh-port)
`(sh-port fd)` or `(sh-port job-or-id fd)` or `(sh-port job-or-id fd transcoder-sym)` returns the port
corresponding to file descriptor `fd` for specified `job-or-id`, or for current job if `job-or-id` is not specified or is `#f`.

If `transcoder-sym` is `'binary`, the returned port is a binary port.
In all other cases - not specified or one of `'text` `'utf8b` - the returned port is textual
and uses UTF-8b for converting between bytes and characters.

Ports returned by this function are always buffered - they use `(buffer-mode block)` -
and are closed automatically when the corresponding job finishes.

##### (current-input-port)
If not set to a different value, `(current-input-port)` returns a wrapper port that reads from/writes to the textual port
corresponding to file descriptor `0` for current job.
It is logically equivalent to `(sh-port #f 0 'text)`, although it may return a different port.

##### (current-output-port)
If not set to a different value, `(current-output-port)` returns a wrapper port that reads from/writes to the textual port
corresponding to file descriptor `1` for current job.
It is logically equivalent to `(sh-port #f 1 'text)`, although it may return a different port.

##### (current-error-port)
If not set to a different value, `(current-error-port)` returns a wrapper port that reads from/writes to the textual port
corresponding to file descriptor `2` for current job.
It is logically equivalent to `(sh-port #f 2 'text)`, although it may return a different port.

##### (sh-stdin)
`(sh-stdin)` returns the binary port corresponding to file descriptor `0` for current job.
It returns the same port as `(sh-port #f 0 'binary)`.

##### (sh-stdout)
`(sh-stdout)` returns the binary port corresponding to file descriptor `1` for current job.
It returns the same port as `(sh-port #f 1 'binary)`.

##### (sh-stderr)
`(sh-stderr)` returns the binary port corresponding to file descriptor `2` for current job.
It returns the same port as `(sh-port #f 2 'binary)`.

##### (sh-fd)
`(sh-fd fd)` or `(sh-fd job-or-id fd)` returns the OS-level file descriptor corresponding to `fd` for specified `job-or-id`,
or for current job if `job-or-id` is not specified or is `#f`.

File descriptors returned by this function are closed automatically when the corresponding job finishes.
