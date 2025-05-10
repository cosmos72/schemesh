# schemesh shell job redirection

Shell jobs are implemented by library `(schemesh shell job)` which is also included in `(schemesh shell)` and `(schemesh)`.

Shell syntax to create and redirect shell jobs is the same as POSIX shells,
and differences are documented in the main [README.md](../../README.md).

Scheme functions to **create** shell jobs are not documented yet.

Scheme functions to **redirect** existing shell jobs are documented below.

### Index
* [`(sh-run/bytevector)`](#sh-run-bytevector)
* [`(sh-run/string)`](#sh-run-string)
* [`(sh-start/fd-stdout)`](#sh-start-fd-stdout)
* [`(sh-start/ports)`](#sh-start-ports)
* [`(sh-start/fds)`](#sh-start-fds)

### Run a job with redirections

The following functions add redirections to an existing job, start the job in foreground,
wait for it to finish, and return the output produced by the job.

The added redirections are *temporary* i.e. they are automatically removed when the job finishes.

###### sh-run/bytevector
`(sh-run/bytevector job)` or `(sh-run/bytevector job job-options)` starts a job in foreground and waits for it to exit.<br/>
Does NOT return early if job is stopped, use `(sh-run/i)` for that. Options are the same as `(sh-start)`.<br/>
Reads job's standard output and returns it converted to bytevector.<br/>
Note: if job finishes with a status `(exception ...)` `(killed 'sigint)` `(killed 'sigquit)` tries to kill `(sh-current-job)` then raises exception.

###### sh-run/string
`(sh-run/string job)` or `(sh-run/string job job-options)` same as [`(sh-run/bytevector)`](#sh-run-bytevector), except that job's output is converted to a string:<br/>
starts a job in foreground and waits for it to exit.<br/>
Does NOT return early if job is stopped, use `(sh-run/i)` for that. Options are the same as `(sh-start)`.<br/>
Reads job's standard output and returns it converted to string.<br/>
Note: if job finishes with a status `(exception ...)` `(killed 'sigint)` `(killed 'sigquit)` tries to kill `(sh-current-job)` then raises exception.

### Start a job with redirections

The following functions add redirections to an existing job, start the job in background,
return *immediately* without waiting for the job to finish,
and return ports or file descriptors connected to the started job.

The returned ports or file descriptors must be explicitly closed when no longer needed,
because each one consumes an OS-level file descriptor.

The added redirections are *temporary* i.e. they are automatically removed when the job finishes.

###### sh-start/fd-stdout
`(sh-start/fd-stdout job)` or `(sh-start/fd-stdout job options)` starts a job in background, returns a file descriptor fixnum<br/>
for reading job's standard output - for example with `(open-fd-input-port fd)` or `(fd-read-some fd bytevector)`.<br/>
Options are the same as `(sh-start)`. File descriptor must be closed with `(fd-close)` when no longer needed.


###### sh-start/ports
`(sh-start/ports job [redirections [?transcoder-sym [buffer-mode [options]]]]))` starts a job in background,<br/>
returns a list of binary or textual ports connected to the job.<br/>

Again, each port must be explicitly closed when no longer needed, because it consumes an OS-level file descriptor.

Optional arguments are:
* `redirections` a property list i.e. an even-sized list containing zero or redirections.
  Each redirection is a file descriptor fixnum followed by a direction symbol '<& '>& or '<>&
  Examples:
  `'(1 >&)` redirect job's standard output i.e. file descriptor 1 to write to a pipe.
           The function will return a list containing one element: the read side of such pipe, converted to an input port.

  `'(0 <& 1 >& 2 >&)`
     redirect job's standard input i.e. file descriptor 0 to read from a pipe,
     redirect job's standard output i.e. file descriptor 1 to write to a pipe,
     redirect job's standard error i.e. file descriptor 2 to write to a pipe.
     this is also the default if redirections are not specified.
     The function will return a list containing three element: the other side of such pipes, appropriately converted to input or output ports.

  `'(0 <& 17 <>&)`
     redirect job's file descriptors 0 to read from a pipe, and 17 file descriptor to read and write from a socketpair.
     The function will return a list containing two elements: the write side of job's standard input pipe, converted to output port,
     and the socketpair connected to job's file descriptor 17, converted to input/output port.


* `?transcoder-sym` must be one of: `#f` `'binary` `text` `'utf8b`<br/>
  If it's not specified or is either `#f` or `'binary`, returned ports will be binary ports.<br/>
  Otherwise returned ports will be textual ports that use UTF-8b for converting between bytes and characters.

* `buffer-mode` must be a valid port buffer-mode, i.e. one of `'block` `'line` `'none`
  If not specified, defaults to `'block`.
  The usual Scheme convention applies: we recommend writing it as one of `(buffer-mode block)` `(buffer-mode line)` `(buffer-mode none)`
  as they better describe the meaning and check for validity at compile-time.

* `options` is described in `(sh-start)` and defaults to the empty list.


###### sh-start/fds
`(sh-start/fds job [redirections [options]])` is a lower-level alternative to [`(sh-start/ports)`](#sh-start-ports):<br>
starts a job in background, returns a list of file descriptors connected to the job.<br/>
Optional argument `redirections` is described in [`(sh-start/ports)`](#sh-start-ports) above and again defaults to `'(0 <& 1 >& 2 >&)` <br/>
Optional argument `options` is described in `(sh-start)` and defaults to the empty list.<br/>
Returned file descriptors must be closed with `(fd-close)` when no longer needed.
