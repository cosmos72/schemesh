# shell job control and management

Shell jobs are implemented by library `(schemesh shell job)` which is also included in `(schemesh shell)` and `(schemesh)`.

Scheme functions to **create** shell jobs are not documented yet.

Scheme functions to **start and control** shell jobs are documented below.

Scheme functions to manage the **environment variables** of existing shell jobs are documented in [env.md](env.md)

Scheme functions to **redirect** existing shell jobs, and to access redirected ports and file descriptors of a job, are documented in [redirect.md](redirect.md).


### Alphabetical index
* [`(sh-bg)`](#sh-bg)
* [`(sh-fg)`](#sh-fg)
* [`(sh-job)`](#sh-job)
* [`(sh-job?)`](#sh-job)
* [`(sh-job->string)`](#sh-job-string)
* [`(sh-job-display)`](#sh-job-display)
* [`(sh-job-find)`](#sh-job-find)
* [`(sh-job-id)`](#sh-job-id)
* [`(sh-job-pgid)`](#sh-job-pgid)
* [`(sh-job-pid)`](#sh-job-pid)
* [`(sh-job-status)`](#sh-job-status)
* [`(sh-job-verbose?)`](#sh-job-verbose)
* [`(sh-job-verbose?-set!)`](#sh-job-verbose-set)
* [`(sh-kill)`](#sh-kill)
* [`(sh-options)`](#sh-options)
* [`(sh-run)`](#sh-run)
* [`(sh-run/i)`](#sh-runi)
* [`(sh-start)`](#sh-start)
* [`(sh-wait)`](#sh-wait)


### Starting a job

An existing job can be started with the following Scheme functions:

##### (sh-start)
`(sh-start job)` or `(sh-start job options)` starts a job in background and returns immediately, without waiting for it to exit.<br/>

Optional argument `options` is described in [`(sh-options)`](#sh-options) and defaults to the empty list.

Note: some shell builtins or (rarely) very fast commands may have already exited by the time this function returns.

Returns job status, which is usually `(running JOB_ID)` but any other value is allowed too.<br/>
For the complete list of possible job statuses, see `[(sh-job-status)](#sh-job-status).

See also:
[`(sh-start/fd1)`](redirect.md#sh-startfd1)
[`(sh-start/fds)`](#redirect.mdsh-startfds)
[`(sh-start/ports)`](#redirect.mdsh-startports)


##### (sh-run/i)
`(sh-run/i job)` or `(sh-run/i job options)` starts a job in foreground and waits for it to exit or stop.<br/>
Note that this function WILL return early if job is stopped.

Optional argument `options` is described in [`(sh-options)`](#sh-options) and defaults to the empty list.

Returns job status, which is guaranteed to be one of `(ok ...)` `(exception ...)` `(failed ...)` `(killed ...)` `(stopped ...)`.<br/>
For the complete list of possible job statuses, see `[(sh-job-status)](#sh-job-status).

See also:
[`(sh-run)`](#sh-run)
[`(sh-run/bytevector)`](redirect.md#sh-runbytevector)
[`(sh-run/string)`](redirect.md#sh-runstring)


##### (sh-run)
`(sh-run job)` or `(sh-run job options)` starts a job in foreground and waits for it to exit.<br/>
Does NOT return early if job is stopped, use [`(sh-run/i)`](#sh-runi) for that.

Optional argument `options` is described in [`(sh-options)`](#sh-options) and defaults to the empty list.

Returns job status, which is guaranteed to be one of `(ok ...)` `(exception ...)` `(failed ...)` `(killed ...)`.<br/>
For the complete list of possible job statuses, see `[(sh-job-status)](#sh-job-status).

See also:
[`(sh-run/i)`](#sh-runi)
[`(sh-run/bytevector)`](redirect.md#sh-runbytevector)
[`(sh-run/string)`](redirect.md#sh-runstring)


### Waiting for a job

The following functions resume a stopped job and possibly wait for it to stop or exit.

##### (sh-bg)
`(sh-bg job-or-id)` resumes a job in background, and immediately returns updated job status,
without waiting for it to stop or exit.

If job has already exited, returned job status will indicate that.

For the complete list of possible job statuses, see `[(sh-job-status)](#sh-job-status).

See also:
[`(sh-fg)`](#sh-fg)
[`(sh-wait)`](#sh-wait)


##### (sh-fg)
`(sh-fg job-or-id)` resumes a job in foreground, and waits for it to stop or exit.

Returns updated job status, which is guaranteed to be one of  `(ok ...)` `(exception ...)` `(failed ...)` `(killed ...)` `(stopped ...)`.<br/>
For the complete list of possible job statuses, see `[(sh-job-status)](#sh-job-status).

See also:
[`(sh-bg)`](#sh-bg)
[`(sh-wait)`](#sh-wait)


##### (sh-wait)
`(sh-wait job-or-id)` resumes a job in foreground, and waits for it to exit.<br/>
Does NOT return early if job is stopped, use [`(sh-fg)`](#sh-fg) for that.

Returns updated job status, which is guaranteed to be one of  `(ok ...)` `(exception ...)` `(failed ...)` `(killed ...)`.<br/>
For the complete list of possible job statuses, see `[(sh-job-status)](#sh-job-status).

See also:
[`(sh-bg)`](#sh-bg)
[`(sh-fg)`](#sh-fg)


### Setting and retrieving job attributes

##### (sh-job-id)
`(sh-job-id job-or-id)` returns the ID of a started job: a fixnum >= 0, or `#f` if not currently available.

##### (sh-job-pgid)
`(sh-job-id job-or-id)` returns the POSIX process group ID of a started job, or `#f` if not currently available.

##### (sh-job-pid)
`(sh-job-id job-or-id)` returns the POSIX process ID of a started job, or `#f` if not currently available.

##### (sh-job-status)
`(sh-job-status job-or-id)` returns the `status` object representing the job's current status.

TODO: document possible values

##### (sh-job-verbose?)
`(sh-job-verbose? job)` returns `#t` (the default) if job's status changes should be notified at REPL, otherwise returns `#f`.

Note: if `(sh-job-verbose? job)` returns `#f` then the job is "silent" and will not have a job ID.

##### (sh-job-verbose?-set!)
`(sh-job-verbose?-set! job verbose?)` sets verbose/silent flag for a job.<br/>
Job must be just created or already exited, i.e. NOT running or stopped.

If `verbose?` is truish then the job is set to "verbose": it may have a job ID, and its status changes will be notified at REPL.

If `verbose?` is `#f` then job is set to "silent": it will NOT have a job ID, and its status changes will NOT be notified at REPL.

