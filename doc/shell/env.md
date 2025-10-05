# schemesh shell job environment variables

Shell jobs are implemented by library `(schemesh shell job)` which is also included in `(schemesh shell)` and `(schemesh)`.

Shell syntax to create and redirect shell jobs is the same as POSIX shells,
and differences are documented in the main [README.md](../../README.md).

Scheme functions to **create** shell jobs are not documented yet.

Scheme functions to **modify** the environment variables of existing shell jobs are documented below.

Scheme functions to **redirect** existing shell jobs, and to access redirected file descriptors of a Scheme job, are documented in [redirect.md](redirect.md).

### Alphabetical index
* [`(sh-env-copy)`](#sh-env-copy)
* [`(sh-env-delete!)`](#sh-env-delete!)
* [`(sh-env-ref)`](#sh-env-ref)
* [`(sh-env-set!)`](#sh-env-set!)
* [`(sh-env-visibility-ref)`](#sh-env-visibility-ref)
* [`(sh-env-visibility-set!)`](#sh-env-visibility-set!)

### Jobs environment variables

Each job created by schemesh has its own environment variables, redirections and current directory.

When a new job is created, it *references* environment variables, redirections, and current directory of its parent job.
Note: the main schemesh process acts the default parent job if none is specified.

This means that by default, changing the environment variables, redirections or current directory of a parent job also affects all its children jobs.

To stop such sharing, just set a job's environment variable (or delete it): the new value shadows the inherited one, and such variable is no longer shared.
The same applies for redirections and current directory: if you set some of them, they shadow inherited values.

All the functions to access or modify jobs' environment variables always accept as first parameter a `job-or-id`:
it can be a job object, or the numeric ID of a job, or one of the special values:
* `#t` shortcut for the main schemesh process, i.e. the job (sh-globals)
* `#f` shortcut for the current scheme job, i.e. (sh-current-job)

##### (sh-env-ref)
`(sh-env-ref job-or-id name)` returns the string value of environment variable `name` for specified job or job-id.
Argument `name` must be a string. Returns the empty string if environment variable `name` is not found.

##### (sh-env-set!)
`(sh-env-set! job-or-id name value [visibility])` sets the string value of environment variable `name` for specified job or job-id.
Argument `name` must be a string, and optional `visibility` must be one of the symbols `'export` `'private` `'maintain`.

Only environment variables with `visibility` equal to `'export` are passed to child processes,
and `visibility` not specified or equal to `'maintain` indicates to preserve the current visibility of specified environment variable.

##### (sh-env-visibility-ref)
`(sh-env-visibility-ref job-or-id name)` returns the value and visibility of environment variable `name` for specified job or job-id.

If the environment variable `name` is found, returns `(value str vis)`
where `str` is the the string value of environment variable,
and `vis` is one of the symbols `'export` `'private`.

If the environment variable `name` is not found, returns `(values #f #f)`.

##### (sh-env-visibility-set!)
`(sh-env-visibility-set! job-or-id name visibility)` is equivalent to `(sh-env-set! job-or-id name [visibility])`
with the only difference that all arguments are mandatory.

##### (sh-env-delete!)
`(sh-env-delete! job-or-id name)` removes environment variable `name` from specified job or job-id.

##### (sh-env-copy)
`(sh-env-copy job-or-id visibility)` returns a string->string hashtable containing a copy of all environment variables for specified job or job-id.

Argument `visibility` must be one of the symbols:
* `'export` indicating that only exported environment variables should be returned
* `'all` indicating that both exported and private environment variables should be returned
