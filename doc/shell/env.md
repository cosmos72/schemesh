# schemesh shell job environment variables

Shell jobs are implemented by library `(schemesh shell job)` which is also included in `(schemesh shell)` and `(schemesh)`.

Shell syntax to create and redirect shell jobs is the same as POSIX shells,
and differences are documented in the main [README.md](../../README.md).

Scheme functions to **create** shell jobs are not documented yet.

Scheme functions to **modify** the environment variables of existing shell jobs are documented below

Scheme functions to **redirect** existing shell jobs, and to access redirected file descriptors of a Scheme job, are documented in [redirect.md](redirect.md).

### Alphabetical index
TODO

### Jobs environment variables

Each job created by schemesh has its own environment variables, redirections and current directory.

When a new job is created, it *references* environment variables, redirections, and current directory of its parent job.
Note: the main schemesh process acts the default parent job if none is specified.

This means that by default, changing the environment variables, redirections or current directory of a parent job also affects all its children jobs.

To stop such sharing, just set a job's environment variable (or delete it): the new value shadows the inherited one, and such variable is no longer shared.
The same applies for redirections and current directory: if you set some of them, they shadow inherited values.

The functions to access or modify jobs' environment variables are:

##### (sh-env-ref)
`(sh-env-ref job-or-id name)`

##### (sh-env-set!)
`(sh-env-set! job-or-id name value [visibility])`

##### (sh-env-visibility-ref)
`(sh-env-visibility-ref job-or-id name)`

##### (sh-env-visibility-set!)
`(sh-env-visibility-set! job-or-id name visibility)`

##### (sh-env-delete!)
`(sh-env-delete! job-or-id name)`

##### (sh-env-copy)
`(sh-env-copy job-or-id visibility)`
