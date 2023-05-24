/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "shell.h"
#include "container.h"
#include "eval.h"
#include "io.h"
#include "lineedit.h"
#include "parse.h"
#include "posix.h"
#include "repl.h"
#include "signal.h"

#include <string.h>
#include <unistd.h>

#ifndef CHEZ_SCHEME_DIR
#error "please #define CHEZ_SCHEME_DIR to the installation path of Chez Scheme"
#endif

#define STR_(arg) #arg
#define STR(arg) STR_(arg)
#define CHEZ_SCHEME_DIR_STR STR(CHEZ_SCHEME_DIR)

#undef SCHEMESH_LIBRARY_SHELL_PARSE_DEBUG

static void c_environ_to_sh_env(char** env) {
  const char* entry;
  if (!env) {
    return;
  }
  for (; (entry = *env) != NULL; ++env) {
    const char* separator = strchr(entry, '=');
    size_t      namelen   = separator ? separator - entry : 0;
    iptr        inamelen  = Sfixnum_value(Sfixnum(namelen));
    if (namelen == 0 || inamelen < 0 || namelen != (size_t)inamelen) {
      continue;
    }
    call3("sh-env-set!", Strue, Sstring_utf8(entry, inamelen), Sstring_utf8(separator + 1, -1));
    call3("sh-env-export!", Strue, Sstring_utf8(entry, inamelen), Strue);
  }
}

/**
 * Define the record types "job" "cmd" "multijob" and functions operating on them.
 * Define the functions (sh-env...) and (sh-fd...)
 *
 * Convention: (sh) and (sh-...) are functions
 *             (shell) and (shell-...) are macros
 */
static void schemesh_define_library_shell_jobs(void) {

#define SCHEMESH_LIBRARY_SHELL_JOBS_EXPORT                                                         \
  "sh-job? sh-job-ref sh-job-span sh-job-status sh-cmd sh-cmd<> sh-cmd? sh-multijob sh-multijob? " \
  "sh-globals sh-global-env sh-env-copy sh-env-get sh-env-set! sh-env-unset! "                     \
  "sh-env-exported? sh-env-export! sh-env->vector-of-bytevector0 "                                 \
  "sh-start sh-bg sh-fg sh-run sh-run-capture-output sh-wait sh-and sh-or sh-and-or* "             \
  "sh-list sh-list* sh-fd-redirect! sh-fds-redirect! "

  eval(
      "(library (schemesh shell jobs (0 1))\n"
      "  (export " SCHEMESH_LIBRARY_SHELL_JOBS_EXPORT ")\n"
      "  (import\n"
      "    (rnrs)\n"
      "    (rnrs mutable-pairs)\n"
      "    (only (chezscheme) foreign-procedure record-writer reverse! void)\n"
      "    (schemesh containers misc)\n"
      "    (schemesh containers span)\n"
      "    (schemesh containers hashtable)\n"
      "    (schemesh conversions)\n"
      "    (schemesh pid)\n"
      "    (schemesh fd)\n"
      "    (schemesh signal))\n"
      "\n"
      /** Define the record type "job" */
      "(define-record-type\n"
      "  (job %make-job sh-job?)\n"
      "  (fields\n"
      "    (mutable pid)\n"               /* fixnum: process id,       -1 if unknown     */
      "    (mutable pgid)\n"              /* fixnum: process group id, -1 if unknown     */
      "    (mutable last-status)\n"       /* cons: last known status                     */
      "    (mutable to-redirect-fds)\n"   /* vector: fds to redirect between fork() and
                                             (subshell-func)                             */
      "    (mutable to-redirect-files)\n" /* vector of files to open before fork()       */
      "    (mutable to-close-fds)\n"      /* list: fds to close after spawn              */
      "    subshell-func\n"               /* procedure to run in fork()ed child.
                                             receives job as only argument, and its return
                                             value is passed to (exit-with-job-status)   */
      "    (mutable env)\n"               /* hashtable: overridden env variables, or '() */
      "    (mutable parent)))\n"          /* parent job, contains default values of env variables */
      "\n"                                /* and default redirections */
      /** Define the record type "cmd" */
      "(define-record-type\n"
      "  (cmd %make-cmd sh-cmd?)\n"
      "  (parent job)"
      "  (fields argv))\n" /* vector of bytevectors, each #\nul terminated */
      "\n"
      /** Define the record type "multijob" */
      "(define-record-type\n"
      "  (multijob %make-multijob sh-multijob?)\n"
      "  (parent job)"
      "  (fields\n"
      "    kind\n"                /* symbol: one of 'and 'or 'and-or 'list 'global */
      "    children\n"            /* span:   children jobs. May also contain symbols && || & ; */
      "    (mutable next-id)))\n" /* fixnum: first available index in span of children jobs */
      "\n"
      /**
       * Define the variable sh-globals, contains the global job.
       * Jobs started with (sh-start) will be children of sh-globals.
       *
       * Variable may be set! to a different value in subshells.
       */
      "(define sh-globals\n"
      /* waiting for sh-globals to exit is not useful:
       * pretend it already exited with unknown exit status */
      "  (%make-multijob (get-pid) (get-pgid 0) '(unknown . 0) (vector 0 1 2) (vector) '()\n"
      "    #f\n" /* subshell-func */
      "    (make-hashtable string-hash string=?) #f\n"
      "    'global (span #t) 1))\n" /* skip job-id 0 */
      "\n"
      /** Define the global hashtable pid -> job */
      "(define %table-pid->job (make-eq-hashtable))\n"
      "\n"
      /** Define function (pid->job) to convert pid to job, return #f if job not found */
      "(define (pid->job pid)\n"
      "  (assert (fixnum? pid))\n"
      "  (hashtable-ref %table-pid->job pid #f))\n"
      "\n"
      /* Define function (pid->job-set!) adds entries to the global hashtable pid -> job */
      "(define (pid->job-set! pid job)\n"
      "  (assert (fixnum? pid))\n"
      "  (assert (sh-job? job))\n"
      "  (hashtable-set! %table-pid->job pid job))\n"
      "\n"
      /* Define function (pid->job-delete!) removes entries from the global hashtable pid -> job */
      "(define (pid->job-delete! pid)\n"
      "  (assert (fixnum? pid))\n"
      "  (hashtable-delete! %table-pid->job pid))\n"
      "\n"
      /** Define the function (multijob-child-delete!), removes a job-id from a multijob */
      "(define (multijob-child-delete! globals job-id)\n"
      "  (let* ((arr (multijob-children globals))\n"
      "         (job-id\n"
      "           (if (fixnum? job-id)\n"
      "             (when (and (fx>=? job-id 0) (fx<? job-id (span-length arr)))\n"
      "               job-id)\n"
      "             (span-find arr 0 (span-length arr) (lambda (elem) (eq? elem job-id))))))\n"
      "    (when job-id\n"
      "      (span-set! arr job-id #f)\n"
      "      (multijob-next-id-set! globals\n"
      "                              (fxmin job-id (multijob-next-id globals))))))\n"
      "\n"
      /**
       * Define the function (multijob-child-put!), adds a job to a multijob
       * extending (multijob-span globals) as needed.
       * Return job-id assigned to job.
       */
      "(define (multijob-child-put! mjob j)\n"
      "  (let* ((arr     (multijob-children mjob))\n"
      "         (len     (span-length arr))\n"
      "         (next-id (multijob-next-id mjob))\n"
      "         (job-id (span-find arr next-id (fx- len next-id) not)))\n"
      "    (if job-id\n"
      "      (span-set! arr job-id j)\n" /* found a free job-id */
      "      (begin\n"                   /* no free job-id, enlarge span */
      "        (span-insert-back! arr j)\n"
      "        (set! job-id len)))\n"
      "    (let* ((start   (multijob-next-id mjob))\n"
      "           (len     (span-length arr))\n"
      "           (next-id (span-find arr start (fx- len job-id) not)))\n"
      "      (multijob-next-id-set! mjob\n"
      "                             (or next-id len)))\n"
      "    job-id))\n"
      "\n"
      /**
       * Define the function (sh-job-ref), converts job-id to job.
       * Job-id can be either a job,
       * or #t which means sh-globals - needed by C function c_environ_to_sh_env()
       * or a fixnum indicating one of the running jobs stored in (multijob-children sh-globals)
       *
       * Raises error if no job matches job-id.
       */
      "(define (sh-job-ref job-id)\n"
      "  (cond\n"
      "    ((eq? #t job-id) sh-globals)\n"
      "    ((fixnum? job-id)\n"
      "      (let* ((all-jobs (multijob-children sh-globals))\n"
      "             (job (when (and (fx>? job-id 0)\n" /* job-ids start at 1 */
      "                             (fx<? job-id (span-length all-jobs)))\n"
      "                    (span-ref all-jobs job-id))))\n"
      "        (unless (sh-job? job)\n"
      "          (error 'sh-job-ref \"job not found:\" job-id))\n"
      "        job))\n"
      "    ((sh-job? job-id) job-id)\n"
      "    (#t (error 'sh-job-ref \"not a job-id:\" job-id))))\n"
      "\n"
      /**
       * Define the function (sh-job-span), returns currently running jobs
       * as an span of pairs (job-id . job) sorted by job-id
       */
      "(define (sh-job-span)\n"
      "  (let ((src (multijob-children sh-globals))\n"
      "        (dst (span)))\n"
      "    (span-iterate src\n"
      "      (lambda (job-id job)\n"
      "        (when (sh-job? job)\n"
      "          (span-insert-back! dst (cons job-id job)))))\n"
      "    dst))\n"
      "\n"
      /**
       * Define the function (job-parents-iterate), calls (proc j) on given job and each of its
       * parents. Stops iterating if (proc) returns #f.
       */
      "(define (job-parents-iterate job-id proc)\n"
      "  (do ((parent (sh-job-ref job-id) (job-parent parent)))\n"
      "      ((or (not (sh-job? parent)) (not (proc parent))))))\n"
      "\n"
      /**
       * Define the function (job-parents-list), returns list containing all job's parents,
       * starting from sh-globals, until job itself.
       */
      "(define (job-parents-revlist job-id)\n"
      "  (let ((jlist '()))\n"
      "    (job-parents-iterate job-id\n"
      "      (lambda (job)\n"
      "        (set! jlist (cons job jlist))))\n"
      "    jlist))\n"
      "\n"
      /**
       * Define the function (job-parents-list), returns list containing job
       * followed by all its parents.
       */
      "(define (job-parents-list job-id)\n"
      "  (reverse! (job-parents-revlist job-id)))\n"
      "\n"
      /**
       * Create a cmd to later spawn it. Each argument must be a string or bytevector.
       * TODO: also support closures (lambda (job) ...) that return a string or bytevector.
       */
      "(define (sh-cmd program . args)\n"
      "  (%make-cmd -1 -1 '(new . 0) (vector 0 1 2) (vector) '()\n"
      "    #f\n"         /* subshell-func */
      "    '()\n"        /* overridden environment variables - initially none */
      "    sh-globals\n" /* parent job - initially the global job */
      "    (list->cmd-argv (cons program args))))\n"
      "\n"
      /**
       * Create a cmd to later spawn it. Each argument must be a string, bytevector or symbol.
       * Each symbols indicates a redirection and must be followed by a string or bytevector.
       * TODO: also support closures (lambda (job) ...) that return a string or bytevector.
       */
      "(define (sh-cmd<> program . args)\n"
      /** FIXME: implement allowed symbols: < > >> >& N< N> N>> N>& */
      "  (apply sh-cmd program args))\n"
      "\n"
      /** return global environment variables */
      "(define (sh-global-env)\n"
      "  (job-env sh-globals))\n"
      "\n"
      /**
       * Return direct environment variables of job, creating them if needed.
       * Returned hashtable does not include default variables,
       * i.e. the ones inherited from parent jobs.
       */
      "(define (job-direct-env job-id)\n"
      "  (let* ((job (sh-job-ref job-id))\n"
      "         (vars (job-env job)))\n"
      "    (unless (hashtable? vars)\n"
      "      (set! vars (make-hashtable string-hash string=?))\n"
      "      (job-env-set! job vars))\n"
      "    vars))\n"
      "\n"
      /**
       * Return a copy of job's environment variables,
       * including default variables inherited from parent jobs.
       * Argument which must be one of:
       *   'exported: only exported variables are returned.
       *   'all : unexported variables are returned too.
       */
      "(define (sh-env-copy job-id which)\n"
      "  (assert (memq which '(exported all)))\n"
      "  (let* ((jlist (job-parents-revlist job-id))\n"
      "         (vars (make-hashtable string-hash string=?))\n"
      "         (also-unexported? (eq? 'all which))\n"
      "         (only-exported? (not also-unexported?)))\n"
      "    (list-iterate jlist\n"
      "      (lambda (j)\n"
      "        (let ((env (job-env j)))\n"
      "          (when (hashtable? env)\n"
      "            (hashtable-iterate env\n"
      "              (lambda (cell)\n"
      "                (let ((name (car cell))\n"
      "                      (flag (cadr cell))\n"
      "                      (val  (cddr cell)))\n"
      "                  (cond\n"
      "                    ((or (eq? 'delete flag)\n"
      "                         (and only-exported? (eq? 'private flag)))\n"
      "                      (hashtable-delete! vars name))\n"
      "                    ((or (eq? 'export flag)\n"
      "                         (and also-unexported? (eq? 'private flag)))\n"
      "                      (hashtable-set! vars name val))))))))))\n"
      "    vars))\n"
      "\n"
      /**
       * Return environment variable named "name" of specified job.
       * If name is not found in job's environment, also search in environment
       * inherited from parent jobs.
       */
      "(define (sh-env-get job-id name)\n"
      "  (let ((ret \"\"))\n"
      "    (job-parents-iterate job-id\n"
      "      (lambda (j)\n"
      "        (let* ((vars (job-env j))\n"
      "               (elem (if (hashtable? vars) (hashtable-ref vars name #f) #f)))\n"
      "          (when (pair? elem)\n"
      "            (set! ret (cdr elem))\n"
      "            #f))))\n" /* name found, stop iterating */
      "    ret))\n"
      "\n"
      "(define (sh-env-set! job-id name val)\n"
      "  (let* ((vars (job-direct-env job-id))\n"
      "         (elem (hashtable-ref vars name #f)))\n"
      "    (if (pair? elem)\n"
      "      (set-cdr! elem val)\n"
      "      (hashtable-set! vars name (cons 'private val)))))\n"
      "\n"
      /**
       * Note: (sh-env-unset!) inserts an entry that means "deleted",
       * in order to override any parent job's environment variable
       * with the same name.
       */
      "(define (sh-env-unset! job-id name)\n"
      "  (let ((vars (job-direct-env job-id)))\n"
      "    (hashtable-set! vars name (cons 'delete \"\"))))\n"
      "\n"
      "(define (sh-env-exported? job-id name)\n"
      "  (let ((ret #f))\n"
      "    (job-parents-iterate job-id\n"
      "      (lambda (j)\n"
      "        (let* ((vars (job-env j))\n"
      "               (elem (if (hashtable? vars) (hashtable-ref vars name #f) #f)))\n"
      "          (when (pair? elem)\n"
      "            (set! ret (eq? 'export (car elem)))\n"
      "            #f))))\n" /* name found, stop iterating */
      "    ret))\n"
      "\n"
      "(define (sh-env-export! job-id name exported?)\n"
      "  (assert (boolean? exported?))\n"
      "  (let* ((j (sh-job-ref job-id))\n"
      /*        val may be in a parent environment */
      "         (val (sh-env-get j name))\n"
      "         (export (if exported? 'export 'private)))\n"
      "" /* (job-direct-env j) creates job environment if not yet present */
      "    (hashtable-set! (job-direct-env j) name (cons export val))))\n"
      "\n"
      /**
       * Extract environment variables from specified job and all its parents,
       * and convert them to a vector of bytevector0.
       * Argument which must be one of:
       * 'exported: only exported variables are returned.
       * 'all : unexported variables are returned too.
       */
      "(define (sh-env->vector-of-bytevector0 job-id which)\n"
      "  (string-hashtable->vector-of-bytevector0 (sh-env-copy job-id which)))\n"
      "\n"
      "(define (job-start-options->process-group-id options)\n"
      "  (let ((existing-pgid -1))\n"
      "    (list-iterate options\n"
      "      (lambda (option)\n"
      "        (when (fixnum? option)\n"
      "          (set! existing-pgid option)\n"
      "          #f)))\n" /* stop iterating on options */
      "    existing-pgid))\n"
      "\n"
      /**
       * NOTE: this is an internal implementation function, use (sh-start) instead.
       * This function does not update job's status, does not close (job-to-close-fds j)
       * - thus calling it manually leaks file descriptors - and does not register job
       * into global (pid->job) table nor into global job-id table.
       *
       * Description:
       * Start a cmd i.e. fork() and exec() an external process, optionally inserting it into an
       * existing process group.
       *
       * The new process is started in background, i.e. the foreground process group is NOT set
       * to the process group of the newly created process.
       *
       * Options is a list of zero or more of the following:
       *   process-group-id: a fixnum, if present and > 0 the new process will be inserted
       *     into the corresponding process group id - which must already exist.
       */
      "(define %cmd-start\n"
      "  (let ((c-spawn-pid (foreign-procedure \"c_spawn_pid\""
      "                        (scheme-object scheme-object scheme-object int) int)))\n"
      "    (lambda (c . options)\n"
      "      (assert (sh-cmd? c))\n"
      "      (let* ((process-group-id (job-start-options->process-group-id options))\n"
      "             (ret (c-spawn-pid\n"
      "                    (cmd-argv c)\n"
      "                    (job-to-redirect-fds c)\n"
      "                    (sh-env->vector-of-bytevector0 c 'exported)\n"
      "                    process-group-id)))\n"
      "        (when (< ret 0)\n"
      "          (raise-errno-condition 'sh-start ret))\n"
      "        (job-pid-set! c ret)\n"
      "        (job-pgid-set! c (if (> process-group-id 0) process-group-id ret))))))\n"
      "\n"
      /**
       * NOTE: this is an internal implementation function, use (sh-start) instead.
       * This function does not update job's status, does not close (job-to-close-fds j)
       * - thus calling it manually leaks file descriptors - and does not register job
       * into global (pid->job) table nor into global job-id table.
       *
       * Description:
       * Start a generic job, optionally inserting it into an existing process group.
       *
       * Forks a new subshell process in background, i.e. the foreground process group is NOT set
       * to the process group of the newly created process.
       *
       * The subshell process will execute the Scheme function (job-subshell-func j)
       * passing the job j as only argument,
       * then will call (exit-with-job-status) with the value returned by (job-subshell-func j)
       *
       * Options is a list of zero or more of the following:
       *   process-group-id: a fixnum, if present and > 0 the new subshell will be inserted
       *     into the corresponding process group id - which must already exist.
       */
      "(define %job-start\n"
      "  (let ((c-fork-pid (foreign-procedure \"c_fork_pid\" (scheme-object int) int)))\n"
      "    (lambda (j . options)\n"
      "      (assert (procedure? (job-subshell-func j)))\n"
      "      (let* ((process-group-id (job-start-options->process-group-id options))\n"
      "             (ret (c-fork-pid\n"
      "                    (job-to-redirect-fds j)\n"
      "                    process-group-id)))\n"
      "        (cond\n"
      "          ((< ret 0)\n"
      "            (raise-errno-condition 'sh-start ret))\n" /* fork() failed */
      "          ((= ret 0)\n"                               /* child */
      "            (let ((status '(exited . 255)))\n"
      "              (dynamic-wind\n"
      "                void\n"       /* run before body */
      "                (lambda ()\n" /* body */
      "                  (job-pid-set!  j (get-pid))\n"
      "                  (job-pgid-set! j (get-pgid 0))\n"
      /*                 this process now "is" the job j => update sh-globals' pid and pgid */
      "                  (job-pid-set!  sh-globals (job-pid j))\n"
      "                  (job-pgid-set! sh-globals (job-pgid j))\n"
      /*                 cannot wait on our own process */
      "                  (job-last-status-set! j '(unknown . 0))\n"
      "                  (set! status ((job-subshell-func j) j)))\n"
      "                (lambda ()\n" /* run after body, even if it raised a condition */
      "                  (exit-with-job-status status)))))\n"
      "          ((> ret 0)\n" /* parent */
      "            (job-pid-set! j ret)\n"
      "            (job-pgid-set! j (if (> process-group-id 0) process-group-id ret))))))))\n"
      "\n"
      /** Return #t if job was already started, otherwise return #f */
      "(define (job-started? j)\n"
      "  (and (fx>=? (job-pid j) 0) (fx>=? (job-pgid j) 0)))\n"
      /**
       * Start a cmd or a job.
       * If job parent is sh-globals, return job-id assigned to job.
       * Otherwise return (void).
       *
       * Options is a list of zero or more of the following:
       *   process-group-id: a fixnum, if present and > 0 the new process will be inserted
       *     into the corresponding process group id - which must already exist.
       */
      "(define (sh-start j . options)\n"
      "  (when (fx>=? (job-pid j) 0)\n"
      "    (error 'sh-start \"job already started\" (job-pid j)))\n"
      "  (cond\n"
      "    ((sh-cmd? j)\n"
      "      (apply %cmd-start j options))\n"
      "    ((procedure? (job-subshell-func j))\n"
      "      (apply %job-start j options))\n"
      "    (#t\n"
      "      (error 'sh-start \"cannot start job, it has bad or missing subshell-func\" j)))\n"
      "  (fd-close-list (job-to-close-fds j))\n"
      "  (job-last-status-set! j '(running . 0))\n" /* job can now be waited-for */
      "  (pid->job-set! (job-pid j) j)\n"           /* add job to pid->job table */
      "  (if (eq? sh-globals (job-parent j))\n"
      "    (multijob-child-put! sh-globals j)\n"
      "    (void)))\n"
      "\n"
      /**
       * Convert pid-wait-result to a symbolic job-status:
       *
       * If pid-wait-result is a pair (pid . exit-status) where exit-status is:
       *   not a fixnum, or < 0 => return (cons 'unknown exit-status)
       *   0..255               => return (cons 'exited  exit-status)
       *   256 + kill_signal    => return (cons 'killed  signal-name)
       *   512 + stop_signal    => return (cons 'stopped signal-name)
       *   >= 768               => return (cons 'unknown (fx- exit-status 768))
       *
       * If pid-wait-result is '() i.e. process status did not change,
       * return '(running . 0) indicating process is still running.
       *
       * Otherwise return (cons 'unknown pid-wait-result)
       */
      "(define (pid-wait->job-status pid-wait-result)\n"
      "  (cond"
      "    ((pair? pid-wait-result)\n"
      "      (let ((num (cdr pid-wait-result)))\n"
      "        (cond ((or (not (fixnum? num)) (fx<? num 0)) (cons 'unknown num))\n"
      "              ((fx<? num 256) (cons 'exited  num))\n"
      "              ((fx<? num 512) (cons 'killed  (signal-number->name (fxand num 255))))\n"
      "              ((fx<? num 768) (cons 'stopped (signal-number->name (fxand num 255))))\n"
      "              (#t            (cons 'unknown (fx- num 768))))))\n"
      "    ((null? pid-wait-result)\n"
      "      '(running . 0))\n"
      "    (#t\n"
      "      (cons 'unknown pid-wait-result))))\n"
      "\n"
      /**
       * Return #t if job-status is a pair whose car is in allowed-list:
       * otherwise return #f
       */
      "(define (job-status-member? job-status allowed-list)\n"
      "  (and (pair? job-status)\n"
      "       (memq (car job-status) allowed-list)))\n"
      "\n"
      /**
       * Wait for a cmd or job to exit or stop and return its status, which can be one of:
       *   (cons 'running ...)   ; may happen only if may-block is 'nonblocking
       *   (cons 'exited  exit-status)
       *   (cons 'killed  signal-name)
       *   (cons 'stopped signal-name)
       *   (cons 'unknown ...)
       *
       * Argument may-block must be one of: 'blocking 'nonblocking
       *
       * Warning: does not set the job as foreground process group,
       * consider calling (sh-fg j) instead.
       */
      "(define (job-wait j may-block)\n"
      "  (assert (memq may-block '(blocking nonblocking)))\n"
      "  (cond\n"
      "    ((job-status-member? (job-last-status j) '(exited killed unknown))\n"
      "      (job-last-status j))\n" /* job exited, and exit status already available */
      "    ((not (job-started? j))\n"
      "      (error 'job-wait \"job not started yet\" j))\n"
      "    (#t\n"
      /**    TODO: wait for ALL processes in job's process group? */
      "      (let* ((ret    (pid-wait (job-pid j) may-block))\n"
      "             (status (pid-wait->job-status ret)))\n"
      /*       if may-block is 'non-blocking, ret may be '() and status will be '(running . 0)
       *       indicating job status did not change i.e. it's (expected to be) still running */
      "        (job-last-status-set! j status)\n"
      "        (when (job-status-member? status '(exited killed unknown))\n"
      /*         job exited. it can now be spawned again */
      "          (when (eq? sh-globals (job-parent j))\n"
      "            (multijob-child-delete! sh-globals j))\n"
      "          (pid->job-delete! (job-pid j))\n"
      "          (job-pid-set! j -1)\n"
      "          (job-pgid-set! j -1))\n"
      "        status))))\n"
      "\n"
      /**
       * Return up-to-date status of a job or job-id, which can be one of:
       *   (cons 'new     0)
       *   (cons 'running 0)
       *   (cons 'exited  exit-status)
       *   (cons 'killed  signal-name)
       *   (cons 'stopped signal-name)
       *   (cons 'unknown ...)
       *
       * Note: this function also non-blocking checks if job status changed.
       */
      "(define (sh-job-status job-id)\n"
      "  (let ((j (sh-job-ref job-id)))\n"
      "    (when (job-status-member? (job-last-status j) '(running))\n"
      /**    nonblocking wait for job's pid to exit or stop.
       *     TODO: wait for ALL pids in process group? */
      "      (job-wait j 'nonblocking))\n"
      "    (job-last-status j)))\n"
      "\n"
      /**
       * Continue a job or job-id in background by sending SIGCONT to it.
       * Return job status, which can be one of:
       *
       *   (cons 'running 0)
       *   (cons 'exited  exit-status)
       *   (cons 'killed  signal-name)
       *   (cons 'stopped signal-name)
       *   (cons 'unknown ...)
       */
      "(define (sh-bg job-id)\n"
      "  (let ((j (sh-job-ref job-id)))\n"
      "    (cond\n"
      /**    if job already exited, return its exit status.
       *     if job is stopped, consider as running: we'll send SIGCONT to it below */
      "      ((job-status-member? (job-last-status j) '(exited killed unknown))\n"
      "        (job-last-status j))\n"
      "      ((not (job-started? j))\n"
      "        (error 'sh-bg \"job not started yet\" j))\n"
      "      (#t\n"
      /**      send SIGCONT to job's process group. may raise error */
      "        (pid-kill (fx- (job-pgid j)) 'sigcont)\n"
      /**      nonblocking wait for job's pid to exit or stop.
       *       TODO: wait for ALL pids in process group? */
      "        (job-wait j 'nonblocking)))))\n"
      "\n"
      /**
       * Continue a job or job-id by sending SIGCONT to it, wait for it to exit or stop,
       * and finally return its status, which can be one of:
       *
       *   (cons 'exited  exit-status)
       *   (cons 'killed  signal-name)
       *   (cons 'stopped signal-name)
       *   (cons 'unknown ...)
       *
       * Note: upon invocation, sets the job as fg process group.
       * Before returning, restores the sh-globals as fg process group.
       */
      "(define sh-fg\n"
      "  (let ((c-pgid-foreground (foreign-procedure \"c_pgid_foreground\" (int) int)))\n"
      "    (lambda (job-id)\n"
      "      (let ((j (sh-job-ref job-id)))\n"
      "        (cond\n"
      /**        if job already exited, return its exit status.
       *         if job is stopped, consider as running: we'll send SIGCONT to it below */
      "          ((job-status-member? (job-last-status j) '(exited killed unknown))\n"
      "            (job-last-status j))\n"
      "          ((not (job-started? j))\n"
      "            (error 'sh-fg \"job not started yet\" j))\n"
      "          (#t\n"
      /**          set job's process group as the foreground process group */
      "            (let ((ret (c-pgid-foreground (job-pgid j))))\n"
      "              (when (< ret 0)\n"
      "                (raise-errno-condition 'sh-fg ret)))\n"
      "            (dynamic-wind\n"
      "              void\n"       /* run before body */
      "              (lambda ()\n" /* body */
      /**              send SIGCONT to job's process group. may raise error */
      "                (pid-kill (fx- (job-pgid j)) 'sigcont)\n"
      /**              blocking wait for job's pid to exit or stop.
       *               TODO: wait for ALL pids in process group? */
      "                (job-wait j 'blocking))\n"
      /*             run after body, even if it raised a condition:
       *             restore sh-globals as the foreground process group */
      "              (lambda ()\n"
      "                (c-pgid-foreground (job-pgid sh-globals))))))))))\n"
      "\n"
      /**
       * Wait for a job or job-id to exit. Does NOT send SIGCONT to it in case it's already stopped,
       * and does NOT return if the job gets stopped.
       * Return job status, which can be one of:
       *
       *   (cons 'exited  exit-status)
       *   (cons 'killed  signal-name)
       *   (cons 'unknown ...)
       *
       * Note: upon invocation, sets the job as fg process group.
       * Before returning, restores sh-globals as fg process group.
       */
      "(define sh-wait\n"
      "  (let ((c-pgid-foreground (foreign-procedure \"c_pgid_foreground\" (int) int)))\n"
      "    (lambda (job-id)\n"
      "      (let ((j (sh-job-ref job-id)))\n"
      "        (cond\n"
      /*         if job already exited, return its exit status. */
      /*         if job is stopped, consider as running */
      "          ((job-status-member? (job-last-status j) '(exited killed unknown))\n"
      "            (job-last-status j))\n"
      "          ((not (job-started? j))\n"
      "            (error 'sh-wait \"job not started yet\" j))\n"
      "          (#t\n"
      /*           set job's process group as the foreground process group */
      "            (let ((ret (c-pgid-foreground (job-pgid j))))\n"
      "              (when (< ret 0)\n"
      "                (raise-errno-condition 'sh-wait ret)))\n"
      "            (dynamic-wind\n"
      "              void\n"       /* run before body */
      "              (lambda ()\n" /* body */
      /*              blocking wait for job's pid to exit. */
      /*              TODO: wait for ALL pids in process group? */
      "                (do ((status #f (job-wait j 'blocking)))\n"
      "                    ((job-status-member? status '(exited killed unknown)) status)))\n"
      /*             run after body, even if it raised a condition: */
      /*             restore sh-globals as the foreground process group */
      "              (lambda ()\n"
      "                (c-pgid-foreground (job-pgid sh-globals))))))))))\n"
      "\n"
      /**
       * Start a job and wait for it to exit or stop.
       * Options are the same as (sh-start)
       * Return job status, possible values are the same as (sh-fg)
       */
      "(define (sh-run j . options)\n"
      "  (apply sh-start j options)\n"
      "  (sh-fg j))\n"
      "\n"
      /** Create or remove a file description redirection for cmd or job */
      "(define (sh-fd-redirect! job-id child-fd existing-fd-or-minus-1)\n"
      "  (when (or (not (fixnum? child-fd)) (< child-fd 0))\n"
      "    (error 'job-redirect! \"invalid redirect fd\" child-fd))\n"
      "  (let* ((j (sh-job-ref job-id))\n"
      "         (old-fds (job-to-redirect-fds j))\n"
      "         (old-n (vector-length old-fds)))\n"
      "    (when (fx<=? old-n child-fd)\n"
      "      (let* ((new-n (max (+ 1 child-fd) (* 2 old-n)))\n"
      "             (new-fds (make-vector new-n -1)))\n" /* fill with -1 i.e. no redirection */
      "        (do ((i 0 (+ 1 i)))\n"
      "            ((>= i old-n))\n"
      "          (vector-set! new-fds i (vector-ref old-fds i)))\n"
      "        (job-to-redirect-fds-set! j new-fds)))\n"
      "    (vector-set! (job-to-redirect-fds j) child-fd existing-fd-or-minus-1)))\n"
      "\n"
      /** Create or remove multiple file description redirections for cmd or job */
      "(define (sh-fds-redirect! j child-fds existing-fd-or-minus-1)\n"
      "  (do ((child-cons child-fds (cdr child-cons)))\n"
      "      ((eq? '() child-cons))"
      "    (sh-fd-redirect! j (car child-cons) existing-fd-or-minus-1)))\n"
      "\n"
      /**
       * convert job-status to 8-bit exit status suitable for C function exit().
       * if job-status is '(exited . n) return n
       * if job-status is '(killed . signal_name) return 128 + signal_number
       * otherwise return 255
       */
      "(define (job-approx-exit-status job-status)\n"
      "  (if (pair? job-status)\n"
      "    (cond\n"
      "      ((eq? 'exited (car job-status))\n"
      "        (cdr job-status))\n"
      "      ((eq? 'killed (car job-status))\n"
      "        (fx+ 128 (signal-name->number (cdr job-status))))\n"
      "      (#t 255))\n" /* (car job-status) is 'new 'running 'stopped etc */
      "    255))\n"       /* job-status is not a cons */
      "\n"
      /**
       * Create a multijob to later start it.
       * Internal function, accepts an optional function to validate each element in children-jobs
       */
      "(define (make-multijob kind validate-job-proc subshell-func . children-jobs)\n"
      "  (assert (symbol? kind))\n"
      "  (assert (or (not subshell-func) (procedure? subshell-func)))\n"
      "  (when validate-job-proc\n"
      "    (list-iterate children-jobs validate-job-proc))\n"
      "  (%make-multijob -1 -1 '(new . 0) (vector 0 1 2) (vector) '()\n"
      "    subshell-func\n"
      "    '()\n"        /* overridden environment variables - initially none */
      "    sh-globals\n" /* parent job - initially the global job */
      "    kind\n"
      "    (list->span children-jobs)\n"
      "    0))\n"
      "\n"
      "(define (assert-is-job j)\n"
      "  (assert (sh-job? j)))\n"
      "\n"
      /** Create a multijob to later start it. Each argument must be a sh-job or subtype. */
      "(define (sh-multijob kind subshell-func . children-jobs)\n"
      "  (apply make-multijob kind assert-is-job subshell-func children-jobs))\n"
      "\n"
      /**
       * Run a multijob containing an "and" of children jobs.
       * Used by (sh-and), implements runtime behavior of shell syntax foo && bar && baz
       */
      "(define (%multijob-run-and mj)\n"
      "  (let ((jobs   (multijob-children mj))\n"
      "        (pgid   (job-pgid mj))\n"
      "        (status '(exited . 0)))\n"
      "    (span-iterate jobs\n"
      "      (lambda (i job)\n"
      "        (sh-start job pgid)\n"             /* run child job in parent's process group */
      "        (set! status (sh-wait job))\n"     /* wait for child job to exit              */
      "        (equal? status '(exited . 0))))\n" /* keep iterating only if job succeeded    */
      "    status))\n"
      "\n"
      /**
       * Run a multijob containing an "or" of children jobs.
       * Used by (sh-and), implements runtime behavior of shell syntax foo || bar || baz
       */
      "(define (%multijob-run-or mj)\n"
      "  (let ((jobs   (multijob-children mj))\n"
      "        (pgid   (job-pgid mj))\n"
      "        (status '(exited . 1)))\n"
      "    (span-iterate jobs\n"
      "      (lambda (i job)\n"
      "        (sh-start job pgid)\n"         /* run child job in parent's process group     */
      "        (set! status (sh-wait job))\n" /* wait for child job to exit                  */
      "        (not (equal? status '(exited . 0)))))\n" /* keep iterating only if job failed */
      "    status))\n"
      "\n"
      /**
       * Run a multijob containing children jobs separated by & |
       * Used by (sh-and-and), implements runtime behavior of shell syntax foo && bar || baz
       */
      "(define (%multijob-run-and-or mj)\n"
      /** TODO: check for && || among mj and implement them */
      "  (let ((jobs   (multijob-children mj))\n"
      "        (pgid   (job-pgid mj))\n"
      "        (status '(exited . 0)))\n"
      "    (span-iterate jobs\n"
      "      (lambda (i job)\n"
      "        (if (bitwise-bit-set? i 0)\n"
      "          (let ((success? (equal? status '(exited . 0))))\n"
      "            (case job\n"
      "              ((&&)           success?)\n"       /* keep iterating only if job succeeded */
      "              ((\\x7c;\\x7c;) (not success?))\n" /* keep iterating only if job failed */
      "              (else (assert (memq job '(&& \\x7c;\\x7c;))))))\n"
      "          (begin\n"
      "            (sh-start job pgid)\n"             /* run child job in parent's process group */
      "            (set! status (sh-wait job))))))\n" /* wait for child job to exit              */
      "    status))\n"
      "\n"
      /**
       * Run a multijob containing a sequence of children jobs.
       * Used by (sh-list), implements runtime behavior of shell syntax foo; bar; baz
       */
      "(define (%multijob-run-list mj)\n"
      "  (let ((jobs   (multijob-children mj))\n"
      "        (pgid   (job-pgid mj))\n"
      "        (status '(exited . 0)))\n"
      "    (span-iterate jobs\n"
      "      (lambda (i job)\n"
      "        (sh-start job pgid)\n"         /* run child job in parent's process group */
      "        (set! status (sh-wait job))\n" /* wait for child job to exit */
      "        #t))\n"                        /* keep iterating */
      "    status))\n"
      "\n"
      /**
       * Run a multijob containing a sequence of children jobs optionally followed by & ;
       * Used by (sh-list*), implements runtime behavior of shell syntax foo; bar & baz
       */
      "(define (%multijob-run-list* mj)\n"
      /** TODO: check for && || among mj and implement them */
      "  (%multijob-run-list mj))\n"
      "\n"
      "(define (sh-and . children-jobs)\n"
      "  (apply make-multijob 'and assert-is-job %multijob-run-and children-jobs))\n"
      "\n"
      "(define (sh-or . children-jobs)\n"
      "  (apply make-multijob 'or  assert-is-job %multijob-run-or  children-jobs))\n"
      "\n"
      /**
       * Odd arguments must be sh-job
       * Even arguments must be a symbol && ||
       */
      "(define (sh-and-or* . children-jobs-with-and-or)\n"
      "  (when (null? children-jobs-with-and-or)\n"
      "    (assertion-violation 'sh-and-or* \"requires at least one argument\" '()))\n"
      "  (let ((expect-job? #t))\n"
      "    (list-iterate children-jobs-with-and-or (lambda (j)\n"
      "      (if expect-job?\n"
      "        (unless (sh-job? j)\n"
      "          (assertion-violation 'sh-and-or* \"even arguments must be sh-job\" j))\n"
      "        (unless (memq j '(&& \\x7c;\\x7c;))\n"
      "            (assertion-violation 'sh-and-or* \"odd arguments must be one of the "
      "symbols && ||\" j)))\n"
      "      (set! expect-job? (not expect-job?))))\n"
      "    (when expect-job?\n"
      "      (assertion-violation 'sh-and-or* \"number of arguments must odd\" "
      "children-jobs-with-and-or)))\n"
      "  (apply make-multijob 'and-or #f %multijob-run-and-or children-jobs-with-and-or))\n"
      "\n"
      /** Each argument must be a sh-job */
      "(define (sh-list . children-jobs)\n"
      "  (apply make-multijob 'list assert-is-job %multijob-run-list children-jobs))\n"
      "\n"
      /** Each argument must be a sh-job, possibly followed by a symbol ; & */
      "(define (sh-list* . children-jobs-with-colon-ampersand)\n"
      "  (apply make-multijob 'list\n"
      "    (lambda (j)\n" // validate-job-proc
      "      (unless (memq j '(& \\x3b;))\n"
      "        (assert (sh-job? j))))\n"
      "    %multijob-run-list* children-jobs-with-colon-ampersand))\n"
      "\n"
      "(define (sh-run-capture-output job)\n"
      /** TODO: implement */
      "  \"\")\n"
      "\n"
      /** customize how "job" objects are printed */
      "(record-writer (record-type-descriptor job)\n"
      "  (lambda (obj port writer)\n"
      "    (display \"(sh-job \" port)\n"
      "    (writer (job-subshell-func obj) port)\n"
      "    (display #\\) port)))\n"
      "\n"
      /** customize how "cmd" objects are printed */
      "(record-writer (record-type-descriptor cmd)\n"
      "  (lambda (obj port writer)\n"
      "    (display \"(sh-cmd\" port)\n"
      "    (vector-iterate (cmd-argv obj)\n"
      "       (lambda (i arg)\n"
      "         (display #\\space port)\n"
      "         (write-bytevector0 arg port)))\n"
      "    (display #\\) port)))\n"
      "\n"
      /** customize how "multijob" objects are printed */
      "(record-writer (record-type-descriptor multijob)\n"
      "  (lambda (obj port writer)\n"
      "    (display \"(sh-\" port)\n"
      "    (display (multijob-kind obj) port)\n"
      "    (span-iterate (multijob-children obj)\n"
      "       (lambda (i child)\n"
      "         (display #\\space port)\n"
      "         (display child port)))\n"
      "    (display #\\) port)))\n"
      "\n"
      ")\n"); /* close library */
}

/**
 * Define the functions (sh) (sh-parse)
 *
 * Convention: (sh) and (sh-...) are functions
 *             (shell) and (shell-...) are macros
 */
static void schemesh_define_library_shell_parse(void) {

#define SCHEMESH_LIBRARY_SHELL_PARSE_EXPORT "sh sh-parse "

  eval("(library (schemesh shell parse (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_SHELL_PARSE_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (rnrs mutable-pairs)\n"
       "    (only (chezscheme) eval\n"
#ifdef SCHEMESH_LIBRARY_SHELL_PARSE_DEBUG
       "      format\n"
#endif
       "      remq! reverse!)\n"
       "    (only (schemesh bootstrap)       until)\n"
       "    (only (schemesh containers misc) list-iterate list-quoteq!)\n"
       "    (only (schemesh containers hashtable) eq-hashtable)\n"
       "    (schemesh shell jobs))\n"
       "\n"
       /** Return #t if token is a shell command separator: ; & && || | |& */
       "(define (sh-separator? token)\n"
       "  (and (symbol? token)\n"
       "       (memq token '(\\x3b; & && \\x7c;\\x7c; \\x7c; \\x7c;&))))\n"
       "\n"
       /** Return #t if token is a shell redirection operator: < > >> >& */
       "(define (sh-redirect-operator? token)\n"
       "  (and (symbol? token)\n"
       "       (memq token '(< > >> >&))))\n"
       "\n"
       /**
        * Parse args using shell syntax, and return corresponding sh-cmd or sh-multijob object.
        *
        * Each element in args must be a symbol, string, closure or pair:
        * 1. symbols are operators. Recognized symbols are: ; & && || | |& < > >> >&
        *    TODO: implement N>> N< etc.
        * 2. strings stand for themselves. for example (sh "ls" "-l")
        *    is equivalent to (sh-cmd "ls" "-l")
        * 3. closures must accept a single argument and return a string.
        *    TODO: implement support for them.
        * 4. pairs TBD
        */
       "(define (sh . args)\n"
       /* implementation: use sh-parse for converting shell commands to Scheme forms,
        * then (eval) such forms */
       "  (eval sh-parse args))\n"
       "\n"
       /**
        * Parse list containing a sequence of shell commands separated by ; & && || | |&
        * Return list containing parsed args.
        */
       "(define (sh-parse args)\n"
       "  (let ((saved-args args)\n"
       "        (ret '()))\n"
       "    (until (null? args)\n"
       "      (let-values (((parsed tail) (sh-parse-and-or args)))\n"
       "        (set! ret (cons parsed ret))\n"
       "        (set! args tail)\n"
#ifdef SCHEMESH_LIBRARY_SHELL_PARSE_DEBUG
       "        (format #t \"sh-parse          iterate: ret = ~s, args = ~s~%\"\n"
       "(reverse ret) args)\n"
#endif
       "        (cond\n"
       "          ((null? args) #f)\n"
       "          ((not (symbol? (car args)))\n"
       "            (set! ret (cons '\\x3b; ret)))\n"
       "          ((memq (car args) '(\\x3b; &))\n"
       "            (set! ret (cons (car args) ret))\n"
       "            (set! args (cdr args)))\n"
       "          (#t\n"
       "            (syntax-violation 'sh-parse \"syntax error, unknown shell operator:\"\n"
       "              saved-args (car args))))))\n"
#ifdef SCHEMESH_LIBRARY_SHELL_PARSE_DEBUG
       "    (format #t \"sh-parse           return: ret = ~s, args = ~s~%\"\n"
       "(reverse ret) args)\n"
#endif
       "    (cond\n"
       "      ((null? ret) '(sh-true))\n"
       "      ((null? (cdr ret)) (car ret))\n"
       "      (#t (cons 'sh-list* (reverse! (list-quoteq! '(& \\x3b;) ret)))))))\n"
       "\n"
       /**
        * Parse list containing a sequence of shell commands separated by && || | |&
        * Return two values:
        *   A list containing parsed args;
        *   The remaining, unparsed args.
        */
       "(define (sh-parse-and-or args)\n"
       "  (let ((ret '())\n"
       "        (done? (null? args))\n"
       "        (and? #f)\n"
       "        (or? #f))\n"
       "    (until done?\n"
       "      (let-values (((parsed tail) (sh-parse-pipe args)))\n"
       "        (set! ret (cons parsed ret))\n"
       "        (set! args tail))\n"
#ifdef SCHEMESH_LIBRARY_SHELL_PARSE_DEBUG
       "      (format #t \"sh-parse-and-or iterate: ret = ~s, args = ~s~%\" (reverse ret) args)\n"
#endif
       "      (cond\n"
       "        ((null? args) (set! done? #t))\n"
       "        ((memq (car args) '(&& \\x7c;\\x7c;))\n"
       "          (if (eq? '&& (car args))\n"
       "            (set! and? #t)\n"
       "            (set! or?  #t))\n"
       "          (set! ret (cons (car args) ret))\n"
       "          (set! args (cdr args))\n"
       "          (set! done? (null? args)))\n"
       "        (#t   (set! done? #t))))\n" /* unhandled token => exit loop */
#ifdef SCHEMESH_LIBRARY_SHELL_PARSE_DEBUG
       "    (format #t \"sh-parse-and-or  return: ret = ~s, args = ~s~%\" (reverse ret) args)\n"
#endif
       "    (values\n"
       "      (cond\n"
       "        ((null? ret) ret)\n"
       "        ((null? (cdr ret)) (car ret))\n"
       "        ((and and? or?)\n"
       "          (cons 'sh-and-or* (reverse! (list-quoteq! '(&& \\x7c;\\x7c;) ret))))\n"
       "        (and? (cons 'sh-and (reverse! (remq! '&& ret))))\n"
       "        (#t   (cons 'sh-or  (reverse! (remq! '\\x7c;\\x7c; ret)))))\n"
       "      args)))\n"
       "\n"
       /**
        * Parse list containing a sequence of shell commands separated by | |&
        * Return two values:
        *   A list containing parsed args;
        *   The remaining, unparsed args.
        */
       "(define (sh-parse-pipe args)\n"
       "  (let ((ret '())\n"
       "        (done? (null? args)))\n"
       "    (until done?\n"
       "      (let-values (((parsed tail) (sh-parse-cmd args)))\n"
       "        (set! ret (cons parsed ret))\n"
       "        (set! args tail))\n"
#ifdef SCHEMESH_LIBRARY_SHELL_PARSE_DEBUG
       "      (format #t \"sh-parse-pipe  iterate: ret = ~s, args = ~s~%\" (reverse ret) args)\n"
#endif
       "      (cond\n"
       "        ((null? args) (set! done? #t))\n"
       "        ((memq (car args) '(\\x7c; \\x7c;&))\n"
       "          (set! ret (cons (car args) ret))\n"
       "          (set! args (cdr args))\n"
       "          (set! done? (null? args)))\n"
       "        (#t   (set! done? #t))))\n" /* unhandled token => exit loop */
#ifdef SCHEMESH_LIBRARY_SHELL_PARSE_DEBUG
       "    (format #t \"sh-parse-pipe   return: ret = ~s, args = ~s~%\" (reverse ret) args)\n"
#endif
       "    (values\n"
       "      (cond\n"
       "        ((null? ret) ret)\n"
       "        ((null? (cdr ret)) (car ret))\n"
       "        (#t (cons 'sh-pipe* (reverse! (list-quoteq! '(\\x7c; \\x7c;&) ret)))))\n"
       "      args)))\n"
       "\n"
       /**
        * Parse args for a single shell command, i.e. everything before the first ; & && || | |&
        * Return two values:
        *   A list containing parsed args;
        *   The remaining, unparsed args.
        */
       "(define (sh-parse-cmd args)\n"
       "  (let ((saved-args args)\n"
       "        (ret '())\n"
       "        (redirections? #f)"
       "        (done? (null? args)))\n"
       "    (until done?\n"
#ifdef SCHEMESH_LIBRARY_SHELL_PARSE_DEBUG
       "      (format #t \"sh-parse-cmd iterate: ret = ~s, args = ~s~%\" (reverse ret) args)\n"
#endif
       "      (if (null? args)\n"
       "        (set! done? #t)\n"
       "        (let ((arg (car args)))\n"
       "          (cond\n"
       "            ((sh-separator? arg)\n"
       "              (set! done? #t))\n" /* separator => exit loop */
       "            ((or (sh-redirect-operator? arg) (pair? arg) (string? arg) (procedure? arg))\n"
       "              (when (sh-redirect-operator? arg)\n"
       "                (set! redirections? #t)\n"
       /*               quote redirection operator (a symbol) to use its name, not its value */
       "                (set! arg (list 'quote arg)))\n"
       "              (set! ret (cons arg ret))\n"
       "              (set! args (cdr args)))\n"
       "            (#t\n"
       "              (syntax-violation 'sh-parse \"syntax error, expecting a redirection "
       "operator, string, pair or procedure, found:\"\n"
       "                saved-args arg))))))\n"
#ifdef SCHEMESH_LIBRARY_SHELL_PARSE_DEBUG
       "    (format #t \"sh-parse-cmd  return: ret = ~s, args = ~s~%\" (reverse ret) args)\n"
#endif
       "    (values\n"
       "      (cons (if redirections? 'sh-cmd<> 'sh-cmd) (reverse! ret))\n"
       "      args)))\n"
       "\n"
       ")\n"); /* close library */
}

/**
 * Define the funcstion (sh-true) (sh-false) (sh-cd) (sh-pwd) etc.
 */
static void schemesh_define_library_shell_builtins(void) {

#define SCHEMESH_LIBRARY_SHELL_BUILTINS_EXPORT "sh-true sh-false sh-cd sh-pwd "

  eval("(library (schemesh shell builtins (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_SHELL_BUILTINS_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme) void)\n"
       "    (schemesh shell jobs))\n"
       "\n"
       "(define (sh-true . ignored-args)\n"
       "  (void))\n" /** TODO: implement */
       "\n"
       "(define (sh-false . ignored-args)\n"
       "  (void))\n" /** TODO: implement */
       "\n"
       "(define (sh-cd path)\n"
       "  (void))\n" /** TODO: implement */
       "\n"
       "(define (sh-pwd . ignored-args)\n"
       "  (void))\n" /** TODO: implement */
       "\n"
       ")\n"); /* close library */
}
/**
 * Define the macros (shell) (shell-backquote) etc.
 *
 * Convention: (sh) and (sh-...) are functions
 *             (shell) and (shell-...) are macros
 */
static void schemesh_define_library_shell_macros(void) {

#define SCHEMESH_LIBRARY_SHELL_MACROS_EXPORT "shell shell-list shell-backquote "

  eval("(library (schemesh shell macros (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_SHELL_MACROS_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
#ifdef SCHEMESH_LIBRARY_SHELL_PARSE_DEBUG
       "    (only (chezscheme) format)\n"
#endif
       "    (schemesh bootstrap)\n"
       "    (schemesh shell jobs)\n"
       "    (schemesh shell parse))\n"
       "\n"
       "(define-macro (shell . args)\n"
       "  (let ((ret (sh-parse args)))\n"
#ifdef SCHEMESH_LIBRARY_SHELL_PARSE_DEBUG
       "    (format #t \"; expanded to: ~s~%\" ret)\n"
#endif
       "    ret))\n"
       "\n"
       "(define-syntax shell-list\n"
       "  (syntax-rules ()\n"
       "    ((_)           '(sh-true))\n"
       "    ((_ arg)          arg)\n"
       "    ((_ arg0 arg1 ...) (sh-list arg0 arg1 ...))))\n"
       "\n"
       "(define-syntax shell-backquote\n"
       "  (syntax-rules ()\n"
       "    ((_)           \"\")\n"
       "    ((_ arg)         (sh-run-capture-output arg))\n"
       "    ((_ arg0 arg1 ...) (sh-run-capture-output (sh-list arg0 arg1 ...)))))\n"
       "\n"
       ")\n"); /* close library */
}

static void schemesh_define_library_shell(void) {
  schemesh_define_library_shell_jobs();
  schemesh_define_library_shell_builtins();
  schemesh_define_library_shell_parse();
  schemesh_define_library_shell_macros();

  eval("(library (schemesh shell (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_SHELL_JOBS_EXPORT ""
       /*        */ SCHEMESH_LIBRARY_SHELL_BUILTINS_EXPORT ""
       /*        */ SCHEMESH_LIBRARY_SHELL_PARSE_EXPORT ""
       /*        */ SCHEMESH_LIBRARY_SHELL_MACROS_EXPORT ")\n"
       "  (import\n"
       "    (schemesh shell jobs)\n"
       "    (schemesh shell builtins)\n"
       "    (schemesh shell parse)\n"
       "    (schemesh shell macros)))\n");
}

int schemesh_define_libraries(void) {
  int err;

  schemesh_define_library_bootstrap();
  schemesh_define_library_containers();
  schemesh_define_library_conversions();
  schemesh_define_library_io();
  schemesh_define_library_parser();

  if ((err = schemesh_define_library_fd()) < 0) {
    return err;
  }
  schemesh_define_library_signal();
  schemesh_define_library_tty();
  schemesh_define_library_pid();
  schemesh_define_library_lineedit();
  schemesh_define_library_shell();
  schemesh_define_library_repl();

  return err;
}

void schemesh_import_libraries(void) {
  eval("(begin\n"
       "  (import (schemesh bootstrap))\n"
       "  (import (schemesh containers))\n"
       "  (import (schemesh conversions))\n"
       "  (import (schemesh io))\n"
       "  (import (schemesh parser))\n"
       "  (import (schemesh fd))\n"
       "  (import (schemesh signal))\n"
       "  (import (schemesh tty))\n"
       "  (import (schemesh pid))\n"
       "  (import (schemesh lineedit))\n"
       "  (import (schemesh shell))\n"
       "  (import (schemesh repl)))\n");

  /* requires (import (schemesh shell)) */
  c_environ_to_sh_env(environ);
}

void schemesh_init(void (*on_scheme_exception)(void)) {
  Sscheme_init(on_scheme_exception);
  Sregister_boot_file(CHEZ_SCHEME_DIR_STR "/petite.boot");
  Sregister_boot_file(CHEZ_SCHEME_DIR_STR "/scheme.boot");
  Sbuild_heap(NULL, NULL);
}

void schemesh_quit(void) {
  Sscheme_deinit();
}
