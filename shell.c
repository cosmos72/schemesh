/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "shell.h"
#include "eval.h"

#include <string.h>

void define_job_functions(void) {

  /** Define the record type "job" */
  eval("(define-record-type\n"
       "  (job %make-job sh-job?)\n"
       "  (fields\n"
       "    (mutable pid)\n"               /* fixnum, -1 if unknown */
       "    (mutable exit-status)\n"       /* fixnum, -1 if unknown */
       "    (mutable to-redirect-fds)\n"   /* vector of fds to redirect between fork() and
                                              (start-func) */
       "    (mutable to-redirect-files)\n" /* vector of files to open before fork() */
       "    (mutable to-close-fds)\n"      /* list of fds to close after spawn */
       "    start-func\n"                  /* function to start the job in fork()ed child */
       "    (mutable env)\n"               /* overridden env variables, or '() */
       "    (mutable parent))))\n");       /* parent job, contains default values of env variables
                                            * and default redirections */

  /** Define the record type "cmd" */
  eval("(define-record-type\n"
       "  (cmd %make-cmd sh-cmd?)\n"
       "  (parent job)"
       "  (fields\n"
       "    argv))\n"); /* vector of bytevectors, each #\nul terminated */

  /** customize how "job" objects are printed */
  eval("(record-writer (record-type-descriptor job)\n"
       "  (lambda (obj port writer)\n"
       "    (display \"(sh-job \" port)\n"
       "    (writer (job-start-func obj) port)\n"
       "    (display #\\) port)))\n");

  /** customize how "cmd" objects are printed */
  eval("(record-writer (record-type-descriptor cmd)\n"
       "  (lambda (obj port writer)\n"
       "    (display \"(sh-cmd\" port)\n"
       "    (vector-for-each\n"
       "       (lambda (arg)\n"
       "         (display #\\space port)\n"
       "         (write-bytevector0 arg port))\n"
       "       (cmd-argv obj))\n"
       "    (display #\\) port)))\n");

  /**
   * Define the variable sh-globals, contains global job.
   * May be set! to a different value in subshells.
   */
  eval("(define sh-globals\n"
       "  (%make-job -1 -1 (vector 0 1 2) (vector) '() #f\n"
       "             (make-hashtable string-hash string=?) #f))\n");

  /**
   * Define the function (sh-get-job), converts job-id to job.
   * Job-id can be either a job, the empty list '() which means sh-globals,
   * or a fixnum (TODO: implement) which means one of the running jobs.
   */
  eval("(define (sh-get-job job-id)\n"
       "  (assert (or (null? job-id) (sh-job? job-id)))\n"
       "  (if (null? job-id) sh-globals job-id))\n");

  /**
   * Define the function (job-parents-iterate), calls (proc j) on given job and each of its parents.
   * Stops iterating if (proc) returns #f.
   */
  eval("(define (job-parents-iterate job-id proc)\n"
       "  (do ((parent (sh-get-job job-id) (job-parent parent)))\n"
       "      ((or (not (sh-job? parent)) (not (proc parent))))))\n");

  /**
   * Define the function (job-parents-list), returns list containing all job's parents,
   * starting from sh-globals, until job itself.
   */
  eval("(define (job-parents-revlist job-id)\n"
       "  (let ((jlist '()))\n"
       "    (job-parents-iterate job-id\n"
       "      (lambda (job)\n"
       "        (set! jlist (cons job jlist))))\n"
       "    jlist))\n");

  /**
   * Define the function (job-parents-list), returns list containing job
   * followed by all its parents.
   */
  eval("(define (job-parents-list job-id)\n"
       "  (reverse! (job-parents-revlist job-id)))\n");

  /** Create a cmd to later spawn it. */
  eval("(define (sh-cmd program . args)\n"
       "  (%make-cmd -1 -1 (vector 0 1 2) (vector) '()\n"
       "    #f\n"         /* start-func */
       "    '()\n"        /* overridden environment variables - initially none */
       "    sh-globals\n" /* parent job - initially the global job */
       "    (list->cmd-argv (cons program args))))\n");
}

void define_env_functions(void) {
  /** return global environment variables */
  eval("(define (sh-global-env)\n"
       "  (job-env sh-globals))\n");

  /**
   * Return direct environment variables of job, creating them if needed.
   * Returned hashtable does not include default variables,
   * i.e. the ones inherited from parent jobs.
   */
  eval("(define (job-direct-env job-id)\n"
       "  (let* ((job (sh-get-job job-id))\n"
       "         (vars (job-env job)))\n"
       "    (unless (hashtable? vars)\n"
       "      (set! vars (make-hashtable string-hash string=?))\n"
       "      (job-env-set! job vars))\n"
       "    vars))\n");

  /**
   * Return a copy of job's environment variables,
   * including default variables inherited from parent jobs.
   * If all? is #t, unexported variables are returned too.
   */
  eval("(define (job-flatten-env job-id all?)\n"
       "  (assert (boolean? all?))\n"
       "  (let ((jlist (job-parents-revlist job-id))\n"
       "        (vars (make-hashtable string-hash string=?)))\n"
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
       "                         (and (not all?) (eq? 'private flag)))\n"
       "                      (hashtable-delete! vars name))\n"
       "                    ((or (eq? 'export flag)\n"
       "                         (and all? (eq? 'private flag)))\n"
       "                      (hashtable-set! vars name val))))))))))\n"
       "    vars))\n");

  /**
   * Return environment variable named "name" of specified job.
   * If name is not found in job's environment, also search in environment
   * inherited from parent jobs.
   */
  eval("(define (sh-env-get job-id name)\n"
       "  (let ((ret \"\"))\n"
       "    (job-parents-iterate job-id\n"
       "      (lambda (j)\n"
       "        (let* ((vars (job-env j))\n"
       "               (elem (if (hashtable? vars) (hashtable-ref vars name #f) #f)))\n"
       "          (when (pair? elem)\n"
       "            (set! ret (cdr elem))\n"
       "            #f))))\n" /* name found, stop iterating */
       "    ret))\n");
  eval("(define (sh-env-set! job-id name val)\n"
       "  (let* ((vars (job-direct-env job-id))\n"
       "         (elem (hashtable-ref vars name #f)))\n"
       "    (if (pair? elem)\n"
       "      (set-cdr! elem val)\n"
       "      (hashtable-set! vars name (cons 'private val)))))\n");
  /**
   * Note: (sh-env-unset!) inserts an entry that means "deleted",
   * in order to override any parent job's environment variable
   * with the same name.
   */
  eval("(define (sh-env-unset! job-id name)\n"
       "  (let ((vars (job-direct-env job-id)))\n"
       "    (hashtable-set! vars name (cons 'delete \"\"))))\n");
  eval("(define (sh-env-exported? job-id name)\n"
       "  (let ((ret #f))\n"
       "    (job-parents-iterate job-id\n"
       "      (lambda (j)\n"
       "        (let* ((vars (job-env j))\n"
       "               (elem (if (hashtable? vars) (hashtable-ref vars name #f) #f)))\n"
       "          (when (pair? elem)\n"
       "            (set! ret (eq? 'export (car elem)))\n"
       "            #f))))\n" /* name found, stop iterating */
       "    ret))\n");
  eval("(define (sh-env-export! job-id name exported?)\n"
       "  (assert (boolean? exported?))\n"
       "  (let* ((j (sh-get-job job-id))\n"
       "          ;\n" /* val may be in a parent environment */
       "         (val (sh-env-get j name))\n"
       "         (export (if exported? 'export 'private)))\n"
       "    ;\n" /* (job-direct-env j) creates job environment if not yet present */
       "    (hashtable-set! (job-direct-env j) name (cons export val))))))\n");

  /**
   * Extract environment variables from specified job and all its parents,
   * and convert them to a vector of bytevector0.
   * If all? is #t, unexported variables are returned too.
   */
  eval("(define (sh-env->vector-of-bytevector0 job-id all?)\n"
       "  (string-hashtable->vector-of-bytevector0 (job-flatten-env job-id all?)))\n");
}

void c_environ_to_sh_env(char** env) {
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
    call3("sh-env-set!", Snil, Sstring_of_length(entry, inamelen), Sstring(separator + 1));
    call3("sh-env-export!", Snil, Sstring_of_length(entry, inamelen), Strue);
  }
}

void define_shell_functions(void) {

  eval("(define (cmd-start-options->existing-pgid options)\n"
       "  (let ((existing-pgid 0))\n"
       "    (list-iterate options\n"
       "      (lambda (option)\n"
       "        (when (fixnum? option)\n"
       "          (set! existing-pgid option)\n"
       "          #f)))\n" // stop iterating on options
       "    existing-pgid))\n");

  eval("(define (cmd-start-options->c-spawn-options options)\n"
       "  (let ((c-spawn-options 0))\n"
       "    (list-iterate options\n"
       "      (lambda (option)\n"
       "        (cond\n"
       "          ((eq? 'fg option) (set! c-spawn-options (logior c-spawn-options 2)))\n"
       "          ((fixnum? option) (set! c-spawn-options (logior c-spawn-options 1))))))\n"
       "    c-spawn-options))\n");

  /**
   * Start a cmd,
   * optionally in foreground (prevents this shell from doing tty I/O until the process finishes!)
   * and optionally insert the new process into an existing process group.
   *
   * Options can be zero or more of:
   *   'fg to start the cmd in foreground
   *   a fixnum to insert the new process into the corresponding process group id.
   */
  eval("(define cmd-start\n"
       "  (let ((c-spawn-pid (foreign-procedure \"c_spawn_pid\""
       "                        (scheme-object scheme-object scheme-object int int) int)))\n"
       "    (lambda (j . options)\n"
       "      (let ((ret (c-spawn-pid\n"
       "                   (cmd-argv j)\n"
       "                   (job-to-redirect-fds j)\n"
       "                   (sh-env->vector-of-bytevector0 j #f)\n"
       "                   (cmd-start-options->existing-pgid options)\n"
       "                   (cmd-start-options->c-spawn-options options))))\n"
       "        (when (< ret 0)\n"
       "          (raise-errno-condition 'sh-start ret))\n"
       "        (fd-close-list (job-to-close-fds j))\n"
       "        (job-pid-set! j ret)\n"
       "        (job-exit-status-set! j -1)))))\n"); /* job can now be waited-for */

  /** Start a cmd or a job. TODO: implement starting a job */
  eval("(define (sh-start j . options)\n"
       "  (when (>= (job-pid j) 0)\n"
       "    (error 'sh-start \"job already started\" (job-pid j)))\n"
       "  (when (not (sh-cmd? j))\n"
       "    (error 'sh-start \"unimplemented for non-cmd jobs\"))\n"
       "  (apply cmd-start j options))\n");

  /**
   * Wait for a cmd or job to exit and return its exit status, or 256 + signal
   * Warning: does not set the job as foreground process group,
   * consider calling (sh-fg) instead.
   */
  eval("(define (job-wait j)\n"
       "  (cond\n"
       "    ((>= (job-exit-status j) 0)\n"
       "      (job-exit-status j))\n" /* already waited for */
       "    ((< (job-pid j) 0)\n"
       "      (error 'job-wait \"job not started yet\" j))\n"
       "    (#t\n"
       "      (let ([ret (pid-wait (job-pid j))])\n"
       "        (when (< ret 0)\n"
       "          (raise-errno-condition 'job-wait ret))\n"
       "        (job-pid-set! j -1)\n" /* cmd can now be spawned again */
       "        (job-exit-status-set! j ret)\n"
       "        ret))))\n");

  /** Start a cmd or job and wait for it to exit. return its exit status, or 256 + signal */
  eval("(define (sh-run j)\n"
       "  (sh-start j 'fg)\n"
       "  (job-wait j))\n");

  /** Create or remove a file description redirection for cmd or job */
  eval("(define (sh-redirect-fd! j child-fd existing-fd-or-minus-1)\n"
       "  (when (or (not (fixnum? child-fd)) (< child-fd 0))\n"
       "    (error 'job-redirect! \"invalid redirect fd\" child-fd))\n"
       "  (let* ([old-fds (job-to-redirect-fds j)]\n"
       "         [old-n (vector-length old-fds)])\n"
       "    (when (<= old-n child-fd)\n"
       "      (let* ([new-n (max (+ 1 child-fd) (* 2 old-n))]\n"
       "             [new-fds (make-vector new-n -1)])\n" /* fill with -1 i.e. no redirection */
       "        (do ([i 0 (+ 1 i)])\n"
       "            ((>= i old-n))\n"
       "          (vector-set! new-fds i (vector-ref old-fds i)))\n"
       "        (job-to-redirect-fds-set! j new-fds))))\n"
       "  (vector-set! (job-to-redirect-fds j) child-fd existing-fd-or-minus-1))\n");

  /** Create or remove multiple file description redirections for cmd or job */
  eval("(define (sh-redirect-fds! j child-fds existing-fd-or-minus-1)\n"
       "  (do ([child-cons child-fds (cdr child-cons)])\n"
       "      ((eq '() child-cons))"
       "    (job-redirect-fd! j (car child-cons) existing-fd-or-minus-1)))\n");
}
