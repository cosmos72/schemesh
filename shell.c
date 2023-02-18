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
       "  (job %make-job job?)\n"
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
       "  (cmd %make-cmd cmd?)\n"
       "  (parent job)"
       "  (fields\n"
       "    argv))\n"); /* vector of bytevectors, each #\nul terminated */

  /** Define the function (sh-globals), returns global job */
  eval("(define sh-globals\n"
       "  (let ((j (%make-job -1 -1 (vector 0 1 2) (vector) '() #f\n"
       "                      (make-hashtable string-hash string=?) #f)))\n"
       "    (lambda ()\n"
       "      j)))\n");

  /**
   * Define the function (job-parents-iterate), calls (proc j) on given job and each of its parents.
   * Stops iterating if (proc) returns false.
   */
  eval("(define (job-parents-iterate j proc)\n"
       "  (do ((parent (if (null? j) (sh-globals) j) (job-parent parent)))\n"
       "      ((or (not (job? parent)) (not (proc parent))))))\n");

  /** Create a cmd to later spawn it. */
  eval("(define (sh-cmd program . args)\n"
       "  (%make-cmd -1 -1 (vector 0 1 2) (vector) '()\n"
       "    #f\n"           /* start-func */
       "    '()\n"          /* overridden environment variables - initially none */
       "    (sh-globals)\n" /* parent job - initially the global job */
       "    (list->cmd-argv (cons program args))))\n");
}

void define_env_functions(void) {
  /** return global environment variables */
  eval("(define (sh-global-env)\n"
       "  (job-env (sh-globals)))\n");
  /** return environment variables of specified job, creating them if needed */
  eval("(define (sh-env job)\n"
       "  (if (null? job)\n"
       "    (sh-global-env)\n"
       "    (let ((vars (job-env job)))\n"
       "      (unless (hashtable? vars)\n"
       "        (set! vars (make-hashtable string-hash string=?))\n"
       "        (job-env-set! job vars))\n"
       "      vars)))\n");
  /**
   * return environment variable named "name" of specified job.
   * If name is not found in job's environment, also search in job parents environment
   */
  eval("(define (sh-env-get job name)\n"
       "  (let ((ret \"\"))\n"
       "    (job-parents-iterate job\n"
       "      (lambda (j)\n"
       "        (let* ((vars (job-env j))\n"
       "               (elem (if (hashtable? vars) (hashtable-ref vars name #f) #f)))\n"
       "          (when (pair? elem)\n"
       "            (set! ret (cdr elem))\n"
       "            #f))))\n" /* name found, stop iterating */
       "    ret))\n");
  eval("(define (sh-env-set! job name val)\n"
       "  (let* ((vars (sh-env job))\n"
       "         (elem (hashtable-ref vars name #f)))\n"
       "    (if (pair? elem)\n"
       "      (set-cdr! elem val)\n"
       "      (hashtable-set! vars name (cons #f val)))))\n");
  /**
   * FIXME: (sh-env-unset!) must insert an entry that means "deleted"
   * Current implementation is buggy: it exposes job parent's environment variable
   * with the same name, if any.
   */
  eval("(define (sh-env-unset! job name)\n"
       "  (let ((vars (sh-env job)))\n"
       "    (hashtable-delete! vars name)))\n");
  eval("(define (sh-env-exported? job name)\n"
       "  (let ((ret \"\"))\n"
       "    (job-parents-iterate job\n"
       "      (lambda (j)\n"
       "        (let* ((vars (job-env j))\n"
       "               (elem (if (hashtable? vars) (hashtable-ref vars name #f) #f)))\n"
       "          (when (pair? elem)\n"
       "            (set! ret (car elem))\n"
       "            #f))))\n" /* name found, stop iterating */
       "    ret))\n");
  eval("(define (sh-env-export! job name exported?)\n"
       "  (assert (boolean? exported?))\n"
       "  (let* ((j (if (null? job) (sh-globals) job))\n"
       "         (vars (job-env j))\n"
       "         (elem (if (hashtable? vars) (hashtable-ref vars name #f) #f)))\n"
       "    (if (pair? elem)\n"
       "      ;\n" /* job enviroment contains name: simply mark it exported */
       "      (set-car! elem exported?)\n"
       "      ;\n" /* job enviroment does not contain name: search in parent environments */
       "      (let ((value (sh-env-get j name)))\n"
       "        ;\n" /* (sh-env j) creates job environment if not yet present */
       "        (hashtable-set! (sh-env j) name (cons exported? value))))))\n");
  /**
   * FIXME: replace with a function that counts env variables from job and its parents
   */
  eval("(define (sh-env-size vars all?)\n"
       "  (assert (boolean? all?))\n"
       "  (let ((vars (if (null? vars) (sh-global-env) vars)))\n"
       "    (if all?\n"
       "      (hashtable-size vars)\n"
       "      (let ((n 0))\n"
       "        (hashtable-iterate vars\n"
       "          (lambda (cell)\n"
       "            (when (cadr cell)\n"
       "              (set! n (fx1+ n)))))\n"
       "        n))))\n");
  /**
   * FIXME: replace with a function that extracts env variables from a job and its parents
   */
  eval("(define (sh-env->vector-of-bytevector0 vars all?)\n"
       "  (let* ((vars (if (null? vars) (sh-global-env) vars))\n"
       "         (i 0)\n"
       "         (n (sh-env-size vars all?))\n"
       "         (out (make-vector n #f)))\n"
       "    (hashtable-iterate vars\n"
       "      (lambda (cell)\n"
       "        (let ((key (car cell))\n"
       "              (val (cdr cell)))\n"
       "          (when (or all? (car val))\n"
       "            (vector-set! out i (any->bytevector0 key \"=\" (cdr val)))\n"
       "            (set! i (fx1+ i))))))\n"
       "    out))\n");
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

  /** Start a cmd in a subprocess TODO: also support starting a job in a subprocess */
  eval("(define sh-start\n"
       "  (let ((c-spawn-pid (foreign-procedure \"c_spawn_pid\""
       "                        (scheme-object scheme-object scheme-object) int)))\n"
       "    (lambda (j)\n"
       "      (when (>= (job-pid j) 0)\n"
       "        (error 'sh-start \"job already started\" (job-pid j)))\n"
       "      (when (not (cmd? j))\n"
       "        (error 'sh-start \"unimplemented for non-cmd jobs\"))\n"
       "      (let ((ret (c-spawn-pid\n"
       "                   (cmd-argv j)\n"
       "                   (job-to-redirect-fds j)\n"
       "                   (sh-env->vector-of-bytevector0 (job-env j) #f))))\n"
       "        (when (< ret 0)\n"
       "          (raise-errno-condition 'sh-start ret))\n"
       "        (fd-close-list (job-to-close-fds j))\n"
       "        (job-pid-set! j ret)\n"
       "        (job-exit-status-set! j -1)))))\n"); /* job can now be waited-for */

  /** Wait for a cmd or job to exit and return its exit status, or 256 + signal */
  eval("(define (sh-wait j)\n"
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
       "  (sh-start j)\n"
       "  (sh-wait j))\n");

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
