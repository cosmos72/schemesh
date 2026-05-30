#!/usr/bin/env schemesh

find -type f -name '*.ss' -ls

$(format #t
  "\nrunning inside $():       \n\
   tty-job-control?           ~s\n\
   tty-job-control-available? ~s\n\
   sh-current-job            ~s\n\
   current-output-port       ~s\n"
   (tty-job-control?) (tty-job-control-available?) (sh-current-job) (current-output-port))

#!scheme
(format #t
  "\nrunning inside ():        \n\
   tty-job-control?           ~s\n\
   tty-job-control-available? ~s\n\
   sh-current-job            ~s\n\
   current-output-port       ~s\n\n"
   (tty-job-control?) (tty-job-control-available?) (sh-current-job) (current-output-port))
