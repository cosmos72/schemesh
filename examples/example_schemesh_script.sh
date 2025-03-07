#!/usr/bin/env schemesh

find -type f -name '*.ss' -ls

$(format #t
  "\nrunning inside $():       \n\
   sh-job-control?           ~s\n\
   sh-job-control-available? ~s\n\
   sh-current-job            ~s\n\
   current-output-port       ~s\n"
   (sh-job-control?) (sh-job-control-available?) (sh-current-job) (current-output-port))

#!scheme
(format #t
  "\nrunning inside ():        \n\
   sh-job-control?           ~s\n\
   sh-job-control-available? ~s\n\
   sh-current-job            ~s\n\
   current-output-port       ~s\n\n"
   (sh-job-control?) (sh-job-control-available?) (sh-current-job) (current-output-port))
