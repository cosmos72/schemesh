#!/bin/sh
chezscheme --verbose < /dev/null 2>&1 | grep 'scheme.boot...opened' | sed -e 's,^trying ,,g' -e 's,/scheme.boot...opened,,g' || \
    scheme --verbose < /dev/null 2>&1 | grep 'scheme.boot...opened' | sed -e 's,^trying ,,g' -e 's,/scheme.boot...opened,,g'
