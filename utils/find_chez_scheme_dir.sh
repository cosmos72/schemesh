#!/bin/sh
( chezscheme --verbose < /dev/null 2>&1 || scheme --verbose < /dev/null 2>&1 || chez --verbose < /dev/null 2>&1 ) \
  | grep -E '(chez|scheme)\.boot\.\.\.opened' \
  | sed -e 's,^trying ,,g' -e 's,/chezscheme.boot...opened,,g' -e 's,/scheme.boot...opened,,g' -e 's,/chez.boot...opened,,g'
