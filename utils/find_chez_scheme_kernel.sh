#!/bin/sh
DIR="$1"
if [ "x$DIR" = "x" ]; then
  echo "Usage: $0 CHEZ_SCHEME_DIR" 1>&2
  exit 1
elif [ -r "$DIR/libkernel.a" ]; then
  echo "-lkernel"
elif [ -r "$DIR/kernel.o" ]; then
  echo "$DIR/kernel.o"
else
  echo "Cannot find libkernel.a or kernel.o in Chez Scheme installation directory '$DIR'" 1>&2
  exit 1
fi
