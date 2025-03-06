
# test file read by (sh-read-file) in tests executed by test.c
# contains some random shell commands that are only read, not executed

BAR=
foo "a b" c | bar $BAR && { echo `baz --quiet` </dev/null 2>&1 || fail --verbose }

#!scheme
(set! a 42)
