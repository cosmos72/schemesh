
# test file read by (sh-read-file) in tests executed by test.c
# contains some random shell commands that are never executed

BAR=
foo "a b" c | bar $BAR && { echo `baz --quiet` </dev/null 2>&1 || fail --verbose }
