#!/bin/sh
echo "----------- lsb_release -a --------------"
lsb_release -a
echo "----------- uname -a --------------------"
uname -a
echo "----------- make --version --------------"
( make --version || gmake --version ) 2>/dev/null
echo "----------- cc --version ----------------"
cc --version
echo "----------- chezscheme --verbose --------"
( chez-scheme --verbose || chezscheme --verbose || chez --verbose || scheme --verbose ) </dev/null >/dev/null
echo "----------- git log -1 ------------------"
git log -1
