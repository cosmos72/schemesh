## unexpected places
programs that start the login shell to run a program,
instead of directly running the desired program:

* scp runs "$SHELL -c ../ftp-server..."
* gdb runs "$SHELL ? TARGET_PROGRAM_AND_ARGS"
