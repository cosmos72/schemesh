## unexpected places that require a shell
programs that start the login shell to run a program,
instead of directly running the desired program:

* scp runs "$SHELL -c ../ftp-server..."
* gdb runs "$SHELL ? TARGET_PROGRAM_AND_ARGS"


## loading schemesh repl from vanilla Chez Scheme

Works at least on Linux.
Instructions to compile C shared libraries may differ on other systems.

```
make clean
make -j CC='cc -fPIC'
cc -shared -o libschemesh_c_0.7.6.so containers.o eval.o posix.o shell.o
scheme
```
then, from Chez Scheme repl:
```
(load-shared-object "./libschemesh_c_0.7.6.so")
((foreign-procedure "schemesh_register_c_functions" () int)) ; should return 0
(load "./libschemesh_0.7.6.so")
(import (schemesh))
(repl)
```
