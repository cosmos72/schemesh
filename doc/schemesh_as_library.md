## loading schemesh as library from plain Chez Scheme

Works at least on Linux.
Instructions to compile C shared libraries may differ on other systems.

```shell
make clean
make -j CC='cc -fPIC'
cc -shared -o libschemesh_c_0.9.0.so containers.o eval.o posix.o shell.o
```
then, from Chez Scheme REPL:
```lisp
(load-shared-object "./libschemesh_c_0.9.0.so")
((foreign-procedure "schemesh_register_c_functions" () int)) ; should return 0
(load "./libschemesh_0.9.0.so")
(import (schemesh))
(repl) ; optional, user can also continue with Chez Scheme REPL
```
