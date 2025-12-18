## Loading schemesh as a library from plain Chez Scheme

Works at least on Linux.<br/>
On other systems, the commands for compiling a C shared library may differ.

First, download schemesh following the [build instructions](../README.md#build-instructions) for your system.<br/>
For example, on Debian Linux one would do:
```shell
sudo apt update
sudo apt install build-essential chezscheme-dev liblz4-dev libncurses-dev git uuid-dev zlib1g-dev
git clone https://github.com/cosmos72/schemesh
cd schemesh
```

Then compile schemesh as a pair of libraries: a C shared library, and a scheme one.<br/>
The following commands work at least on Linux, on other systems they may differ.
```shell
make clean
make -j schemesh_so chez_batteries_c_so
```

Finally, from Chez Scheme REPL:
```lisp
(load-shared-object "./libchez_batteries_c_0.9.2.so")
((foreign-procedure "schemesh_register_c_functions" () int)) ; should return 0
(load "./libschemesh_0.9.2.so")
(import (schemesh))
(repl) ; optional, user can also continue with Chez Scheme REPL
```
