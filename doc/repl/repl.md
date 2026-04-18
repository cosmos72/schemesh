# repl customization


## schemesh repl

The library `(schemesh repl)` which is also included in `(schemesh)`, contains the functions that implement REPL and also allow some customization.


### Alphabetical index

* [`repl-after-eval`](#repl-after-eval) parameter, contains user-defined function called after every `(repl-eval)`
* [`repl-before-eval`](#repl-before-eval) parameter, contains user-defined function called before every `(repl-eval)`
* [`repl-before-parse`](#repl-before-parse) parameter, contains user-defined function called before every `(repl-parse)`

Other functions are not (yet) documented.

### Functions

#### repl-after-eval
Syntax: `(repl-after-eval proc)`
<br/>Added in 1.0.1

`proc` must be a function accepting a single argument.<br/>
It will be called after every `(repl-eval)` and receives one argument: the list of values produced by `(repl-eval)`.<br/>
It must **not** modify the received list, and should not throw exceptions.

Default value: `nop`


#### repl-before-eval
Syntax: `(repl-before-eval proc)`
<br/>Added in 1.0.1

`proc` must be a function accepting a single argument.<br/>
It will be called before every `(repl-eval)` and receives one argument: the (**possibly** annotated) list of source forms to be evaluated.<br/>
It must **not** modify the received object, and should not throw exceptions.

See [Chez Scheme user guide: Section 11.11](https://cisco.github.io/ChezScheme/csug/syntax.html#./syntax:h11)
for handling annotations and extracting vanilla source forms from them.

Short summary: for extracting the list of source forms, use something like `(if (annotation? arg) (annotation-stripped arg) arg)`

Default value: `nop`


#### repl-before-parse
Syntax: `(repl-before-parse proc)`
<br/>Added in 1.0.1

`proc` must be a function accepting a single argument.<br/>
It will be called before every `(repl-parse)` and receives one argument: the user-typed string to be parsed and evaluated.<br/>
It must **not** modify the received string, and should not throw exceptions.

Default value: `nop`



### Examples:

```scheme
(repl-before-parse (lambda (x) (printf "# repl-before-parse ~s\n" x)))
(repl-before-eval (lambda (x) (printf "# repl-before-eval ~s\n" x)))
(repl-after-eval (lambda (x) (printf "# repl-after-eval ~s\n" x)))
ls srfi/
```
produces as output:
```shell
# repl-before-parse "ls srfi/"
# repl-before-eval (shell "ls" "srfi/")
18-multithreading.ss
# repl-after-eval (#<void>)
```

To unregister the user-defined functions and restore the default behavior, execute the following:
```scheme
(repl-before-parse nop)
(repl-before-eval nop)
(repl-after-eval nop)
```
