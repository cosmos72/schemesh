# repl customization


## schemesh repl

The library `(schemesh repl)` which is also included in `(schemesh)`, contains the functions that implement REPL and also allow some customization.


### Alphabetical index

* [`repl-current-eval`](#repl-current-eval) parameter, contains function to eval forms. Default: `repl-eval`
* [`repl-current-parse`](#repl-current-parse) parameter, contains function to parse forms. Default: `repl-parse`
* [`repl-eval`](#repl-eval) default function to eval forms.
* [`repl-parse`](#repl-parse) default function to parse forms

Other functions are not (yet) documented.

### Functions

#### repl-current-eval
Syntax: `(repl-current-eval proc)`
<br/>Added in 1.0.1

`proc` must be a function accepting two arguments:
* form - a possibly annotated form containing Scheme source code to evaluate
* env - the environment where to evaluate the form

and must return a single value:
* the list of values returned by evaluating `form` in environment `env`

It will be called to evaluate each parsed form.

The parameter's default value is is [repl-eval](#repl-eval),
and a user-defined function usually calls it to actually evaluate forms,
possibly adding operations before and after such call.

For handling annotations and extracting vanilla source forms from them,
see [Chez Scheme user guide: Section 11.11](https://cisco.github.io/ChezScheme/csug/syntax.html#./syntax:h11)<br/>
Short summary: for extracting the list of source forms, use something like `(if (annotation? arg) (annotation-stripped arg) arg)`

Default value: `repl-eval`


#### repl-current-parse
Syntax: `(repl-current-parse proc)`
<br/>Added in 1.0.1

`proc` must be a function accepting three arguments:
* lctx - the current linectx
* initial-parser - the initial parser to use: a symbol or parser
* str - the user-typed string to parse

and must return two values:
* a possibly annotated list of forms containing Scheme code to evaluate
* the updated parser to use

It will be called to parse each user-typed string.

The parameter's default value is is [repl-parse](#repl-parse),
and a user-defined function usually calls it to actually parse the string,
possibly adding operations before and after such call.

Default value: `repl-parse`


#### repl-eval
Syntax: `(repl-parse form env)`
<br/>Added in 1.0.1

Arguments:
* form - a possibly annotated form containing Scheme source code to evaluate
* env - the environment where to evaluate the form

Compiles and evaluates `form` in environment `env`, and returns one value:
* the list of values returned by executing `form`


#### repl-parse
Syntax: `(repl-parse lctx initial-parser str)`
<br/>Added in 1.0.1

Arguments:
* lctx - the current linectx
* initial-parser - the initial parser to use: a symbol or parser
* str - the user-typed string to parse

Parses a user-typed string. Returns two values:
* a possibly annotated list of forms containing Scheme code to evaluate
* the updated parser to use


### Examples:

After executing
```scheme
(repl-current-parse
  (lambda (lctx initial-parser str)
    (printf "# repl-current-parse: user-typed string is ~s\n" str)
    (flush-output-port (current-output-port))
    (repl-parse lctx initial-parser str)))

(repl-current-eval
  (lambda (form env)
    (printf "# repl-current-eval: form to eval is ~s\n" form)
    (flush-output-port (current-output-port))
    (let ((vals (repl-eval form env)))
      (printf "# repl-current-eval: values list is ~s\n" vals)
      (flush-output-port (current-output-port))
      vals)))
```

the command
```
ls srfi/
```
produces as output:
```shell
# repl-current-parse: user-typed string is "ls srfi/"
# repl-current-eval: form to eval is (shell "ls" "srfi/")
18-multithreading.ss
# repl-current-eval: values list is (#<void>)
```

and the command
```
(+ 1 2 3) (values (/ 2 6) 1+2i)
```
produces as output:
```shell
# repl-current-parse: user-typed string is "(+ 1 2 3) (values (/ 2 6) 1+2i)"
# repl-current-eval: form to eval is (+ 1 2 3)
# repl-current-eval: values list is (6)
6
# repl-current-eval: form to eval is (values (/ 2 6) 1+2i)
# repl-current-eval: values list is (1/3 1+2i)
1/3
1+2i
```

To unregister the user-defined functions and restore the default behavior, execute the following:
```scheme
(repl-current-parse repl-parse)
(repl-current-eval repl-eval)
```
