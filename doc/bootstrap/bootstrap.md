# bootstrap

The library `(scheme2k bootstrap)`, which is also included in `(scheme2k)` and `(schemesh)`,
provides basic utility functions and macros.

### Alphabetical index

* [`(==>)`](#==>)
* [`(assert*)`](#assert*)
* [`(assert-not*)`](#assert-not*)
* [`(catch)`](#catch)
* [`(define-macro)`](#define-macro)
* [`(forever)`](#forever)
* [`(first-value)`](#first-value)
* [`(let-macro)`](#let-macro)
* [`(list->values)`](#list->values)
* [`(repeat)`](#repeat)
* [`(second-value)`](#second-value)
* [`(with-locked-objects)`](#with-locked-objects)
* [`(while)`](#while)
* [`(until)`](#until)
* [`(try)`](#try)
* [`(values->list)`](#values->list)

### Functions and macros

##### (==>)
Macro `==>` introduces a DSL for chaining/threading multiple functions or macro invocations.

Recognizes the following auxiliary keywords:
* `=>`     : unconditional chaining
* `?=>`    : conditional chaining
* `_`      : placeholder for inserting an expression into a template

Syntax `(==> ...)` must be followed by one or more function or macro invocations.

Each functions or macro invocation is **not** enclosed in parentheses: it is instead delimited by the aux keywords `=>` `?=>`
Examples: `(==> a foo => b bar => c baz)` or `(==> a foo ?=> b bar ?=> c baz)`

The call to each function or macro is inserted literally into the next one, before the first argument of the next function or macro or, if present, at the position of placeholder `_`

Examples:
| syntax                            | expands to                          |
|-----------------------------------|-------------------------------------|
| `(==> a)`                         | `(a)`                               |
| `(==> a foo)`                     | `(a foo)`                           |
| `(==> a foo bar baz)`             | `(a foo bar baz)`                   |
| `(==> a foo => b)`                | `(b (a foo))`                       |
| `(==> a => b bar)`                | `(b (a) bar)`                       |
| `(==> a => b _ bar)`              | `(b (a) bar)` i.e. identical to previous one |
| `(==> a => b bar _)`              | `(b bar (a))`                       |
| `(==> a foo => b bar)`            | `(b (a foo) bar)`                   |
| `(==> a foo => b bar => c baz)`   | `(c (b (a foo) bar) baz)`           |
| `(==> a foo => b bar _ => c baz)` | `(c (b bar (a foo)) baz)`           |

Keyword `?=>` adds short-circuit logic, i.e. if the function call at its left evaluates to `#f`,
the chain immediately evaluates to `#f` without executing the remaining functions. Examples:
<table>
  <thead>
    <tr>
      <td><b>syntax</b></td>
      <td><b>expands to</b></td>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>
<pre>(==> a ?=> b)</pre>
      </td><td>
<pre>
(let ((tmp (a)))
  (and tmp (b tmp)))
</pre>
      </td>
    </tr><tr>
      <td>
<pre>(==> a ?=> b ?=> c)</pre>
      </td><td>
<pre>
(let ((tmp-a (a)))
  (and tmp-a
       (let ((tmp-b (b tmp-a)))
         (and tmp-b (c tmp-b)))))
</pre>
      </td>
    </tr>
  </tbody>
</table>

The three keywords `=>` `?=>` `_` can be used simultaneously. Example:
<table>
  <thead>
    <tr>
      <td><b>syntax</b></td>
      <td><b>expands to</b></td>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>
<pre>(==> a foo ?=> b bar _ => c)</pre>
      </td><td>
<pre>
(let ((tmp-a (a foo)))
  (and tmp-a
       (c (b bar tmp-a))))
</pre>
      </td>
    </tr>
  </tbody>
</table>

Note: the keywords `=>` `?=>` `_` are recognized by symbol `eq?`, i.e. the identifiers in user-provided form are compared with `eq?` against the symbols `=>` `?=>` `_`. This means macro `==>` **ignores** any definition for the symbols `=>` `?=>` `_`

The keywords `=>` `?=>` `_` are recognized **only** if they appear at top level. They are **not** recognized if they appear inside parentheses.

##### (assert*)
TO BE DOCUMENTED

##### (assert-not*)
TO BE DOCUMENTED

##### (catch)
Auxiliary keyword recognized by `(try)`

##### (define-macro)
TO BE DOCUMENTED

##### (forever)
TO BE DOCUMENTED

##### (first-value)
TO BE DOCUMENTED

##### (let-macro)
TO BE DOCUMENTED

##### (list->values)
TO BE DOCUMENTED

##### (repeat)
TO BE DOCUMENTED

##### (second-value)
TO BE DOCUMENTED

##### (with-locked-objects)
TO BE DOCUMENTED

##### (until)
TO BE DOCUMENTED

##### (try)
TO BE DOCUMENTED

##### (values->list)
TO BE DOCUMENTED
