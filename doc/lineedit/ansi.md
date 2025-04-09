# schemesh prompt customization

schemesh prompt can be customized using two alternative mechanisms:

1. by setting the environment variable `$SCHEMESH_PS1`, which is mostly compatible with bash PS1
   as described in section "PROMPTING" of `man bash` and available online at
   https://www.man7.org/linux/man-pages//man1/bash.1.html#PROMPTING

2. by registering a user-defined procedure that will be executed each time schemesh needs to draw the prompt.
   Such procedure must accept a single `lctx` argument and can update the prompt stored into `lctx` as it sees fit.

Such a prompt-updating procedure can be registered into schemesh by calling
`(linectx-prompt-proc MY-PROCEDURE)`

The registered procedure can run arbitrary Scheme code, and can also launch external commands
(possibly slow, but supported) either with Chez Scheme function `(open-process-ports)`
or with schemesh facilities for running jobs and capturing their output, as for example `(sh-run/string)`.

Users must take care to capture or redirect all the input and output of the external commands,
because they should **not** read from or write to the current terminal.

### schemesh lineedit linectx

The library `(schemesh lineedit linectx)` provides two functions for prompt customization:

`(linectx-prompt-ansi-text lctx)` extracts the current prompt from `lctx` object and converts it to a mutable `ansi-text` object

`(linectx-prompt-ansi-text-set! lctx a)` stores the `ansi-text` object `a` into `lctx` as the updated prompt

### schemesh lineedit ansi

The library `(schemesh lineedit ansi)` provides several functions for inspecting and modifying `ansi-text` objects:

`(ansi-text? a)` returns `#t` if `a` is an `ansi-text` object, otherwise returns `#f`

`(ansi-text-bytes a)` returns the bytespan (a resizeable bytevector) containing the current text

`(ansi-text-visible-length a)` returns an unsigned fixnum indicating the **visible** length of the current text,
  i.e. ignoring all the escape sequences contained in the current text

`(ansi-text-clear! a)` removes all text from `ansi-text` object. It should be called as first action,
  before filling `ansi-text` object with a new prompt

`(make-ansi-text)` can be used to create a standalone `ansi-text` object. This is usually not necessary when
  customizing a prompt

`(string+ a string)` and `(string+ a string visible-length)` append a string to the current text.
  If string contains escape sequences, the **visible** length of the string, i.e. ignoring all the escape sequences,
  must be specified too

`(black a string)`    appends string to the current text, highlighting it in black foreground

`(black+ a string)`   appends string to the current text, highlighting it in bold black or bright black (= grey) foreground

`(red a string)`      appends string to the current text, highlighting it in red foreground

`(red+ a string)`     appends string to the current text, highlighting it in bold/bright red foreground

`(green a string)`    appends string to the current text, highlighting it in green foreground

`(green+ a string)`   appends string to the current text, highlighting it in bold/bright green foreground

`(yellow a string)`   appends string to the current text, highlighting it in yellow foreground

`(yellow+ a string)`  appends string to the current text, highlighting it in bold/bright yellow foreground

`(blue a string)`     appends string to the current text, highlighting it in blue foreground

`(blue+ a string)`    appends string to the current text, highlighting it in bold/bright blue foreground

`(magenta a string)`  appends string to the current text, highlighting it in magenta foreground

`(magenta+ a string)` appends string to the current text, highlighting it in bold/bright magenta foreground

`(cyan a string)`     appends string to the current text, highlighting it in cyan foreground

`(cyan+ a string)`    appends string to the current text, highlighting it in bold/bright cyan foreground

`(white a string)`    appends string to the current text, highlighting it in white foreground

`(white+ a string)`   appends string to the current text, highlighting it in bold/bright white foreground


`(color a col-seq string)` is a general-purpose string highliting function:
  appends string to the current text, highlighting it with specified ansi attribute.
  The argument `col-seq` must be a string containing a semicolon-separated list of ansi colors codes
  supported by the current terminal. Some examples supported by most terminals:
- `"0"`  reset foreground, background and bright or bold to default
- `"1"`  bright or bold foreground
- `"30"` black   foreground
- `"31"` red     foreground
- `"32"` green   foreground
- `"33"` yellow  foreground
- `"34"` blue    foreground
- `"35"` magenta foreground
- `"36"` cyan    foreground
- `"37"` white   foreground
- `"40"` black   background
- `"41"` red     background
- `"42"` green   background
- `"43"` yellow  background
- `"44"` blue    background
- `"45"` magenta background
- `"46"` cyan    background
- `"47"` white   background

Example:
```lisp

(define (my-prompt lctx)
  (let ((a (linectx-prompt-ansi-text lctx)))
    (ansi-text-clear! a)
    (magenta a (c-username))
    (string+ a "@")
    (blue+   a (c-hostname))
    (string+ a ":")
    (cyan    a (charspan->string (sh-home->~ (sh-cwd))))
    (string+ a ":")
    (linectx-prompt-ansi-text-set! lctx a)))

(linectx-prompt-proc my-prompt)
```

To restore the default prompt procedure, that reads and interprets the environment variable `$SCHEMESH_PS1`, execute
```lisp
(linectx-prompt-proc sh-expand-ps1)
```
