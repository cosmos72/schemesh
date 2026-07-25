# lineedit

The effect of key presses at REPL is controlled the library `(scheme2k lineedit)` which is also included in `(schemesh)`.
Users can change these effects, and define new ones, as described in [doc/lineedit/key.md](key.md).

The main type defined by library `(scheme2k lineedit)` is `linectx`,
and almost all functions in the library take a `linectx` object as first argument.

Internals of `linectx` objects are mostly undocumented, the (few) documented functions are:

##### (linectx?)
`(linectx? lctx)` returns `#t` if `lctx` is a `linectx` object, otherwise returns `#f`.

##### (linectx-height)
`(linectx-height lctx)` returns the current terminal height, i.e. the number of rows.

##### (linectx-width)
`(linectx-width lctx)` returns the current terminal width, i.e. the number of columns.

##### (linectx-clipboard)
`(linectx-clipboard lctx)` returns the clipboard as a `vcellspan` object.

##### (linectx-clipboard-clear!)
`(linectx-clipboard-clear! lctx)` clears the clipboard contents.

##### (linectx-history)
`(linectx-history lctx)` returns the history as a `vhistory` object.

##### (linectx-insert/bytespan!)
`(linectx-insert/bytespan! lctx bsp [start end])` inserts the contents of a `bytespan` object into current lines, starting at cursor.<br/>
Added in 0.9.2

##### (linectx-insert/char!)
`(linectx-insert/char! lctx ch)` inserts a single character into current lines, starting at cursor.<br/>
Added in 0.9.2

##### (linectx-insert/charspan!)
`(linectx-insert/charspan! lctx csp [start end])` inserts the contents of a `charspan` object into current lines, starting at cursor.<br/>
Added in 0.9.2

##### (linectx-insert/string!)
`(linectx-insert/string! lctx str [stard end])` inserts the contents of a `string` object into current lines, starting at cursor.<br/>
Added in 0.9.2

##### (linectx-load-history!)
`(linectx-load-history! lctx)` loads history from file. Returns `#t` if successful, otherwise returns `#f`.

##### (linectx-parser-name)
`(linectx-parser-name lctx)` returns a symbol representing the current syntax parser,
usually one of `'shell` `'scheme` `'r6rs`

Note that more parsers can be defined, either by the user or by future versions.

##### (linectx-parser-name-set!)
`(linectx-parser-name-set! lctx name)` changes the current syntax parser.

This function is effective only if called from a key binding, i.e. from a procedure installed with `(linectx-keytable-insert! ...)`
because the REPL overwrites it before waiting for the next lines to evaluate.

Name must be a symbol among the currently enable parsers, usually one of `'shell` `'scheme` `'r6rs`

Note that more parsers can be defined, either by the user or by future versions,
and that it's possible to create a `linectx` object with **fewer** enabled parsers.

##### (linectx-save-history)
`(linectx-save-history lctx)` saves history to file. Returns `#t` if successful, otherwise returns `#f`.

##### (linectx-to-history)
`(linectx-to-history lctx)` appends a copy of current lines to history, and returns an unspecified value.
Also clears current lines, and removes empty lines from history.<br/>
Added in 0.9.3

##### (linectx-vscreen lctx)
`(linectx-vscreen lctx)` returns current lines as a `vscreen` object.
