# schemesh lineedit key bindings

The effect of key presses at REPL is controlled the library `(schemesh lineedit)` which is also included in `(schemesh)`.
Users can change these effects, and define new ones.

The library `(schemesh lineedit)` contains default key bindings suitable for most terminals:<br/>
they are stored in the scheme global variable `linectx-default-keytable`<br/> which is a hashtable and can be accessed by calling
* `(values linectx-default-keytable)`
* `(hashtable-cells linectx-default-keytable)`
* or any equivalent mechanism that inspects the content of hashtable `linectx-default-keytable`.

To change a key binding, or create a new one, call
```scheme
(linectx-keytable-insert! linectx-default-keytable PROCEDURE BYTE-SEQUENCE)
```
where
* `PROCEDURE` is the scheme function to execute when the desired key is pressed
* `BYTE-SEQUENCE` is the sequence of bytes produced by such key press,<br/>
  and must be encoded as one of: a fixnum, a list of fixnums, a bytevector or a string.

Example:
```scheme
(linectx-keytable-insert! linectx-default-keytable lineedit-key-history-prev "\x1b;[[A")
```
associates the byte sequence `"\x1b;[[A"` i.e. `ESC [ [ A`, usually produced by key F1,<br/>
to the function `lineedit-key-history-prev` that searches backward in history.

Defining new functions that can be passed as arguments to `(linectx-keytable-insert!)` is mostly undocumented,<br/>
because such functions need to access internals of `linectx` objects, which are mostly undocumented too -
see [linectx.md](linectx.md) for the (few) documented ones.

### lineedit key functions

The following functions are predefined by library `(schemesh lineedit)`
and can be passed as arguments to `(linectx-keytable-insert!)`.

Note: you need to pass the functions itself - not call it - thus the parentheses must be omitted.<br/>
Example:
```scheme
(linectx-keytable-insert! linectx-default-keytable lineedit-key-nop "\x12;")
```

##### (lineedit-key-nop)
`(lineedit-key-nop lctx)` does nothing. Can be used to effectively remove a key binding.

##### (lineedit-key-left)
`(lineedit-key-left lctx)` moves cursor left by 1, jumping to the end of previous line
  if cursor is already at the beginning of current line

##### (lineedit-key-right)
`(lineedit-key-right lctx)` moves cursor right by 1, jumping to the beginning of next line
  if cursor is already at the end of current line

##### (lineedit-key-up)
`(lineedit-key-up lctx)` moves cursor up by 1, or to previous history entry if cursor is already on the first line

##### (lineedit-key-down)
`(lineedit-key-down lctx)` moves cursor down by 1, or to next history entry if cursor is already on the last line

##### (lineedit-key-word-left)
`(lineedit-key-word-left lctx)` moves cursor to the beginning of current word

##### (lineedit-key-word-right)
`(lineedit-key-word-right lctx)` moves cursor to end of current word

##### (lineedit-key-bof)
`(lineedit-key-bof lctx)` moves cursor to the beginning of *first* line

##### (lineedit-key-bol)
`(lineedit-key-bol lctx)` moves cursor to the beginning of current line

##### (lineedit-key-eol)
`(lineedit-key-eol lctx)` moves cursor to the end of current line

##### (lineedit-key-eof)
`(lineedit-key-eof lctx)` moves cursor to the end of *last* line

##### (lineedit-key-clear)
`(lineedit-key-clear lctx)` deletes all lines

##### (lineedit-key-ctrl-d)
`(lineedit-key-ctrl-d lctx)` deletes one character to the right. Acts as end-of-file if lines are empty.

##### (lineedit-key-del-left)
`(lineedit-key-del-left lctx)` deletes one character to the left of cursor,
  then moves cursor one character to the left as `(lineedit-key-left lctx)` does.

##### (lineedit-key-del-right)
`(lineedit-key-del-right lctx)` deletes one character at cursor. Does not move the cursor.

##### (lineedit-key-del-word-left)
`(lineedit-key-del-word-left lctx)` deletes from cursor to beginning of current word.
  Also moves cursor n characters to the left, where n is the number of deleted characters.

##### (lineedit-key-del-word-right)
`(lineedit-key-del-word-right lctx)` deletes from cursor to end of current work. Does not move the cursor.

##### (lineedit-key-del-line-left)
`(lineedit-key-del-line-left lctx)` deletes from cursor to beginning of current line

##### (lineedit-key-del-line-right)
`(lineedit-key-del-line-right lctx)` deletes from cursor to end of current line

##### (lineedit-key-newline-left)
`(lineedit-key-newline-left lctx)` inserts a newline character before the cursor, and moves the cursor
  to beginning of next line

##### (lineedit-key-newline-right)
`(lineedit-key-newline-right lctx)` inserts a newline character at the cursor. Does not move the cursor.

##### (lineedit-key-enter)
`(lineedit-key-enter lctx)` if lines contain correctly paired parentheses, brackets, braces and quotes,
  executes the lines. Otherwise inserts a newline character before the cursor, and moves the cursor
  to beginning of next line

##### (lineedit-key-histor-nect)
`(lineedit-key-history-next lctx)` searches backward in history for lines that start with the same characters
  as the text between start of first line and cursor, and if found replace current lines
  with text from such history entry

##### (lineedit-key-history-prev)
`(lineedit-key-history-prev lctx)` searches forward in history for lines that start with the same characters
  as the text between start of first line and cursor, and if found replace current lines

##### (lineedit-key-insert-clibboard)
`(lineedit-key-insert-clipboard lctx)` inserts the content of clipboard at cursor

##### (lineedit-key-redraw)
`(lineedit-key-redraw lctx)` redraws current lines

##### (lineedit-key-autocomplete)
`(lineedit-key-autocomplete lctx)` tries to autocomplete current word by calling the function
  stored in parameter `(linectx-completion-proc)`

##### (lineedit-key-inxpect-linectx)
`(lineedit-key-inspect-linectx lctx)` debugging helper: calls Chez Scheme `(inspect)` on `linectx` object passed as argument

##### (lineedit-key-cmd-cd-parent)
`(lineedit-key-cmd-cd-parent lctx)` executes the shell command `{cd ..}`, then redraws prompt and current lines

##### (lineedit-key-cmd-cd-old-dir)
`(lineedit-key-cmd-cd-old-dir lctx)` executes the shell command `{cd-}`, then redraws prompt and current lines

##### (lineedit-key-cmd-ls)
`(lineedit-key-cmd-ls lctx)` executes the shell command `{ls}`, then redraws prompt and current lines



### Examples

The following code instructs schemesh to start the command `` make -j `nproc` ``
every time keypad KP+ is pressed, which produces the sequences `ESC O k` at least on xterm:
```
(linectx-keytable-insert! linectx-default-keytable
  (lambda (lctx)
    (lineedit-key-clear lctx)
    (linectx-insert/string! lctx "make -j `nproc`")
    (lineedit-key-enter lctx))
  "\x1b;Ok")
```
See [linectx.md](linectx.md) for the function `(linectx-insert/string!)` used in the example and not documented above.
