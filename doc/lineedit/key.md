# schemesh lineedit key bindings

The effect of key presses at REPL is controlled the library `(schemesh lineedit)`.
Users can change these effects, and define new ones.

The library `(schemesh lineedit)` contains default key bindings suitable for most terminals:<br/>
they are stored in the scheme global variable `linectx-default-keytable`<br/>
which is a hashtable and can be accessed by calling
* `(values linectx-default-keytable)`
* or `(hashtable-cells linectx-default-keytable)`
* or any equivalent mechanism that inspects the content of hashtable `linectx-default-keytable`.

To change a key binding, or create a new one, call
`(linectx-keytable-insert! linectx-default-keytable PROCEDURE BYTE-SEQUENCE)` where
* `PROCEDURE` is the scheme procedure to execute when the desired key is pressed
* `BYTE-SEQUENCE` is the sequence of bytes produced by such key press,
  which must be encoded as one of: a fixnum, a list of fixnums, a bytevector or a string.

Example:
```scheme
(linectx-keytable-insert! linectx-default-keytable lineedit-key-history-prev "\x1b;[[A")
```
associates the byte sequence `"\x1b;[[A"` i.e. `ESC [ [ A`, usually produced by key F1,<br/>
to the procedure `lineedit-key-history-prev` that searches backward in history.

Defining new procedures that can be passed as arguments to `(linectx-keytable-insert!)` is currently not documented,<br/>
because such procedures need to access internals of `linectx` objects, which are undocumented too.

The following procedures are predefined by library `(schemesh lineedit)` and can be passed
as arguments to `(linectx-keytable-insert!)`.

Note: you need to pass the procedures itself - not call it - thus the parentheses must be omitted.<br/>
Example:
```scheme
(linectx-keytable-insert! linectx-default-keytable lineedit-key-nop "\x12;")
```

`(lineedit-key-nop lctx)` does nothing. Can be used to effectively remove a key binding.

`(lineedit-key-left lctx)` moves cursor left by 1, jumping to the end of previous line
  if cursor is already at the beginning of current line

`(lineedit-key-right lctx)` moves cursor right by 1, jumping to the beginning of next line
  if cursor is already at the end of current line

`(lineedit-key-up lctx)` moves cursor up by 1, or to previous history entry if cursor is already on the first line

`(lineedit-key-down lctx)` moves cursor down by 1, or to next history entry if cursor is already on the last line

`(lineedit-key-word-left lctx)` moves cursor to the beginning of current word

`(lineedit-key-word-right lctx)` moves cursor to end of current word

`(lineedit-key-bof lctx)` moves cursor to the beginning of *first* line

`(lineedit-key-bol lctx)` moves cursor to the beginning of current line

`(lineedit-key-eol lctx)` moves cursor to the end of current line

`(lineedit-key-eof lctx)` moves cursor to the end of *last* line

`(lineedit-key-clear lctx)` deletes all lines

`(lineedit-key-ctrl-d lctx)` deletes one character to the right. Acts as end-of-file if lines are empty.

`(lineedit-key-del-left lctx)` deletes one character to the left of cursor,
  then moves cursor one character to the left as `(lineedit-key-left lctx)` does.

`(lineedit-key-del-right lctx)` deletes one character at cursor. Does not move the cursor.

`(lineedit-key-del-word-left lctx)` deletes from cursor to beginning of current word.
  Also moves cursor n characters to the left, where n is the number of deleted characters.

`(lineedit-key-del-word-right lctx)` deletes from cursor to end of current work. Does not move the cursor.

`(lineedit-key-del-line-left lctx)` deletes from cursor to beginning of current line

`(lineedit-key-del-line-right lctx)` deletes from cursor to end of current line

`(lineedit-key-newline-left lctx)` inserts a newline character before the cursor, and moves the cursor
  to beginning of next line

`(lineedit-key-newline-right lctx)` inserts a newline character at the cursor. Does not move the cursor.

`(lineedit-key-enter lctx)` if lines contain correctly paired parentheses, brackets, braces and quotes,
  executes the lines. Otherwise inserts a newline character before the cursor, and moves the cursor
  to beginning of next line

`(lineedit-key-history-next lctx)` searches backward in history for lines that start with the same characters
  as the text between start of first line and cursor, and if found replace current lines
  with text from such history entry

`(lineedit-key-history-prev lctx)` searches forward in history for lines that start with the same characters
  as the text between start of first line and cursor, and if found replace current lines

`(lineedit-key-insert-clipboard lctx)` inserts the content of clipboard at cursor

`(lineedit-key-redraw lctx)` redraws current lines

`(lineedit-key-autocomplete lctx)` tries to autocomplete current word by calling the procedure
  stored in parameter `(linectx-completion-proc)`

`(lineedit-key-inspect-linectx lctx)` debugging helper: calls Chez Scheme `(inspect)` on `linectx` object passed as argument

`(lineedit-key-cmd-cd-parent lctx)` executes the shell command `{cd ..}`, then redraws prompt and current lines

`(lineedit-key-cmd-cd-old-dir lctx)` executes the shell command `{cd-}`, then redraws prompt and current lines

`(lineedit-key-cmd-ls lctx)` executes the shell command `{ls}`, then redraws prompt and current lines
