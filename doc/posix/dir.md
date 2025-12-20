# posix dir

The library `(scheme2k posix dir)`, which is also included in `(scheme2k posix)` and `(schemesh)`,
provides low-level functions to access POSIX filesystem.

Additional functions are also provided by `(scheme2k posix replacements)`, see [replacements.md](replacements.md).

### Alphabetical index

* [`(directory-list)`](#directory-list)
* [`(directory-list-type)`](#directory-list-type)
* [`(directory-sort!)`](#directory-sort!)
* [`(file-delete)`](#file-delete)
* [`(file-rename)`](#file-rename)
* [`(file-type)`](#file-type)
* [`(mkdir)`](#mkdir)

### Functions

##### (directory-list)
`(directory-list dirpath [options])` returns a list of names present in a filesystem directory, in arbitrary order.<br/>
WARNING: Chez Scheme also defines a function `(directory-list)` with different options.<br/>
Argument `dirpath` must be a bytevector, string, bytespan or charspan.<br/>
Optional argument `options` must be a list containing zero or more of:
* `'append-slash` - if a returned name corresponds to a directory, then a '/' will be appended to its name
* `'bytes` - each returned name will be a bytevector, not a string
* `'catch` - errors will be ignored instead of raising a condition
* `'prefix` followed by a charspan, string or bytevector, indicating the filter-prefix: only names that start with such filter-prefix will be returned
* `'suffix` followed by a charspan, string or bytevector, indicating the filter-suffix: only names that end with such filter-suffix will be returned
* `'symlinks` - only meaningful together with `'types`: returned names that are symlinks will have type `'symlink` instead of the type of the file they point to
* `'types` - each returned list element will be a pair `(name . type)` where `name` is a bytevector or string, and `type` is a symbol, one of:
            `'unknown` `'blockdev` `'chardev` `'dir` `'fifo` `'file` `'socket` `'symlink`.
            The type `'symlink` can be returned only if option `'symlinks` is present.

##### (directory-list-type)
`(directory-list-type dirpath [options])` returns a list of names present in a filesystem directory, in arbitrary order, and their types.
Equivalent to `(directory-list dirpath [options])` with the difference that option `'types` is added automatically.

##### (directory-sort!)
`(directory-sort! dir-list)` in-place alphabetically sorts a list returned by `(directory-list)` or `(directory-list-type)`.
Returns the modified list.

##### (file-delete)
`(file-delete path [options])` deletes a file or an empty directory.<br/>
Argument `path` must be a bytevector, string or charspan.<br/>
Optional argument `options` must be a list containing zero or more:
* `'catch` - on error, return numeric c-errno instead of raising a condition

On success, returns `(void)`.<br/>
On error, either returns an integer error code (if `options` contain `'catch`) or raises an exception.

Improvements compared to Chez Scheme `(delete-file)`:
* also deletes empty directories.
* also accepts bytevectors, bytespans or charspans, not only strings.
* strings and charspans are converted to posix paths with UTF-8b instead of UTF-8 - the former can also represent invalid UTF-8 sequences.
* returns `(void)` on success and error code on failure, instead of a boolean.

##### (file-rename)
`(file-rename old-path new-path [options])` moves or renames a file or directory from `old-path` to `new-path`.<br/>
Both arguments `old-path` and `new-path` must be a bytevector, string or charspan.<br/>
Optional argument `options` must be a list containing zero or more:
* `'catch` - on error, return numeric c-errno instead of raising a condition.

On success, returns `(void)`.<br/>
On error, either returns an integer error code (if `options` contain `'catch`) or raises an exception.

Improvements compared to Chez Scheme `(rename-file)`:
* also accepts bytevectors, bytespans and charspans, not only strings.
* strings and charspans are converted to posix paths with UTF-8b instead of UTF-8 - the former can also represent invalid UTF-8 sequences.
* returns `(void)` on success and error code on failure, instead of an unspecified value.

##### (file-type)
`(file-type path [options])` checks existence and type of a filesystem path.<br/>
Argument `path` must be a bytevector, string, bytespan or charspan.<br/>
Optional argument `options` must be a list containing zero or more:
* `'catch` - on error, return numeric c-errno instead of raising a condition.
* `'symlinks` - if path is a symlink, returned value will be `'symlink` instead of the type of the file it points to.

If `path` exists, returns its type which as one the symbols:
`'unknown` `'blockdev` `'chardev` `'dir` `'fifo` `'file` `'socket` `'symlink`.
The type `'symlink` can be returned only if option `'symlinks` is present.

If `path` does not exists, returns `#f`.


##### (mkdir)
`(mkdir dirpath [options])` creates a directory.<br/>
WARNING: Chez Scheme also defines a function (mkdir) with different options.<br/>
Argument `dirpath` must be a bytevector, string or charspan.<br/>
Optional argument `options` must be a list containing zero or more:
* `'catch` - on error, return numeric c-errno instead of raising a condition.
* `'mode` followed by a fixnum - specifies the owner, group and others initial permissions on the directory - see POSIX "man 2 mkdir" for details.

On success, returns `(void)`.<br/>
On error, either returns an integer error code (if `options` contain `'catch`) or raises an exception.

Improvements compared to Chez Scheme `(mkdir)`:
* also accepts bytevectors, bytespans and charspans, not only strings.
* strings and charspans are converted to posix paths with UTF-8b instead of UTF-8 - the former can also represent invalid UTF-8 sequences.
* returns `(void)` on success and error code on failure, instead of an unspecified value.


