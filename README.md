# module-king

ModuleKing programming language, initial Rust version

Planning on renaming the language and want to move on to a
self-hosted version ASAP

The feature set for this version will be similar to C with the following
differences

* Interpreted, not compiled like major C implementations
* Directory-based modules like e.g. Rust
* Two built-in generic types `List(Type)` (dynamic array) and `Map(Type)`
which will be a map from `String` -> `Type`
* Tagged unions which will each be declared as a `variant`

Once I finish the above feature-set it should be enough to write a self-hosted
compiler which I currently plan to make output x86 assembly text files.

The git repo for the self-hosted version which I will get working on is
[`spectra`](https://github.com/gnocarroll/spectra).

Will change `spectra` from being private when I make some more progress,
currently just getting started and it is not usable at all.

## Use of Executable

Executable will look inside directory you run it in for codefiles and directories.

It will determine what is a codefile by checking for extension specified in
`constants.rs`.

It will use these to create a directory-based module structure and then run
your code in the interpreter.

Currently executable does not use command-line arguments.
