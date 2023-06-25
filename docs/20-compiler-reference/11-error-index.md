# Compiler Error Index

Each diagnostic emitted from the compiler has a unique error number of the form
`SCMXXX` where `XXX` is the error number. Error numbers are grouped into
'categories' based on the phase in the compiler that emitted the diagnostic.

Currently the diagnostic categories are:

 * `1-9` - Legacy syntax errors.
 * `10-19` - Parser and read.
 * `20-29` - Library declaration and use.
 * `30-39` - Binder diagnostics.
 * `40-49` - Macro definition and expansion.

TODO: Fill out the index with the current errors for reference.
