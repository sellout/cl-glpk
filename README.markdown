# cl-glpk

## Portability

CL-GLPK should work on any Lisp/OS/machine combination, as long as CFFI, Trivial-Garbage, Iterate, and GLPK work.

CL-GLPK has been tested on SBCL 1.0.2 / Mac OS X (10.4) / PPC and CCL trunk /
Mac OS X (10.6) / x86-64.

## Quick Introduction

Load the ASDF system. The low-level FFI bindings are implicitly documented by
the GLPK manual. For the (partial) high-level API, see EXAMPLES/SAMPLE.LISP.

## License

BSD sans advertising clause (see file COPYING)

## TODO List

### Write more examples

### Improve error handling

Currently error handling is done by either (error ...) or (assert ...).

### Extend High Level API

This is partially completed in MAKE-LINEAR-PROGRAM.

### Make up a DSL (similar to GLPK's MPS files, but lispier)

### Complete FFI bindings
