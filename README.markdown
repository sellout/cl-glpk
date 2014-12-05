# cl-glpk

A Common Lisp library for solving linear programs.

## Portability

CL-GLPK should work on any Lisp/OS/machine combination, as long as CFFI,
Trivial-Garbage, Iterate, Autowrap, and GLPK work.

CL-GLPK has been tested on SBCL 1.2.5 Linux x86-64 with GLPK 4.55.

## Quick Introduction

Load the ASDF system. The low-level FFI bindings are implicitly documented by
the GLPK manual. For the (partial) high-level API, see `examples/sample.lisp`. The highest-level API allows you to express linear programs like:

    (make-linear-program
     :maximize (+ (* (- 12 2) x1) (* 6 x2) (* 4 x3))
     :subject-to ((<= (+ x1 x2 x3) 100)
                  (<= (+ (* (+ 2 2) x2) (* 10 x1) (* 5 x3)) (+ 200 400))
                  (<= (+ (* 2 x1) (* 2 x2) (* 6 x3)) 300))
     :bounds ((>= x3 10)))

Any of the :BOUNDS constraints could be in the :SUBJECT-TO list instead, but I'm
not yet sure what impact that will have on performance, so until I can identify
(and if necessary, prevent) that, I pull my variable bounds into a separate
parameter.

Then you can solve the program and get results with

    (simplex *)
    (format t "z = ~a, x1 = ~a, x2 = ~a, x3 = ~a~%"
            (objective-value **)
            (column-primal-value ** 1)
            (column-primal-value ** 2)
            (column-primal-value ** 3))

There are still a lot of improvements to build into this high-level API before
it's really complete.

## A note about this fork

This fork of the CL-GLPK project uses cl-autowrap instead of the
manually-written CFFI bindings. Thus the low-level interface has
changed. From what I see these are the notable changes:

- Use of glp- prefix instead of % for low-level functions.

- Passing +glp-constant+ rather than :constant. Unfortunately GLPK
doesn't use enums in its function prototypes. The old Lisp names also
don't directly translate to GPLK's names, so :lower-bound may end up
as +glp-lo+.

- Autowrap's allocation/array access/etc. need to be used, rather than
CFFI's.

- GLPK's own interface has changed. For example, glp-simplex now takes
another argument.

Also, I have fixed a small memory leak when setting a problem's
constraints.

## License

BSD sans advertising clause (see file COPYING)

## TODO List

### Write more examples

### Improve error handling

Currently error handling is done by either `(ERROR ...)` or `(ASSERT ...)`.

### Extend High Level API

#### Make up a DSL (similar to GLPK's MPS files, but lispier)

This is partially completed in `MAKE-LINEAR-PROGRAM`.
