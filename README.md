# nanoscheme

Scheme meta circular evaluator / evaluator library.


Nanoscheme is tested on chicken scheme 5.
It may or may not work unmodified on other scheme implementations.

These modifications most of the time are trivial.

# Basic Example:

.. load nanoscheme.scm ..

(define nscm (nanoscheme-full))

((nscm 'eval) '(display "A"))

# Deviation from RnRS

Nanoscheme doesn't follow any of the RnRS standards!

Most notable deviations are:

- lack of syntax-rule

- no builtin (define (func p1 p2 ...) function) form

- the call of non defined variables returns '()

- unqoute (,) causes the expression ,expr to be passed in to the evaluator
  with the current lexical scope thus, literally unqouting the expression.

- variadic functions with the ... name

# Macros

the macro form -- (macro (parameter-list) body) returns a new macro function.
When called it will evaluate the arguements literally.
