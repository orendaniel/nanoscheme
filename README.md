# nanoscheme

Scheme meta circular evaluator / evaluator library.


Nanoscheme is tested on chicken scheme 5.
It may or may not work unmodified on other scheme implementations.

These modifications most of the time are trivial.

# Basic Example:

.. load nanoscheme.scm ..

(define nscm (nanoscheme-full))

((nscm 'eval) '(display "A"))

# Keysymbols

- quote

- unquoute

- set!

- define

- if

- lambda

- macro

- macro.

- &

- @

- ...

- _environment_


# Deviation from RnRS

Nanoscheme doesn't follow any of the RnRS standard!

Most notable deviations are:

- lack of syntax-rule

- no builtin (define (func p1 p2 ...) function) form

- the call of non defined variables returns '()

- anonymous functions can be recursive with @ keysymbol

- unqoute (,) causes the expression ,expr to be passed in to the evaluator
  with the current lexical scope thus, literally unqouting the expression

- define, set!, lambda, macro will recursively evaluate the name (for define and set!)
  or the parameters list (for lambda macro) until a symbol is reached.

- macro and macro. keysymbol

- variadic functions with the ... keysymbol

# TODO

- Write about the new macro system 

- Better hash function

