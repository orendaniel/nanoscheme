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

Nanoscheme doesn't follow any of the RnRS standard!

Most notable deviations are:

- lack of syntax-rule

- no builtin (define (func p1 p2 ...) function) form

- the call of non defined variables returns '()

- anonymous functions can be recursive with recall keyword

- unqoute (,) causes the expression ,expr to be passed in to the evaluator
  with the current lexical scope thus, literally unqouting the expression

- the current lexical environment is accessible with _env keyword

- define, set!, lambda, macro will recursively evaluate the name (for define and set!)
  or the parameters list (for lambda macro) until a symbol is reached.


# TODO

REWRITE EVERYTHING REGARDING MACROS

# Macro Example

Many standard scheme features can be implemented with a macros.

For example implementation of OR:

(define or (macro (...)
	(define or (lambda (stmts)
		(if (null? stmts)
			#f
			(if ,(car stmts)
				#t
				(or (cdr stmts))))))
	(or ...)))

