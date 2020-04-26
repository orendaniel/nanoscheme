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

