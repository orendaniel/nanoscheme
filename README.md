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

For example implementation of cond

(define cond (macro (...) 
	(define cond (lambda (stmts)
		(if (null? stmts)
		''()
		(list 'if (car (car stmts)) (list (append '(lambda ()) (cdr (car stmts)))) (cond (cdr stmts))))))
	(cond ...)))

The function cond returns an S-expression of "cond->if" in a new scope which is passed to the evaluator.
