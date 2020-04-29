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

quote unquoute set! define if lambda macro macro. & @ ... _environment_

core functions:

eval apply run empty-environment


# Deviation from RnRS

Nanoscheme doesn't follow any of the RnRS standards!

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

# Macros

The basic idea behind the macro system is,
that a macro is a function that receives the parameters literally.

They also share the same environment with caller. Macros can be called with run command.

(run macro param environment)

In order to avoid name capture, every macro parameter is prefix with & (a guard prefix).

Macros can be deeply guarded (that is any non macro function's parameters will also receive a prefix)
or unguarded (that is only the macro's parameters will receive a prefix) with the dotted macro.

As long as define is not used directly inside the macro, and the macro doesn't call 
a nested macro, name capture cannot occur.

examples

(define or (macro (...)
	((lambda (stmts)
		(if (null? &stmts)
			#f
			(if ,(car &stmts)
				#t
	(@ (cdr &stmts))))) &...)))

(define begin (macro (...)
	,(cons (append '(lambda ()) &...) '())))

(define cond (macro (...)
	(define else #t)
	((lambda (stmts)
		(if (null? &stmts)
			'()
			(if ,(caar &stmts)
				,(cons (append '(lambda ()) (cdar &stmts)) '())
				(@ (cdr &stmts))))) &...)))

# TODO

- Better hash function

