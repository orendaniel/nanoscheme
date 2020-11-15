;; Nanoscheme
;; Copyright (C) 2020  Oren Daniel

;; This file is licensed under the terms of the BSD 3-Clause License.

(load "nanoscheme.scm")

(define env (make-environment '()))

((env 'def) 'eval nscm-eval)
((env 'def) 'apply nscm-apply)
((env 'def) 'expand nscm-expand)
((env 'def) 'empty-environment (lambda () (make-environment '())))

((env 'def) 'car car)
((env 'def) 'cdr cdr)
((env 'def) 'cons cons)

((env 'def) '+ +)
((env 'def) '* *)
((env 'def) '- -)
((env 'def) '/ /)
((env 'def) 'expt expt)
((env 'def) 'modulo modulo)

((env 'def) 'not not)
((env 'def) '< <)
((env 'def) '<= <=)
((env 'def) '> >)
((env 'def) '>= >=)
((env 'def) '= =)
((env 'def) 'eq? eq?)
((env 'def) 'eqv? eqv?)
((env 'def) 'equal? equal?)

((env 'def) 'null? null?)
((env 'def) 'pair? pair?)
((env 'def) 'char? char?)
((env 'def) 'symbol? symbol?)
((env 'def) 'number? number?)
((env 'def) 'vector? vector?)
((env 'def) 'string? string?)
((env 'def) 'boolean? boolean?)

((env 'def) 'integer? integer?)
((env 'def) 'real? real?)
((env 'def) 'complex? complex?)

((env 'def) 'string->number string->number)
((env 'def) 'string->symbol string->symbol)
((env 'def) 'symbol->string symbol->string)
((env 'def) 'number->string number->string)

((env 'def)
	'procedure? 
	(lambda (prc) (or (procedure? prc) (eqv? (car prc) 'FNC))))

((env 'def) 'make-rectangular make-rectangular)
((env 'def) 'make-polar make-polar)
((env 'def) 'make-string make-string)
((env 'def) 'string-length string-length)
((env 'def) 'string-ref string-ref)
((env 'def) 'string-set! string-set!)
((env 'def) 'string-append string-append)
((env 'def) 'make-vector make-vector)
((env 'def) 'vector-length vector-length)
((env 'def) 'vector-ref vector-ref)
((env 'def) 'vector-set! vector-set!)

((env 'def) 'display (lambda (text) (display text) '()))
((env 'def) 'read read)
((env 'def) 'exit exit)
((env 'def) 'error error)

((env 'def) 'list list)
((env 'def) 'list? list?)
((env 'def) 'list-ref list-ref)
((env 'def) 'reverse reverse)
((env 'def) 'append append)

((env 'def) 'list->vector list->vector)
((env 'def) 'vector->list vector->list)

((env 'def) 'caar caar)
((env 'def) 'cadr cadr)
((env 'def) 'cdar cdar)
((env 'def) 'cddr cddr)
((env 'def) 'caaar caaar)
((env 'def) 'caadr caadr)
((env 'def) 'cadar cadar)
((env 'def) 'caddr caddr)
((env 'def) 'cdaar cdaar)
((env 'def) 'cdadr cdadr)
((env 'def) 'cddar cddar)
((env 'def) 'cdddr cdddr)

(nscm-eval
	'(define map (lambda (prc lst)
		(if (null? lst)
			'()
			(cons (prc (car lst)) (map prc (cdr lst)))))) env)

(nscm-eval
	'(define begin (macro (... *begin_body*)
		'((lambda ,() ,@*begin_body*)))) env)

(nscm-eval
	'(define let (macro (*let_defs* ... *let_body*)
		'((lambda ,(map car *let_defs*) ,@*let_body*)
			,@(map cadr *let_defs*)))) env)

(nscm-eval
	'(define and (macro (... *and_clauses*)
		(define helper (lambda (lst)
			(if (null? (cdr lst))
				(list 'if (car lst) #t #f)
				(list 'if (car lst) (helper (cdr lst)) #f))))
		(helper *and_clauses*))) env)

(nscm-eval
	'(define or (macro (... *or_clauses*)
		(define helper (lambda (lst)
			(if (null? (cdr lst))
				(list 'if (car lst) #t #f)
				(list 'if (car lst) #t (helper (cdr lst))))))
		(helper *or_clauses*))) env)

(nscm-eval
	'(define cond (macro (... *cond_clauses*)
		(define helper (lambda (lst)
			(if (null? (cdr lst))
				(list 'if (if (eqv? (caar lst) 'else) #t (caar lst)) '(begin ,@(cdar lst)) '())
				(list 'if (caar lst) '(begin ,@(cdar lst)) (helper (cdr lst))))))
		(helper *cond_clauses*))) env)


(define (repl)
	(display ">> ")
	(display (nscm-eval (read) env))
	(newline)
	(repl))

(repl)
