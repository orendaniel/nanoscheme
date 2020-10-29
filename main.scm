;; Nanoscheme
;; Copyright (C) 2020  Oren Daniel

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(load "nanoscheme.scm")

(define env (make-environment '()))

((env 'def) 'eval nanoscheme-eval)
((env 'def) 'apply nanoscheme-apply)
((env 'def) 'empty-environment (lambda () (make-environment '())))


((env 'def) 'car car)
((env 'def) 'cdr cdr)
((env 'def) 'cons cons)
((env 'def) 'set-car! set-car!)
((env 'def) 'set-cdr! set-cdr!)

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
	(lambda (prc) (or (procedure? prc) (eqv? (car prc) 'FNC) (eqv? (car prc) 'MCR))))

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

((env 'def) 'display (lambda (text) (display text) (newline) '()))
((env 'def) 'read read)
((env 'def) 'exit exit)
((env 'def) 'error error)

((env 'def) 'assoc assoc)
((env 'def) 'string-fill! string-fill!)
((env 'def) 'vector-fill! vector-fill!)

((env 'def) 'list list)
((env 'def) 'list? list?)
((env 'def) 'list-ref list-ref)
((env 'def) 'reverse reverse)
((env 'def) 'append append)
((env 'def) 'member member)

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

(nanoscheme-eval
	'(define or (macro (... *or_STMTS*)
		(define *or_HELPER* (lambda (*or_STMTS*)
			(if (null? *or_STMTS*)
				#f
				(if ,(car *or_STMTS*)
					#t
					(*or_HELPER* (cdr *or_STMTS*)))))) 
		(*or_HELPER* *or_STMTS*))) env)

(nanoscheme-eval 
	'(define and (macro (... *and_STMTS*)
		(define *and_HELPER* (lambda (*and_STMTS*)
			(if (null? *and_STMTS*)
				#t
				(if (not ,(car *and_STMTS*))
					#f
					(*and_HELPER* (cdr *and_STMTS*)))))) 
		(*and_HELPER* *and_STMTS*))) env)


(nanoscheme-eval
	'(define map (lambda (prc lst)
		(if (null? lst)
			'()
			(cons (prc (car lst)) (map prc (cdr lst)))))) env)

(nanoscheme-eval 
	'(define begin (macro (... *begin_PRM*)
		,(cons (append (list 'lambda '()) *begin_PRM*) '()))) env)

(nanoscheme-eval
	'(define cond (macro (... *cond_STMTS*)
		(define else #t)
		(define *cond_HELPER* (lambda (*cond_STMTS*)
			(if (null? *cond_STMTS*)
				'()
				(if ,(caar *cond_STMTS*)
					,(cons (append '(lambda ()) (cdar *cond_STMTS*)) '())
					(*cond_HELPER* (cdr *cond_STMTS*))))))

		(*cond_HELPER* *cond_STMTS*))) env)

(nanoscheme-eval
	'(define let (macro (*let_TUPLES* ... *let_REST*)
		,(append 
			(cons (append 
				(append '(lambda) (cons 
					(map car *let_TUPLES*)
					'()))
				*let_REST*) '())
			(map cadr *let_TUPLES*)))) env)

(define (repl)
	(display ">> ")
	(display (nanoscheme-eval (read) env))
	(newline)
	(repl))

(repl)
