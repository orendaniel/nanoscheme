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

;;; Environment 
(define make-environment (lambda (outer)
	;; every environment has an outer environment pointer
	;; that represent the outer lexical scope

	(define cells-count 10) ; 10 cells for each hash table
	(define dict (make-vector cells-count)) 
	(vector-fill! dict '()) 

	(define find-on-list (lambda (lst name fn op prms)
		(cond
			((null? lst) (if (null? outer) '() (apply (outer op) prms)))
			((eqv? (caar lst) name) (fn lst))
			(else (find-on-list (cdr lst) name fn op prms)))))

	(define look-if-defined? (lambda (lst name)
		(cond
			((null? lst) #f)
			((eqv? (caar lst) name) #t)
			(else (look-if-defined? (cdr lst) name)))))

	;; Hash function
	(define hash (lambda (sym)
		(modulo (string-length (symbol->string sym)) cells-count)))

	;; Definer
	(define dict-define! (lambda (name value)
		(if (not (look-if-defined? (vector-ref dict (hash name)) name))
			(set! 
				(vector-ref dict (hash name)) 
				(cons (cons name value) (vector-ref dict (hash name))))
			(error "cannot redefine")))) ; dont allow to redefine in the same scope

	;; Setter
	(define dict-set! (lambda (name value)
		(let ((cell (vector-ref dict (hash name))))
			(find-on-list cell name (lambda (lst) (set-cdr! (car lst) value))
				'set! (list name value)))))
	;; Getter
	(define dict-get (lambda (name)
		(if (symbol? name)
			(find-on-list (vector-ref dict (hash name)) name cdar 'get (list name))
			'())))

	(lambda (cmd)
		(cond
			((eqv? cmd 'dump) (display dict) (newline))
			((eqv? cmd 'define) dict-define!)
			((eqv? cmd 'set!) dict-set!)
			((eqv? cmd 'get) dict-get)))))

;;; Evaluator
(define (make-evaluator)

	;; Basic expressions 
	;;----------------------------------------------------------------------------------------------

	(define self? (lambda (expr)
		(or
			(char? expr)
			(number? expr)
			(string? expr)
			(boolean? expr)
			(vector? expr))))

	(define variable? (lambda (expr) (symbol? expr)))

	(define quoted? (lambda (expr) (eqv? (car expr) 'quote)))

	;(define quotate (lambda (expr) (cadr expr)))
	(define quotate (lambda (expr) expr))

	;; Assignment expression
	;;----------------------------------------------------------------------------------------------

	(define assignment? (lambda (expr) 
		(and (eqv? (car expr) 'set!) (symbol? (car expr)))))

	(define assignment-name (lambda (expr) (cadr expr)))

	(define assignment-value (lambda (expr) (caddr expr)))

	;; Definition expression
	;;----------------------------------------------------------------------------------------------

	(define definition? (lambda (expr) 
		(and (eqv? (car expr) 'define) (symbol? (car expr)))))

	(define definition-name (lambda (expr) (cadr expr)))

	(define definition-value (lambda (expr) (caddr expr)))

	;; Conditional expression
	;;----------------------------------------------------------------------------------------------

	(define if? (lambda (expr) (eqv? (car expr) 'if)))

	(define if-predicate (lambda (expr) (cadr expr)))

	(define if-consequent (lambda (expr) (caddr expr)))

	(define if-alternative (lambda (expr)
		(if (not (null? (cdddr expr)))
			(cadddr expr)
			'())))

	;; Function creation expression
	;;----------------------------------------------------------------------------------------------

	(define lambda? (lambda (expr) (eqv? (car expr) 'lambda)))

	(define macro? (lambda (expr) (eqv? (car expr) 'macro)))

	(define parameters (lambda (expr) (cadr expr)))

	(define body (lambda (expr) (cddr expr)))

	(define make-function (lambda (parameters body env)
		(list 'function parameters body env)))

	(define make-macro (lambda (parameters body env)
		(list 'macro-function parameters body env)))

	;; Procedure handler
	;;----------------------------------------------------------------------------------------------

	(define function? (lambda (prc) (or (procedure? prc) (eqv? (car prc) 'function))))

	(define macro-function? (lambda (prc) (eqv? (car prc) 'macro-function)))

	(define procedure-body (lambda (prc) (caddr prc)))

	(define procedure-parameters (lambda (prc) (cadr prc)))

	(define procedure-env (lambda (prc) (cadddr prc)))

	;; Procedure Call Expression
	;;----------------------------------------------------------------------------------------------

	(define call? (lambda (expr) (pair? expr)))

	(define operator (lambda (expr) (car expr)))

	(define operands (lambda (expr) (cdr expr)))

	;; Apply
	;;----------------------------------------------------------------------------------------------

	(define init-parameters (lambda (prms args env)
		(if (and (null? prms) (null? args))
			'()
			(if (eqv? (car prms) '...)
				((env 'define) '... args)
				(begin 
					((env 'define) (car prms) (car args))
					(init-parameters (cdr prms) (cdr args) env))))))

	(define sequence-eval (lambda (exprs env)
		(if (not (null? (cdr exprs)))
			(begin 
				(_eval (car exprs) env)
				(sequence-eval (cdr exprs) env))
			(_eval (car exprs) env))))

	(define _apply (lambda (operator operands)
		(cond
			((procedure? operator) ;;primitive procedure NOT user defined procedure
				(apply operator operands))
			(else
				(let ((env (make-environment (procedure-env operator))))
					((env 'define) '_env env)

					(init-parameters (procedure-parameters operator) operands env)
					(sequence-eval (procedure-body operator) env))))))
					
	;; Eval
	;;----------------------------------------------------------------------------------------------

	(define eval-assignment (lambda (expr env)
		((env 'set!) (assignment-name expr) (_eval (assignment-value expr) env))))

	(define eval-definition (lambda (expr env)
			((env 'define) (definition-name expr) (_eval (definition-value expr) env))))

	(define eval-if (lambda (expr env)
		(if (_eval (if-predicate expr) env)
			(_eval (if-consequent expr) env)
			(_eval (if-alternative expr) env))))

	(define eval-eager-operands (lambda (lst env)
		(if (null? lst)
			'()
			(cons
				(_eval (car lst) env)
				(eval-eager-operands (cdr lst) env)))))

	(define eval-lazy-operands (lambda (lst env)
		(if (null? lst)
			'()
			(cons
				(car lst)
				(eval-lazy-operands (cdr lst) env)))))

	(define _eval (lambda (expr env)
		(cond
			((null? expr) '())
			((self? expr) expr)
			((variable? expr) ((env 'get) expr))
			((quoted? expr) (quotate expr))
			((assignment? expr) (eval-assignment expr env) 'done)
			((definition? expr) (eval-definition expr env) 'done)
			((if? expr) (eval-if expr env))
			((lambda? expr) 
				(make-function
					(parameters expr)
					(body expr) env))
			((macro? expr) 
				(make-macro
					(parameters expr)
					(body expr) env))
			((call? expr)
				(let ((prc (_eval (operator expr) env)))
					(if (function? prc)
						(_apply 
							prc
							(eval-eager-operands (operands expr) env))
						(_eval (_apply 
							prc
							(eval-lazy-operands (operands expr) env)) env))))

			(else "unknown expression"))))

	(lambda (cmd)
		(cond
			((eqv? cmd 'eval) _eval)
			((eqv? cmd 'apply) _apply))))

;; Nanoscheme core
;;--------------------------------------------------------------------------------------------------
(define nanoscheme-core (lambda ()
	(define env (make-environment '()))
	(define evaluator (make-evaluator))

	((env 'define) '_env env)
	((env 'define) 'eval (evaluator 'eval))
	((env 'define) 'apply (evaluator 'apply))
	((env 'define) 'empty-environment (lambda () (make-environment '())))

	((evaluator 'eval) '(define evaluate (macro (expr) (eval expr _env))) env)

	(lambda (cmd)
		(cond 
			((eqv? cmd 'environment) env)
			((eqv? cmd 'eval)
				(lambda (expr) ((evaluator 'eval) expr env)))))))

;; Regular Environment
;;--------------------------------------------------------------------------------------------------
(define nanoscheme (lambda ()
	(define core (nanoscheme-core))
	
	(((core 'environment) 'define) 'car car)
	(((core 'environment) 'define) 'cdr cdr)
	(((core 'environment) 'define) 'cons cons)
	(((core 'environment) 'define) 'set-car! set-car!)
	(((core 'environment) 'define) 'set-cdr! set-cdr!)

	(((core 'environment) 'define) '+ +)
	(((core 'environment) 'define) '* *)
	(((core 'environment) 'define) '- -)
	(((core 'environment) 'define) '/ /)
	(((core 'environment) 'define) 'expt expt)
	(((core 'environment) 'define) 'modulo modulo)

	(((core 'environment) 'define) 'not not)
	(((core 'environment) 'define) '< <)
	(((core 'environment) 'define) '<= <=)
	(((core 'environment) 'define) '> >)
	(((core 'environment) 'define) '>= >=)
	(((core 'environment) 'define) '= =)
	(((core 'environment) 'define) 'eq? eq?)
	(((core 'environment) 'define) 'eqv? eqv?)
	(((core 'environment) 'define) 'equal? equal?)

	(((core 'environment) 'define) 'null? null?)
	(((core 'environment) 'define) 'pair? pair?)
	(((core 'environment) 'define) 'char? char?)
	(((core 'environment) 'define) 'symbol? symbol?)
	(((core 'environment) 'define) 'number? number?)
	(((core 'environment) 'define) 'vector? vector?)
	(((core 'environment) 'define) 'string? string?)
	(((core 'environment) 'define) 'boolean? boolean?)

	(((core 'environment) 'define) 'integer? integer?)
	(((core 'environment) 'define) 'real? real?)
	(((core 'environment) 'define) 'complex? complex?)

	(((core 'environment) 'define) 'string->number string->number)
	(((core 'environment) 'define) 'string->symbol string->symbol)
	(((core 'environment) 'define) 'symbol->string symbol->string)
	(((core 'environment) 'define) 'number->string number->string)

	(((core 'environment) 'define)
		'procedure? 
		(lambda (prc) (or (procedure? prc) (eqv? (car prc) 'function) (eqv? (car prc) 'macro-function))))
	(((core 'environment) 'define) 'function? (lambda (prc) (eqv? (car prc) 'function)))
	(((core 'environment) 'define) 'macro? (lambda (prc) (eqv? (car prc) 'macro-function)))

	(((core 'environment) 'define) 'make-rectangular make-rectangular)
	(((core 'environment) 'define) 'make-polar make-polar)
	(((core 'environment) 'define) 'make-string make-string)
	(((core 'environment) 'define) 'string-length string-length)
	(((core 'environment) 'define) 'string-ref string-ref)
	(((core 'environment) 'define) 'make-vector make-vector)
	(((core 'environment) 'define) 'vector-length vector-length)
	(((core 'environment) 'define) 'vector-ref vector-ref)

	(((core 'environment) 'define) 'display display)
	(((core 'environment) 'define) 'read read)
	(((core 'environment) 'define) 'error error)
	(((core 'environment) 'define) 'exit exit)

	((core 'eval)
		'(define and (macro (...)
			(define and (lambda (stmts)
				(if (null? stmts)
					#t  
					(list 'if (car stmts) (and (cdr stmts)) #f))))
			(and ...))))
	
	((core 'eval)
		'(define or (macro (...)
			(define or (lambda (stmts)
				(if (null? stmts)
					#f
					(list 'if (car stmts) (or (cdr stmts)) #t))))
			(or ...))))

	core))

;; Full Scheme Environment
;;--------------------------------------------------------------------------------------------------
(define nanoscheme-full (lambda ()
	(define core (nanoscheme))

	(((core 'environment) 'define) 'string-fill! string-fill!)
	(((core 'environment) 'define) 'vector-fill! vector-fill!)

	(((core 'environment) 'define) 'list list)
	(((core 'environment) 'define) 'list? list?)
	(((core 'environment) 'define) 'list-ref list-ref)
	(((core 'environment) 'define) 'reverse reverse)
	(((core 'environment) 'define) 'append append)

	(((core 'environment) 'define) 'list->vector list->vector)
	(((core 'environment) 'define) 'vector->list vector->list)

	(((core 'environment) 'define) 'caar caar)
	(((core 'environment) 'define) 'cadr cadr)
	(((core 'environment) 'define) 'cdar cdar)
	(((core 'environment) 'define) 'cddr cddr)

	(((core 'environment) 'define) 'caaar caaar)
	(((core 'environment) 'define) 'caadr caadr)
	(((core 'environment) 'define) 'cadar cadar)
	(((core 'environment) 'define) 'caddr caddr)
	(((core 'environment) 'define) 'cdaar cdaar)
	(((core 'environment) 'define) 'cdadr cdadr)
	(((core 'environment) 'define) 'cddar cddar)
	(((core 'environment) 'define) 'cdddr cdddr)

	(((core 'environment) 'define) 'caaaar caaaar)
	(((core 'environment) 'define) 'caaadr caaadr)
	(((core 'environment) 'define) 'caadar caadar)
	(((core 'environment) 'define) 'caaddr caaddr)
	(((core 'environment) 'define) 'cadaar cadaar)
	(((core 'environment) 'define) 'cadadr cadadr)
	(((core 'environment) 'define) 'caddar caddar)
	(((core 'environment) 'define) 'cadddr cadddr)
	(((core 'environment) 'define) 'cdaaar cdaaar)
	(((core 'environment) 'define) 'cdaadr cdaadr)
	(((core 'environment) 'define) 'cdadar cdadar)
	(((core 'environment) 'define) 'cdaddr cdaddr)
	(((core 'environment) 'define) 'cddaar cddaar)
	(((core 'environment) 'define) 'cddadr cddadr)
	(((core 'environment) 'define) 'cdddar cdddar)
	(((core 'environment) 'define) 'cddddr cddddr)

	(((core 'environment) 'define) 'map map)

	((core 'eval) 
		'(define cond (macro (...) 
			(define cond (lambda (stmts)
				(if (null? stmts)
				''()
				(list 'if (car (car stmts)) (list (append '(lambda ()) (cdr (car stmts)))) (cond (cdr stmts))))))
		(cond ...))))
		

	((core 'eval)
		'(define begin (macro (...)
			(list (append '(lambda ()) ...)))))

	((core 'eval)
		'(define let (macro (tuples ...)
			(append (list (append (append '(lambda) (list (map car tuples))) ...)) (map cadr tuples)))))

	core))
