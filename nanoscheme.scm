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

	(define defined? (lambda (lst name)
		(cond
			((null? lst) #f)
			((eqv? (caar lst) name) #t)
			(else (defined? (cdr lst) name)))))

	;; Hash function
	(define hash (lambda (name)
		(modulo (string-length (symbol->string name)) cells-count)))

	;; Definer
	(define dict-define! (lambda (name value)
		(if (not (defined? (vector-ref dict (hash name)) name))
			(begin 
				(vector-set! dict (hash name)
					(cons (cons name value) (vector-ref dict (hash name))))
				"defined")
			"cannot redefine"))) ; dont allow to redefine in the same scope

	;; Setter
	(define dict-set! (lambda (name value)
		(let ((cell (vector-ref dict (hash name))))
			(if
				(eqv? (find-on-list cell 
							name 
							(lambda (lst) (set-cdr! (car lst) value))
							'set! (list name value)) '())
					"cannot set undefined variable"
					"setted"))))

	;; Getter
	(define dict-get (lambda (name)
		(find-on-list (vector-ref dict (hash name)) name cdar 'get (list name))))

	(lambda (cmd)
		(cond
			((eqv? cmd 'dump) (display dict) (newline) '())
			((eqv? cmd 'define) dict-define!)
			((eqv? cmd 'set!) dict-set!)
			((eqv? cmd 'get) dict-get)))))

;;; Evaluator
(define make-evaluator (lambda ()

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

	(define quotate (lambda (expr) (cadr expr)))

	(define unquoted? (lambda (expr) (eqv? (car expr) 'unquote)))

	(define unquotate (lambda (expr env) (_eval (_eval (cadr expr) env) env)))

	;; Assignment expression
	;;----------------------------------------------------------------------------------------------

	(define assignment? (lambda (expr) 
		(and (eqv? (car expr) 'set!) (symbol? (car expr)))))

	(define assignment-name (lambda (expr) (cadr expr)))

	(define assignment-value (lambda (expr) (caddr expr)))

	;; Definition expression
	;;----------------------------------------------------------------------------------------------

	(define macro-sign "&")

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

	(define init-parameters (lambda (prms args env prefix)
		(if (and (null? prms) (null? args))
			'()
			(if (eqv? (car prms) '...)
				((env 'define) (string->symbol (string-append prefix "...")) args)
				(begin 
					((env 'define) 
						(string->symbol (string-append prefix (symbol->string (car prms))) )
						(car args))
					(init-parameters (cdr prms) (cdr args) env prefix))))))

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
					((env 'define) 'callback operator)
					
					(init-parameters (procedure-parameters operator) operands env
						(if (or (macro-function? operator) (eqv? ((env 'get) '&) #t)) 
							(begin ((env 'define) '& #t) "&") 
							""))
					(sequence-eval (procedure-body operator) env))))))
					
	;; Eval
	;;----------------------------------------------------------------------------------------------

	(define eval-name (lambda (expr env)
		(if (symbol? expr)
			expr
			(eval-name (_eval expr env) env))))

	(define eval-assignment (lambda (expr env)
		((env 'set!) (eval-name (assignment-name expr) env) (_eval (assignment-value expr) env))))

	(define eval-definition (lambda (expr env)
		(if (eqv? (string-ref (symbol->string (definition-name expr)) 0) #\&)
			"invalid name"
			((env 'define) (eval-name (definition-name expr) env) (_eval (definition-value expr) env)))))

	(define eval-if (lambda (expr env)
		(if (_eval (if-predicate expr) env)
			(_eval (if-consequent expr) env)
			(_eval (if-alternative expr) env))))
	
	(define eval-parameters (lambda (params env)
		(cond
			((null? params) '())
			((symbol? (car params)) 
				(cons (car params) (eval-parameters (cdr params) env)))
			(else (eval-parameters (cons (_eval (car params) env) (cdr params)) env)))))

	(define eval-eager-operands (lambda (lst env)
		(if (null? lst)
			'()
			(cons (_eval (car lst) env) (eval-eager-operands (cdr lst) env)))))

	(define eval-literal-operands (lambda (lst env) lst))

	(define _eval (lambda (expr env)
		(cond
			((null? expr) '())
			((self? expr) expr)
			((variable? expr) ((env 'get) expr))
			((quoted? expr) (quotate expr))
			((unquoted? expr) (unquotate expr env))
			((assignment? expr) (eval-assignment expr env))
			((definition? expr) (eval-definition expr env))
			((if? expr) (eval-if expr env))
			((lambda? expr) 
				(make-function
					(eval-parameters (parameters expr) env)
					(body expr) env))
			((macro? expr) 
				(make-macro
					(eval-parameters (parameters expr) env)
					(body expr) env))
			((call? expr)
				(let ((prc (_eval (operator expr) env)))
					(if (not (null? prc))
						(if (function? prc)
							(_apply prc (eval-eager-operands (operands expr) env))
							(_apply prc (eval-literal-operands (operands expr) env)))
						'())))

			(else "unknown expression"))))

	(lambda (cmd)
		(cond
			((eqv? cmd 'eval) _eval)
			((eqv? cmd 'apply) _apply)))))

;; Nanoscheme core
;;--------------------------------------------------------------------------------------------------
(define nanoscheme-core (lambda ()
	(define env (make-environment '()))
	(define evaluator (make-evaluator))

	((env 'define) '_env env)
	((env 'define) 'eval (evaluator 'eval))
	((env 'define) 'apply (evaluator 'apply))
	((env 'define) 'empty-environment (lambda () (make-environment '())))

	(lambda (cmd)
		(cond 
			((eqv? cmd 'environment) env)
			((eqv? cmd 'eval)
				(lambda (expr) ((evaluator 'eval) expr env)))))))

;; Lightweight Environment
;;--------------------------------------------------------------------------------------------------
(define nanoscheme (lambda ()
	(define core (nanoscheme-core))

	((core 'eval) 
		'(define or (macro (...)
			((lambda (stmts)
				(if (null? &stmts)
					#f
					(if ,(car &stmts)
						#t
						(callback (cdr &stmts))))) &...))))

	((core 'eval) 
		'(define and (macro (...)
			((lambda (stmts)
				(if (null? &stmts)
					#t
					(if ,(car &stmts)
						(callback (cdr &stmts))
						#f))) &...))))
	
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
	(((core 'environment) 'define) 'string-set! string-set!)
	(((core 'environment) 'define) 'string-append string-append)
	(((core 'environment) 'define) 'make-vector make-vector)
	(((core 'environment) 'define) 'vector-length vector-length)
	(((core 'environment) 'define) 'vector-ref vector-ref)
	(((core 'environment) 'define) 'vector-set! vector-set!)

	(((core 'environment) 'define) 'display (lambda (text) (display text) '()))
	(((core 'environment) 'define) 'read read)
	(((core 'environment) 'define) 'exit exit)

	core))

;; Full Scheme Environment
;;--------------------------------------------------------------------------------------------------
(define nanoscheme-full (lambda ()
	(define core (nanoscheme))

	((core 'eval)
		'(define map (lambda (prc lst)
			(if (null? lst)
				'()
				(cons (prc (car lst)) (map prc (cdr lst)))))))

	((core 'eval) 
		'(define begin (macro (...)
			,(cons (append '(lambda ()) &...) '()))))

	((core 'eval)
		'(define cond (macro (...)
			(define else #t)
			((lambda (stmts)
				(if (null? &stmts)
					'()
					(if ,(caar &stmts)
						,(cons (append '(lambda ()) (cdar &stmts)) '())
						(callback (cdr &stmts))))) &...))))

	((core 'eval)
		'(define let (macro (tuples ...)
			(set! & #f)
			,(append 
				(cons (append 
					(append '(lambda) (cons 
						(map car &tuples)
						'()))
					&...) '()) 
				(map cadr &tuples)))))

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

	core))
