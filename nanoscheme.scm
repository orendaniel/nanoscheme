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

(define s-quote 'quote)
(define s-unquote 'unquote)
(define s-set! 'set!) ; the syntatic keyword NOT the operation for environments
(define s-define 'define)
(define s-if 'if)
(define s-lambda 'lambda)
(define s-macro 'macro)
(define s-dotted-macro 'macro.)
(define s-guard '&)
(define s-callback '@)
(define s-variadic '...)
(define current-environment '_environment_)

;;; Evaluator
(define make-evaluator (lambda ()

	(define guarded-symbol? (lambda (sym)
		(eqv? (string-ref (symbol->string sym) 0) (string-ref (symbol->string s-guard) 0))))

	(define keysymbol? (lambda (sym) 
		(member sym (list s-quote s-unquote s-set! s-define s-if
						s-lambda s-macro s-dotted-macro s-guard 
						s-callback s-variadic current-environment))))

	;; Basic expressions 
	;;----------------------------------------------------------------------------------------------

	(define self? (lambda (expr)
		(or
			(null? expr)
			(char? expr)
			(number? expr)
			(string? expr)
			(boolean? expr)
			(vector? expr))))

	(define variable? (lambda (expr) (symbol? expr)))

	(define quoted? (lambda (expr) (eqv? (car expr) s-quote)))

	(define quotate (lambda (expr) (cadr expr)))

	(define unquoted? (lambda (expr) (eqv? (car expr) s-unquote)))

	(define unquotate (lambda (expr env) (_eval (_eval (cadr expr) env) env)))

	;; Assignment expression
	;;----------------------------------------------------------------------------------------------

	(define assignment? (lambda (expr) 
		(and (eqv? (car expr) s-set!) (symbol? (car expr)))))

	(define assignment-name (lambda (expr) (cadr expr)))

	(define assignment-value (lambda (expr) (caddr expr)))

	;; Definition expression
	;;----------------------------------------------------------------------------------------------

	(define definition? (lambda (expr) 
		(and (eqv? (car expr) s-define) (symbol? (car expr)))))

	(define definition-name (lambda (expr) (cadr expr)))

	(define definition-value (lambda (expr) (caddr expr)))

	;; Conditional expression
	;;----------------------------------------------------------------------------------------------

	(define if? (lambda (expr) (eqv? (car expr) s-if)))

	(define if-predicate (lambda (expr) (cadr expr)))

	(define if-consequent (lambda (expr) (caddr expr)))

	(define if-alternative (lambda (expr)
		(if (not (null? (cdddr expr)))
			(car (cdddr expr))
			'())))

	;; Function creation expression
	;;----------------------------------------------------------------------------------------------

	(define lambda? (lambda (expr) (eqv? (car expr) s-lambda)))

	(define macro? (lambda (expr)
		(or (eqv? (car expr) s-macro) (eqv? (car expr) s-dotted-macro))))

	(define guarded? (lambda (expr) (eqv? (car expr) s-macro)))

	(define parameters (lambda (expr) (cadr expr)))

	(define body (lambda (expr) (cddr expr)))

	(define valid-parameters? (lambda (params)
		(not (member #t (map guarded-symbol? params)))))

	(define make-function (lambda (parameters body env)
		(if (valid-parameters? parameters)
			(list 'function parameters body env)
			"invalid function")))

	(define make-macro (lambda (parameters body env guarded)
		(if (valid-parameters? parameters)
			(list 'macro-function parameters body guarded)
			"invalid macro")))

	;; Procedure handler
	;;----------------------------------------------------------------------------------------------

	(define function? (lambda (prc) (or (procedure? prc) (eqv? (car prc) 'function))))

	(define macro-function? (lambda (prc) (eqv? (car prc) 'macro-function)))

	(define guarded-macro? (lambda (mcr) (and (macro-function? mcr) (car (cdddr mcr)))))

	(define procedure-body (lambda (prc) (caddr prc)))

	(define procedure-parameters (lambda (prc) (cadr prc)))

	(define procedure-env (lambda (prc) (car (cdddr prc))))

	;; Procedure Call Expression
	;;----------------------------------------------------------------------------------------------

	(define call? (lambda (expr) (pair? expr)))

	(define operator (lambda (expr) (car expr)))

	(define operands (lambda (expr) (cdr expr)))

	;; Apply and Run
	;;----------------------------------------------------------------------------------------------

	(define init-parameters (lambda (prms args env prefix)
		(if (and (null? prms) (null? args))
			'()
			(if (eqv? (car prms) s-variadic)
				((env 'define) 
					(string->symbol (string-append prefix (symbol->string s-variadic))) args)
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
					((env 'define) s-callback operator)
					((env 'define) current-environment env)

					(init-parameters (procedure-parameters operator) operands env
						(if (eqv? ((env 'get) s-guard) #t)
							(symbol->string s-guard) ""))
					(sequence-eval (procedure-body operator) env))))))

	(define _run (lambda (operator operands env)
		(let ((mcr-env (make-environment env)))
			((mcr-env 'define) s-callback operator)
			((mcr-env 'define) current-environment mcr-env)

			(if (guarded-macro? operator)
				((mcr-env 'define) s-guard #t))

			(init-parameters 
				(procedure-parameters operator) operands mcr-env (symbol->string s-guard))
			(sequence-eval (procedure-body operator) mcr-env))))

	;; Eval
	;;----------------------------------------------------------------------------------------------

	(define eval-name (lambda (expr env)
		(if (or (symbol? expr) (self? expr))
			expr
			(eval-name (_eval expr env) env))))

	(define eval-assignment (lambda (expr env)
		(let ((name (eval-name (assignment-name expr) env)))
			(if (keysymbol? name)
				"cannot mutate the given variable"
				((env 'set!) name (_eval (assignment-value expr) env))))))

	(define eval-definition (lambda (expr env)
		(let ((name (eval-name (definition-name expr) env)))
			(if (or (keysymbol? name) (guarded-symbol? name))
				"cannot define variable for the given name"
				((env 'define) name (_eval (definition-value expr) env))))))

	(define eval-if (lambda (expr env)
		(if (_eval (if-predicate expr) env)
			(_eval (if-consequent expr) env)
			(_eval (if-alternative expr) env))))
	
	(define eval-parameters (lambda (params env)
		(cond
			((null? params) '())
			((or (symbol? (car params)) (self? (car params)))
				(cons (car params) (eval-parameters (cdr params) env)))
			(else (eval-parameters (cons (_eval (car params) env) (cdr params)) env)))))

	(define eval-eager-operands (lambda (lst env)
		(if (null? lst)
			'()
			(cons (_eval (car lst) env) (eval-eager-operands (cdr lst) env)))))

	(define eval-literal-operands (lambda (lst) lst))

	(define _eval (lambda (expr env)
		(cond
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
					(body expr) env (guarded? expr)))
			((call? expr)
				(let ((prc (_eval (operator expr) env)))
					(if (not (null? prc))
						(if (function? prc)
							(_apply prc (eval-eager-operands (operands expr) env))
							(_run prc (eval-literal-operands (operands expr)) env))
						"call of not a function")))
			(else "unknown expression"))))

	(lambda (cmd)
		(cond
			((eqv? cmd 'eval) _eval)
			((eqv? cmd 'apply) _apply)
			((eqv? cmd 'run) _run)))))

;; Nanoscheme core
;;--------------------------------------------------------------------------------------------------
(define nanoscheme-core (lambda ()
	(define env (make-environment '()))
	(define evaluator (make-evaluator))

	;((env 'define) current-environment env)
	((env 'define) 'eval (evaluator 'eval))
	((env 'define) 'apply (evaluator 'apply))
	((env 'define) 'run (evaluator 'run))
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
						(@ (cdr &stmts))))) &...))))

	((core 'eval) 
		'(define and (macro (...)
			((lambda (stmts)
				(if (null? &stmts)
					#t
					(if ,(car &stmts)
						(@ (cdr &stmts))
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
						(@ (cdr &stmts))))) &...))))

	((core 'eval)
		'(define let (macro. (tuples ...)
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
	(((core 'environment) 'define) 'member member)

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

	core))
