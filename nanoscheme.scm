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
			(if (eqv? (find-on-list cell   ; '() is evaluated as #t
						name 
						(lambda (lst) (set-cdr! (car lst) value))
						'set (list name value)) '())
				"cannot set undefined variable"
				"setted"))))

	;; Getter
	(define dict-get (lambda (name)
		(find-on-list (vector-ref dict (hash name)) name cdar 'get (list name))))

	(lambda (cmd)
		(cond
			((eqv? cmd 'def) dict-define!)
			((eqv? cmd 'set) dict-set!)
			((eqv? cmd 'get) dict-get)))))


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

(define quoted? (lambda (expr) (eqv? (car expr) 'quote)))

(define quotate (lambda (expr) (cadr expr)))

(define unquoted? (lambda (expr) (eqv? (car expr) 'unquote)))

(define unquotate (lambda (expr env) (_eval (_eval (cadr expr) env) env)))

;; Assignment expression
;;----------------------------------------------------------------------------------------------

(define assignment? (lambda (expr) 
	(and (eqv? (car expr) 'set) (symbol? (car expr)))))

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
		(car (cdddr expr))
		'())))

;; Function creation expression
;;----------------------------------------------------------------------------------------------

(define lambda? (lambda (expr) (eqv? (car expr) 'lambda)))

(define macro? (lambda (expr) (eqv? (car expr) 'macro)))

(define parameters (lambda (expr) (cadr expr)))

(define body (lambda (expr) (cddr expr)))

(define make-function (lambda (parameters body env)
	(list 'FNC parameters body env)))

(define make-macro (lambda (parameters body env)
	(list 'MCR parameters body env)))

;; Procedure handler
;;----------------------------------------------------------------------------------------------

(define function? (lambda (prc) (or (procedure? prc) (eqv? (car prc) 'FNC))))

(define macro-function? (lambda (prc) (eqv? (car prc) 'MCR)))

(define procedure-body (lambda (prc) (caddr prc)))

(define procedure-parameters (lambda (prc) (cadr prc)))

(define procedure-env (lambda (prc) (car (cdddr prc))))

;; Procedure Call Expression
;;----------------------------------------------------------------------------------------------

(define call? (lambda (expr) (pair? expr)))

(define operator (lambda (expr) (car expr)))

(define operands (lambda (expr) (cdr expr)))

;; Apply
;;----------------------------------------------------------------------------------------------

(define variadic? (lambda (prms)
	(eqv? (car prms) '...)))

(define init-parameters (lambda (prms args env)
	(if (and (null? prms) (null? args))
		'()
		(if (variadic? prms)
			(if (not (null? (cdr prms))) ((env 'def) (cadr prms) args))
			(begin 
				((env 'def) (car prms) (car args))
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

		((or (function? operator) (macro-function? operator))
			(let ((env (make-environment (procedure-env operator))))

				(init-parameters (procedure-parameters operator) operands env)

				(sequence-eval (procedure-body operator) env)))

		(else (error "Cannot apply a non procedure")))))
			

;; Eval
;;----------------------------------------------------------------------------------------------

(define eval-name (lambda (expr env)
	(if (or (symbol? expr) (self? expr))
		expr
		(eval-name (_eval expr env) env))))

(define eval-assignment (lambda (expr env)
	(let ((name (eval-name (assignment-name expr) env)))
		((env 'set) name (_eval (assignment-value expr) env)))))

(define eval-definition (lambda (expr env)
	(let ((name (eval-name (definition-name expr) env)))
			((env 'def) name (_eval (definition-value expr) env)))))

(define eval-if (lambda (expr env)
	(if (_eval (if-predicate expr) env)
		(_eval (if-consequent expr) env)
		(_eval (if-alternative expr) env))))

(define eval-parameters (lambda (params env)
	(cond
		((null? params) '())
		((symbol? (car params))
			(cons (car params) (eval-parameters (cdr params) env)))
		(else (error "invalid parameters list given")))))

(define eval-eager-operands (lambda (lst env)
	(if (null? lst)
		'()
		(cons (_eval (car lst) env) (eval-eager-operands (cdr lst) env)))))

(define eval-literal-operands (lambda (lst env) lst))

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
				(body expr) env))
		((call? expr)
			(let ((prc (_eval (operator expr) env)))
				(if (not (null? prc))
					(if (function? prc)
						(_apply prc (eval-eager-operands (operands expr) env))
						(_apply prc (eval-literal-operands (operands expr) env)))
					(begin (display "call of not a function\n") '()))))
		(else (error "unknown expression")))))


;; Nanoscheme core
;;--------------------------------------------------------------------------------------------------
(define nanoscheme-core (lambda ()
	(define env (make-environment '()))

	((env 'def) 'eval _eval)
	((env 'def) 'apply _apply)
	((env 'def) 'empty-environment (lambda () (make-environment '())))

	(lambda (cmd)
		(cond 
			((eqv? cmd 'environment) env)
			((eqv? cmd 'eval)
				(lambda (expr) (_eval expr env)))))))

;; Lightweight Environment
;;--------------------------------------------------------------------------------------------------
(define nanoscheme (lambda ()
	(define core (nanoscheme-core))

	((core 'eval) 
		'(define or (macro (... *or_STMTS*)
			(define *or_HELPER* (lambda (*or_STMTS*)
				(if (null? *or_STMTS*)
					#f
					(if ,(car *or_STMTS*)
						#t
						(*or_HELPER* (cdr *or_STMTS*)))))) 
			(*or_HELPER* *or_STMTS*))))

	((core 'eval) 
		'(define and (macro (... *and_STMTS*)
			(define *and_HELPER* (lambda (*and_STMTS*)
				(if (null? *and_STMTS*)
					#t
					(if (not ,(car *and_STMTS*))
						#f
						(*and_HELPER* (cdr *and_STMTS*)))))) 
			(*and_HELPER* *and_STMTS*))))
	
	(((core 'environment) 'def) 'car car)
	(((core 'environment) 'def) 'cdr cdr)
	(((core 'environment) 'def) 'cons cons)
	(((core 'environment) 'def) 'set-car! set-car!)
	(((core 'environment) 'def) 'set-cdr! set-cdr!)

	(((core 'environment) 'def) '+ +)
	(((core 'environment) 'def) '* *)
	(((core 'environment) 'def) '- -)
	(((core 'environment) 'def) '/ /)
	(((core 'environment) 'def) 'expt expt)
	(((core 'environment) 'def) 'modulo modulo)

	(((core 'environment) 'def) 'not not)
	(((core 'environment) 'def) '< <)
	(((core 'environment) 'def) '<= <=)
	(((core 'environment) 'def) '> >)
	(((core 'environment) 'def) '>= >=)
	(((core 'environment) 'def) '= =)
	(((core 'environment) 'def) 'eq? eq?)
	(((core 'environment) 'def) 'eqv? eqv?)
	(((core 'environment) 'def) 'equal? equal?)

	(((core 'environment) 'def) 'null? null?)
	(((core 'environment) 'def) 'pair? pair?)
	(((core 'environment) 'def) 'char? char?)
	(((core 'environment) 'def) 'symbol? symbol?)
	(((core 'environment) 'def) 'number? number?)
	(((core 'environment) 'def) 'vector? vector?)
	(((core 'environment) 'def) 'string? string?)
	(((core 'environment) 'def) 'boolean? boolean?)

	(((core 'environment) 'def) 'integer? integer?)
	(((core 'environment) 'def) 'real? real?)
	(((core 'environment) 'def) 'complex? complex?)

	(((core 'environment) 'def) 'string->number string->number)
	(((core 'environment) 'def) 'string->symbol string->symbol)
	(((core 'environment) 'def) 'symbol->string symbol->string)
	(((core 'environment) 'def) 'number->string number->string)

	(((core 'environment) 'def)
		'procedure? 
		(lambda (prc) (or (procedure? prc) (eqv? (car prc) 'FNC) (eqv? (car prc) 'MCR))))

	(((core 'environment) 'def) 'make-rectangular make-rectangular)
	(((core 'environment) 'def) 'make-polar make-polar)
	(((core 'environment) 'def) 'make-string make-string)
	(((core 'environment) 'def) 'string-length string-length)
	(((core 'environment) 'def) 'string-ref string-ref)
	(((core 'environment) 'def) 'string-set! string-set!)
	(((core 'environment) 'def) 'string-append string-append)
	(((core 'environment) 'def) 'make-vector make-vector)
	(((core 'environment) 'def) 'vector-length vector-length)
	(((core 'environment) 'def) 'vector-ref vector-ref)
	(((core 'environment) 'def) 'vector-set! vector-set!)

	(((core 'environment) 'def) 'display (lambda (text) (display text) (newline) '()))
	(((core 'environment) 'def) 'read read)
	(((core 'environment) 'def) 'exit exit)
	(((core 'environment) 'def) 'error error)

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
		'(define begin (macro (... *begin_PRM*)
			,(cons (append (list 'lambda '()) *begin_PRM*) '()))))

	((core 'eval)
		'(define cond (macro (... *cond_STMTS*)
			(define else #t)
			(define *cond_HELPER* (lambda (*cond_STMTS*)
				(if (null? *cond_STMTS*)
					'()
					(if ,(caar *cond_STMTS*)
						,(cons (append '(lambda ()) (cdar *cond_STMTS*)) '())
						(*cond_HELPER* (cdr *cond_STMTS*))))))

			(*cond_HELPER* *cond_STMTS*))))

	((core 'eval)
		'(define let (macro (*let_TUPLES* ... *let_REST*)
			,(append 
				(cons (append 
					(append '(lambda) (cons 
						(map car *let_TUPLES*)
						'()))
					*let_REST*) '())
				(map cadr *let_TUPLES*)))))

	(((core 'environment) 'def) 'string-fill! string-fill!)
	(((core 'environment) 'def) 'vector-fill! vector-fill!)

	(((core 'environment) 'def) 'list list)
	(((core 'environment) 'def) 'list? list?)
	(((core 'environment) 'def) 'list-ref list-ref)
	(((core 'environment) 'def) 'reverse reverse)
	(((core 'environment) 'def) 'append append)
	(((core 'environment) 'def) 'member member)

	(((core 'environment) 'def) 'list->vector list->vector)
	(((core 'environment) 'def) 'vector->list vector->list)

	(((core 'environment) 'def) 'caar caar)
	(((core 'environment) 'def) 'cadr cadr)
	(((core 'environment) 'def) 'cdar cdar)
	(((core 'environment) 'def) 'cddr cddr)
	(((core 'environment) 'def) 'caaar caaar)
	(((core 'environment) 'def) 'caadr caadr)
	(((core 'environment) 'def) 'cadar cadar)
	(((core 'environment) 'def) 'caddr caddr)
	(((core 'environment) 'def) 'cdaar cdaar)
	(((core 'environment) 'def) 'cdadr cdadr)
	(((core 'environment) 'def) 'cddar cddar)
	(((core 'environment) 'def) 'cdddr cdddr)

	core))
