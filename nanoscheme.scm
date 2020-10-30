;; Nanoscheme
;; Copyright (C) 2020  Oren Daniel

;; This file is licensed under the terms of the BSD 3-Clause License.

;; Environment 
;;----------------------------------------------------------------------------------------------

; every environment has an outer environment pointer
; that represent the outer lexical scope
(define make-environment (lambda (outer)
	
	; for simplicity the environment will be represented as an assoc-list
	(define dict '())

	;; Definer
	(define dict-define! (lambda (name value)
		(set! dict (cons (cons name value) dict)) "defined"))

	;; Setter
	(define dict-set! (lambda (name value)
		(define helper (lambda (lst)
			(cond
				((null? lst) 
					(if (null? outer) 
					(begin (display "cannot set undefined entry - ") 
						(display name) (display "\n") '())
						((outer 'set) name value)))
				((eqv? (caar lst) name) (set-cdr! (car lst) value) '())
				(else (helper (cdr lst))))))
		(helper dict)))

	;; Getter
	(define dict-get (lambda (name)
		(let ((res (assoc name dict)))
			(if res
				(cdr res)
				(if (null? outer) 
					(begin (display "cannot get undefined entry - ") 
						(display name) (display "\n") '())
					((outer 'get) name))))))

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

(define quotate (lambda (expr env)
	(define helper (lambda (lst)
		(cond
			((null? lst) '())
			((not (pair? lst)) lst)
			((and (pair? (car lst)) (eqv? (caar lst) 'unquote))
				(cons (nscm-eval (helper (cadar lst)) env) (helper (cdr lst))))
			((and (pair? (car lst)) (eqv? (caar lst) 'unquote-splicing))
				(append (nscm-eval (helper (cadar lst)) env) (helper (cdr lst))))
			(else
				(cons (helper (car lst)) (helper (cdr lst)))))))
	(helper (cadr expr))))


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

(define make-expander (lambda (parameters body env)
	(list 'MCR parameters body env)))

;; Procedure handler
;;----------------------------------------------------------------------------------------------

(define function? (lambda (prc) (or (procedure? prc) (eqv? (car prc) 'FNC))))

(define expander? (lambda (prc) (eqv? (car prc) 'MCR)))

(define procedure-body (lambda (prc) (caddr prc)))

(define procedure-parameters (lambda (prc) (cadr prc)))

(define procedure-env (lambda (prc) (car (cdddr prc))))

;; Procedure Call Expression
;;----------------------------------------------------------------------------------------------

(define call? (lambda (expr) (pair? expr)))

(define operator (lambda (expr) (car expr)))

(define operands (lambda (expr) (cdr expr)))

;; Apply and Expand
;;----------------------------------------------------------------------------------------------

(define variadic? (lambda (prms)
	(eqv? (car prms) '...)))

(define init-parameters (lambda (prms args env)
	(if (and (null? prms) (null? args))
		'()
		(if (variadic? prms)
			(if (not (null? (cdr prms)))
				((env 'def) (cadr prms) args))
			(begin 
				((env 'def) (car prms) (car args))
				(init-parameters (cdr prms) (cdr args) env))))))

(define sequence-eval (lambda (exprs env)
	(if (not (null? (cdr exprs)))
		(begin 
			(nscm-eval (car exprs) env)
			(sequence-eval (cdr exprs) env))
		(nscm-eval (car exprs) env))))

(define nscm-apply (lambda (operator operands)
	(cond
		((procedure? operator) ;;primitive procedure NOT user defined procedure
			(apply operator operands))

		((function? operator)
			(let ((env (make-environment (procedure-env operator))))

				(init-parameters (procedure-parameters operator) operands env)

				(sequence-eval (procedure-body operator) env)))

		(else (error "Cannot apply a non procedure")))))

(define nscm-expand (lambda (operator operands caller-env)
	(cond
		((expander? operator)
			(let ((env (make-environment (procedure-env operator))))

				(init-parameters (procedure-parameters operator) operands env)

				(nscm-eval (sequence-eval (procedure-body operator) env) caller-env)))

		(else (error "Cannot expand a non procedure")))))

;; Eval
;;----------------------------------------------------------------------------------------------

(define eval-name (lambda (expr env)
	(if (or (symbol? expr) (self? expr))
		expr
		(eval-name (nscm-eval expr env) env))))

(define eval-assignment (lambda (expr env)
	(let ((name (eval-name (assignment-name expr) env)))
		((env 'set) name (nscm-eval (assignment-value expr) env)))))

(define eval-definition (lambda (expr env)
	(let ((name (eval-name (definition-name expr) env)))
			((env 'def) name (nscm-eval (definition-value expr) env)))))

(define eval-if (lambda (expr env)
	(if (nscm-eval (if-predicate expr) env)
		(nscm-eval (if-consequent expr) env)
		(nscm-eval (if-alternative expr) env))))

(define eval-operands (lambda (lst env)
	(if (null? lst)
		'()
		(cons (nscm-eval (car lst) env) (eval-operands (cdr lst) env)))))

(define eval-literals (lambda (lst) lst))

(define nscm-eval (lambda (expr env)
	(cond
		((self? expr) expr)
		((variable? expr) ((env 'get) expr))
		((quoted? expr) (quotate expr env))
		((assignment? expr) (eval-assignment expr env))
		((definition? expr) (eval-definition expr env))
		((if? expr) (eval-if expr env))
		((lambda? expr) 
			(make-function (parameters expr) (body expr) env))
		((macro? expr)
			(make-expander (parameters expr) (body expr) env))
		((call? expr)
			(let ((prc (nscm-eval (operator expr) env)))
				(cond
					((null? prc) '())
					((function? prc)
						(nscm-apply prc (eval-operands (operands expr) env)))
					((expander? prc)
						(nscm-expand prc (eval-literals (operands expr)) env)))))
		(else (error "unknown expression")))))
