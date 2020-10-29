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
						(display name) (newline) '())
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
						(display name) (newline) '())
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

(define quotate (lambda (expr) (cadr expr)))

(define unquoted? (lambda (expr) (eqv? (car expr) 'unquote)))

(define unquotate (lambda (expr env)
	(nscm-eval (nscm-eval (cadr expr) env) env)))
 

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

(define parameters (lambda (expr) (cadr expr)))

(define body (lambda (expr) (cddr expr)))

(define make-function (lambda (parameters body env)
	(list 'FNC parameters body env)))

;; Procedure handler
;;----------------------------------------------------------------------------------------------

(define function? (lambda (prc) (or (procedure? prc) (eqv? (car prc) 'FNC))))

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
			(if (not (null? (cdr prms))) ((env 'def) (cadr prms) args))
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

(define nscm-eval (lambda (expr env)
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
				(parameters expr)
				(body expr) env))
		((call? expr)
			(let ((prc (nscm-eval (operator expr) env)))
				(cond
					((null? prc) '())
					((function? prc)
						(nscm-apply prc (eval-operands (operands expr) env)))
					(else '()))))
		(else (error "unknown expression")))))

