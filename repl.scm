;; Example for a simple repl

(load "nanoscheme.scm")

(define nscm (nanoscheme-full))

(define (repl)
	(display ">>")
	(display ((nscm 'eval) (read)))
	(newline)
	(repl))

(repl)
