; StateList functions

;Check if a variable is declared
;Returns #t if the varname is found in state and #f otherwise
(define SL_check
	(lambda (varname state)
		(cond
			((null? (car state)) #f)
			((eq? varname (caar state)) #t)
			(else (SL_check varname (list (cdar state) (cdadr state))))
		)
	))

;Add a new pair of variable into the state list
;Returns a new state with varpair concatenated at the head
(define SL_add
	(lambda (varpair state)
		(cond
			((SL_check (car varpair) state) (error (string-append "The variable " (symbol->string (car varpair)) " is being redefined.")))
			((null? (car state)) (list (list (car varpair)) (cdr varpair)))
			(else (list (append (list (car varpair)) (car state)) (append (cdr varpair) (cadr state))))
		)
	))

;Remove a variable from the state list
;Returns a new state with the varname entry removed
(define SL_rm
	(lambda (varname state)
		(cond
			((null? (car state)) state)
			((eq? varname (caar state)) (list (cdar state) (cdadr state)))
			(else (SL_add (list (caar state) (caadr state)) (SL_rm varname (list (cdar state) (cdadr state)))))
		)
	))

;Get the variable value from the state list
;Returns the corresponding value for varname
(define SL_get
	(lambda (varname state)
		(cond
			((null? (car state)) (error (string-append "The variable " (symbol->string varname) " is not declared.")))
			((and (eq? varname (caar state)) (null? (caadr state))) (error (string-append "The variable " (symbol->string varname) " is used before initialization."))) 
			((eq? varname (caar state)) (caadr state))
			(else (SL_get varname (list (cdar state) (cdadr state))))
		)
	))

;Assign value to a variable
;Returns a new state with the corresponding value of the varpair replaced by the new varpair
(define SL_set
	(lambda (varpair state)
		(cond
			((null? (car state)) (error (string-append "The variable " (symbol->string (car varpair)) " is not declared.")))
			((eq? (car varpair) (caar state)) (SL_add varpair (list (cdar state) (cdadr state))))
			(else (SL_add (list (caar state) (caadr state)) (SL_set varpair (list (cdar state) (cdadr state)))))
		)
	))

;Evaluate addition
;Returns the result value and state
(define eval_add
	(lambda (operands state)
		(cond
			((list? (car operands)) (eval_add (list (car (eval_arithm (car operands) state)) (cadr operands)) (cadr (eval_arithm (car operands) state))))
			((list? (cadr operands)) (eval_add (list (car operands) (car (eval_arithm (cadr operands) state))) (cadr (eval_arithm (cadr operands) state))))
			((not (and (number? (car operands)) (number? (cadr operands)))) (error "Adding non-number values!"))
			(else (list (+ (car operands) (cadr operands)) state))
		)
	))

;Evaluate subtraction
;Returns the result value and state
(define eval_sub
	(lambda (operands state)
		(cond
			((list? (car operands)) (eval_sub (list (car (eval_arithm (car operands) state)) (cadr operands)) (cadr (eval_arithm (car operands) state))))
			((null? (cdr operands)) (- (car operands)))
			((list? (cadr operands)) (eval_sub (list (car operands) (car (eval_arithm (cadr operands) state))) (cadr (eval_arithm (cadr operands) state))))
			((not (and (number? (car operands)) (number? (cadr operands)))) (error "Subtracting non-number values!"))
			(else (list (- (car operands) (cadr operands)) state))
		)
	))

;Evaluate multiplication
;Returns the result value and state
(define eval_multi
	(lambda (operands state)
		(cond
			((list? (car operands)) (eval_multi (list (car (eval_arithm (car operands) state)) (cadr operands)) (cadr (eval_arithm (car operands) state))))
			((list? (cadr operands)) (eval_multi (list (car operands) (car (eval_arithm (cadr operands) state))) (cadr (eval_arithm (cadr operands) state))))
			((not (and (number? (car operands)) (number? (cadr operands)))) (error "Multiplicating non-number values!"))
			(else (list (* (car operands) (cadr operands)) state))
		)
	))

;Evaluate division
;Returns the result value and state
(define eval_div
	(lambda (operands state)
		(cond
			((list? (car operands)) (eval_div (list (car (eval_arithm (car operands) state)) (cadr operands)) (cadr (eval_arithm (car operands) state))))
			((list? (cadr operands)) (eval_div (list (car operands) (car (eval_arithm (cadr operands) state))) (cadr (eval_arithm (cadr operands) state))))
			((not (and (number? (car operands)) (number? (cadr operands)))) (error "Dividing non-number values!"))
			(else (list (quotient (car operands) (cadr operands)) state))
		)
	))

;Evaluate modulation
;Returns the result value and state
(define eval_mod
	(lambda (operands state)
		(cond
			((list? (car operands)) (eval_mod (list (car (eval_arithm (car operands) state)) (cadr operands)) (cadr (eval_arithm (car operands) state))))
			((list? (cadr operands)) (eval_mod (list (car operands) (car (eval_arithm (cadr operands) state))) (cadr (eval_arithm (cadr operands) state))))
			((not (and (number? (car operands)) (number? (cadr operands)))) (error "Adding non-number values!"))
			(else (list (modulo (car operands) (cadr operands)) state))
		)
	))

;Evaluate arithmatic expression
;Returns the evaluation result and the new state with posible assignment handled
(define eval_arithm
	(lambda (expr state)
		(cond
			((null? expr) (list '() state))
			((number? expr) (list expr state))
			((not (list? expr)) (list SL_get(expr) state))
			((eq? (car expr) '+) (eval_add (cdr expr) state))
			((eq? (car expr) '-) (eval_sub (cdr expr) state))
			((eq? (car expr) '*) (eval_multi (cdr expr) state))
			((eq? (car expr) '/) (eval_div (cdr expr) state))
			((eq? (car expr) '%) (eval_mod (cdr expr) state))
			(else (error "Unknown operator."))
		)
	))

;Evaluate conditional expression
;Returns #t or #f depends on the result of the expression
(define eval_cond)

;Evaluate if statement
;Returns the result state of the corresponding branch
(define eval_if
	(lambda (condition expr_true expr_false)
		(cond
		)
	))

;Evaluate while statement
;Compute the loop recursively

(define interpret
	(lambda (expr state)
		(cond
			((null? expr) state)
			((null? (car expr)) (interpret (cdr expr) state))
			((list? (car expr) (interpret (cdr expr) (interpret (car expr) state))))
			((eq? (car expr) 'var) (SL_add (list (cadr expr) '()) state))
			((eq? (car expr) 'if))
			((eq? (car expr) 'while))
			(else )
		)
	))