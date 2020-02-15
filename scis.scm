; StateList functions
; Following functions of which the names are prefixed with "SL" are the state operation functions.

;Check if a variable is declared
;Takes a variable name and the state
;Returns #t if the varname is found in state and #f otherwise
(define SL_check
	(lambda (varname state)
		(cond
			((null? (car state)) #f)
			((equal? varname (caar state)) #t)
			(else (SL_check varname (list (cdar state) (cdadr state))))
		)
	))

;Add a new pair of variable into the state list
;Takes a pair of variable name and value and the state
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
;Takes a variable name and the state
;Returns a new state with the varname entry removed
(define SL_rm
	(lambda (varname state)
		(cond
			((null? (car state)) state)
			((equal? varname (caar state)) (list (cdar state) (cdadr state)))
			(else (SL_add (list (caar state) (caadr state)) (SL_rm varname (list (cdar state) (cdadr state)))))
		)
	))

;Get the variable value from the state list
;Takes a variable name and the state
;Returns the corresponding value for varname
(define SL_get
	(lambda (varname state)
		(cond
			((null? (car state)) (error (string-append "The variable " (symbol->string varname) " is not declared.")))
			((and (equal? varname (caar state)) (null? (caadr state))) (error (string-append "The variable " (symbol->string varname) " is used before initialization."))) 
			((equal? varname (caar state)) (caadr state))
			(else (SL_get varname (list (cdar state) (cdadr state))))
		)
	))

;Assign value to a variable
;Takes a pair of variable name and value and the state
;Returns a new state with the corresponding value of the varpair replaced by the new varpair
(define SL_set
	(lambda (varpair state)
		(cond
			((null? (car state)) (error (string-append "The variable " (symbol->string (car varpair)) " is not declared.")))
			((equal? (car varpair) (caar state)) (SL_add varpair (list (cdar state) (cdadr state))))
			(else (SL_add (list (caar state) (caadr state)) (SL_set varpair (list (cdar state) (cdadr state)))))
		)
	))


;Following functions of which the names are prefixed with "eval" are the value evaluation functions.

;Check if the expression needs expansion
;Takes an expression
;Returns #t if and only if the expression is non of number, boolean, or string.
(define eval_needexpan?
	(lambda (expr)
		(not (or (number? expr) (boolean? expr) (string? expr)))
	))

;Evaluate addition
;Takes a list of two operands and the state
;Evaluates the operands if necessary
;Returns the sum of two operands and the new state
(define eval_add
	(lambda (operands state)
		(cond
			((eval_needexpan? (car operands)) (eval_add (list (car (eval_auto (car operands) state)) (cadr operands)) (cadr (eval_auto (car operands) state))))
			((eval_needexpan? (cadr operands)) (eval_add (list (car operands) (car (eval_auto (cadr operands) state))) (cadr (eval_auto (cadr operands) state))))
			((not (and (number? (car operands)) (number? (cadr operands)))) (error "Adding non-number values!"))
			(else (list (+ (car operands) (cadr operands)) state))
		)
	))

;Evaluate subtraction
;Takes a list of one or two operands and the state
;Evaluates the operands if necessary
;Returns the differences of two operands (or the negation if there's only one) and the new state
(define eval_sub
	(lambda (operands state)
		(cond
			((eval_needexpan? (car operands)) (eval_sub (append (list (car (eval_auto (car operands) state))) (cdr operands)) (cadr (eval_auto (car operands) state))))
			((null? (cdr operands)) (list (- (car operands)) state))
			((eval_needexpan? (cadr operands)) (eval_sub (list (car operands) (car (eval_auto (cadr operands) state))) (cadr (eval_auto (cadr operands) state))))
			((not (and (number? (car operands)) (number? (cadr operands)))) (error "Subtracting non-number values!"))
			(else (list (- (car operands) (cadr operands)) state))
		)
	))

;Evaluate multiplication
;Takes a list of two operands and the state
;Evaluates the operands if necessary
;Returns the multiplication of two operands and the new state
(define eval_multi
	(lambda (operands state)
		(cond
			((eval_needexpan? (car operands)) (eval_multi (list (car (eval_auto (car operands) state)) (cadr operands)) (cadr (eval_auto (car operands) state))))
			((eval_needexpan? (cadr operands)) (eval_multi (list (car operands) (car (eval_auto (cadr operands) state))) (cadr (eval_auto (cadr operands) state))))
			((not (and (number? (car operands)) (number? (cadr operands)))) (error "Multiplicating non-number values!"))
			(else (list (* (car operands) (cadr operands)) state))
		)
	))

;Evaluate division
;Takes a list of two operands and the state
;Evaluates the operands if necessary
;Returns the quotient of two operands and the new state
(define eval_div
	(lambda (operands state)
		(cond
			((eval_needexpan? (car operands)) (eval_div (list (car (eval_auto (car operands) state)) (cadr operands)) (cadr (eval_auto (car operands) state))))
			((eval_needexpan? (cadr operands)) (eval_div (list (car operands) (car (eval_auto (cadr operands) state))) (cadr (eval_auto (cadr operands) state))))
			((not (and (number? (car operands)) (number? (cadr operands)))) (error "Dividing non-number values!"))
			(else (list (quotient (car operands) (cadr operands)) state))
		)
	))

;Evaluate modulation
;Takes a list of two operands and the state
;Evaluates the operands if necessary
;Returns the remainder of two operands and the new state
(define eval_mod
	(lambda (operands state)
		(cond
			((eval_needexpan? (car operands)) (eval_mod (list (car (eval_auto (car operands) state)) (cadr operands)) (cadr (eval_auto (car operands) state))))
			((eval_needexpan? (cadr operands)) (eval_mod (list (car operands) (car (eval_auto (cadr operands) state))) (cadr (eval_auto (cadr operands) state))))
			((not (and (number? (car operands)) (number? (cadr operands)))) (error "Adding non-number values!"))
			(else (list (modulo (car operands) (cadr operands)) state))
		)
	))

;Evaluate equal comparison
;Takes a list of two operands and the state
;Evaluates the operands if necessary
;Returns a boolean value and the new state
(define eval_eq
	(lambda (operands state)
		(cond
			((eval_needexpan? (car operands)) (eval_eq (list (car (eval_auto (car operands) state)) (cadr operands)) (cadr (eval_auto (car operands) state))))
			((eval_needexpan? (cadr operands)) (eval_eq (list (car operands) (car (eval_auto (cadr operands) state))) (cadr (eval_auto (cadr operands) state))))
			((not (and (number? (car operands)) (number? (cadr operands)))) (error "Comparing non-number values!"))
			(else (list (equal? (car operands) (cadr operands)) state))
		)
	))

;Evaluate non-equal comparison
;Takes a list of two operands and the state
;Evaluates the operands if necessary
;Returns a boolean value and the new state
(define eval_neq
	(lambda (operands state)
		(cond
			((eval_needexpan? (car operands)) (eval_neq (list (car (eval_auto (car operands) state)) (cadr operands)) (cadr (eval_auto (car operands) state))))
			((eval_needexpan? (cadr operands)) (eval_neq (list (car operands) (car (eval_auto (cadr operands) state))) (cadr (eval_auto (cadr operands) state))))
			((not (and (number? (car operands)) (number? (cadr operands)))) (error "Comparing non-number values!"))
			(else (list (not (equal? (car operands) (cadr operands))) state))
		)
	))

;Evaluate less-than comparison
;Takes a list of two operands and the state
;Evaluates the operands if necessary
;Returns a boolean value and the new state
(define eval_lt
	(lambda (operands state)
		(cond
			((eval_needexpan? (car operands)) (eval_lt (list (car (eval_auto (car operands) state)) (cadr operands)) (cadr (eval_auto (car operands) state))))
			((eval_needexpan? (cadr operands)) (eval_lt (list (car operands) (car (eval_auto (cadr operands) state))) (cadr (eval_auto (cadr operands) state))))
			((not (and (number? (car operands)) (number? (cadr operands)))) (error "Comparing non-number values!"))
			(else (list (< (car operands) (cadr operands)) state))
		)
	))

;Evaluate less-than-or-equal-to comparison
;Takes a list of two operands and the state
;Evaluates the operands if necessary
;Returns a boolean value and the new state
(define eval_le
	(lambda (operands state)
		(cond
			((eval_needexpan? (car operands)) (eval_le (list (car (eval_auto (car operands) state)) (cadr operands)) (cadr (eval_auto (car operands) state))))
			((eval_needexpan? (cadr operands)) (eval_le (list (car operands) (car (eval_auto (cadr operands) state))) (cadr (eval_auto (cadr operands) state))))
			((not (and (number? (car operands)) (number? (cadr operands)))) (error "Comparing non-number values!"))
			(else (list (<= (car operands) (cadr operands)) state))
		)
	))

;Evaluate greater-than comparison
;Takes a list of two operands and the state
;Evaluates the operands if necessary
;Returns a boolean value and the new state
(define eval_gt
	(lambda (operands state)
		(cond
			((eval_needexpan? (car operands)) (eval_gt (list (car (eval_auto (car operands) state)) (cadr operands)) (cadr (eval_auto (car operands) state))))
			((eval_needexpan? (cadr operands)) (eval_gt (list (car operands) (car (eval_auto (cadr operands) state))) (cadr (eval_auto (cadr operands) state))))
			((not (and (number? (car operands)) (number? (cadr operands)))) (error "Comparing non-number values!"))
			(else (list (> (car operands) (cadr operands)) state))
		)
	))

;Evaluate greater-than-or-equal-to comparison
;Takes a list of two operands and the state
;Evaluates the operands if necessary
;Returns a boolean value and the new state
(define eval_ge
	(lambda (operands state)
		(cond
			((eval_needexpan? (car operands)) (eval_ge (list (car (eval_auto (car operands) state)) (cadr operands)) (cadr (eval_auto (car operands) state))))
			((eval_needexpan? (cadr operands)) (eval_ge (list (car operands) (car (eval_auto (cadr operands) state))) (cadr (eval_auto (cadr operands) state))))
			((not (and (number? (car operands)) (number? (cadr operands)))) (error "Comparing non-number values!"))
			(else (list (>= (car operands) (cadr operands)) state))
		)
	))

;Evaluate logic and
;Takes a list of two operands and the state
;Evaluates the operands if necessary
;Returns the result value in scheme #t or #f and the new state with possible assignment handled
;The second operand would not be evaluated if the and operation is short-circuited by the first operand
(define eval_and
	(lambda (operands state)
		(cond
			((eval_needexpan? (car operands)) (eval_and (list (car (eval_auto (car operands) state)) (cadr operands)) (cadr (eval_auto (car operands) state))))
			((number? (car operands)) (eval_and (list (not (equal? (car operands) 0)) (cadr operands)) state))
			((not (boolean? (car operands))) (error (string-append "Type error. Boolean expected but " (symbol->string (car operands)) " found.")))
			((not (car operands)) (list #f state))
			((eval_needexpan? (cadr operands)) (eval_and (list (car operands) (car (eval_auto (cadr operands) state))) (cadr (eval_auto (cadr operands) state))))
			((number? (cadr operands)) (eval_and (list (car operands) (not (equal? (car operands) 0))) state))
			((not (boolean? (cadr operands))) (error (string-append "Type error. Boolean expected but " (symbol->string (cadr operands)) " found.")))
			(else (list (cadr operands) state))
		)
	))

;Evaluate logic or
;Takes a list of two operands and the state
;Evaluates the operands if necessary
;Returns the result value in scheme #t or #f and the new state with possible assignment handled
;The second operand would not be evaluated if the and operation is short-circuited by the first operand
(define eval_or
	(lambda (operands state)
		(cond
			((eval_needexpan? (car operands)) (eval_or (list (car (eval_auto (car operands) state)) (cadr operands)) (cadr (eval_auto (car operands) state))))
			((number? (car operands)) (eval_or (list (not (equal? (car operands) 0)) (cadr operands)) state))
			((not (boolean? (car operands))) (error (string-append "Type error. Boolean expected but " (symbol->string (car operands)) " found.")))
			((car operands) (list #t state))
			((eval_needexpan? (cadr operands)) (eval_or (list (car operands) (car (eval_auto (cadr operands) state))) (cadr (eval_auto (cadr operands) state))))
			((number? (cadr operands)) (eval_or (list (car operands) (not (equal? (car operands) 0))) state))
			((not (boolean? (cadr operands))) (error (string-append "Type error. Boolean expected but " (symbol->string (cadr operands)) " found.")))
			(else (list (cadr operands) state))
		)
	))

;Evaluate logic not
;Takes a list of one operand and the state
;Evaluates the operand if necessary
;Returns the result value in scheme #t or #f and the new state with possible assignment handled
(define eval_not
	(lambda (operands state)
		(cond
			((eval_needexpan? (car operands)) (eval_not (list (car (eval_auto (car operands) state))) (cadr (eval_auto (car operands) state))))
			((number? (car operands)) (list (equal? (car operands) 0) state))
			((not (boolean? (car operands))) (error (string-append "Type error. Boolean expected but " (symbol->string (car operands)) " found.")))
			(else (list (not (car operands)) state))
		)
	))

;Evaluate assignment
;Takes a variable name, an expression, and the state
;Evaluates the expression if necessary
;Returns the value been assigned and the new state
(define eval_assign
	(lambda (varname expr state)
		(cond
			((eval_needexpan? expr) (eval_assign varname (car (eval_auto expr state)) (cadr (eval_auto expr state))))
			(else (list expr (SL_set (list varname expr) state)))
		)
	))


;Evaluate any expression
;Takes an expression and the state
;Checks the type of the expression and distributes it to the corresponding handler
;Returns the evaluation result and the new state with posible assignment handled
(define eval_auto
	(lambda (expr state)
		(cond
			((null? expr) (list '() state))
			((number? expr) (list expr state))
			((boolean? expr) (list expr state))
			((string? expr) (list expr state))
			((equal? expr 'true) (list #t state))
			((equal? expr 'false) (list #f state))
			((not (list? expr)) (list (SL_get expr state) state))
			((equal? (car expr) '+) (eval_add (cdr expr) state))
			((equal? (car expr) '-) (eval_sub (cdr expr) state))
			((equal? (car expr) '*) (eval_multi (cdr expr) state))
			((equal? (car expr) '/) (eval_div (cdr expr) state))
			((equal? (car expr) '%) (eval_mod (cdr expr) state))
			((equal? (car expr) '&&) (eval_and (cdr expr) state))
			((equal? (car expr) (string->symbol "||")) (eval_or (cdr expr) state))
			((equal? (car expr) '!) (eval_not (cdr expr) state))
			((equal? (car expr) '==) (eval_eq (cdr expr) state))
			((equal? (car expr) '!=) (eval_neq (cdr expr) state))
			((equal? (car expr) '<) (eval_lt (cdr expr) state))
			((equal? (car expr) '>) (eval_gt (cdr expr) state))
			((equal? (car expr) '<=) (eval_le (cdr expr) state))
			((equal? (car expr) '>=) (eval_ge (cdr expr) state))
			((equal? (car expr) '=) (eval_assign (cadr expr) (caddr expr) state))
			(else (error (string-append "Invalid operator " (symbol->string (car expr)) ".")))
		)
	))

;Following functions of which the names are prefixed with "terp" are the state transition functions.

;Interpret var statement
;Takes a list of arguments: (variable_name expression) and the state
;Evaluates the expression if necessary
;Returns the new state with new variable declared
(define terp_var
	(lambda (expr state)
		(cond
			((null? (cdr expr)) (SL_add (list (car expr) '()) state))
			((eval_needexpan? (cadr expr)) (terp_var (list (car expr) (car (eval_auto (cadr expr) state))) (cadr (eval_auto (cadr expr) state))))
			(else (SL_add (list (car expr) (cadr expr)) state))
		)
	))


;Interpret if statement
;Takes a condition expression, a list of expression to be executed (either one expression for true only or two expressions for both true and false cases), and the state
;Evaluates the expression if necessary
;Returns the new state from the corresponding branch
(define terp_if
	(lambda (condition expr state)
		(cond
			((eval_needexpan? condition) (terp_if (car (eval_auto condition state)) expr (cadr (eval_auto condition state))))
			(condition (interpret* (car expr) state))
			((and (not condition) (not (null? (cdr expr))))  (interpret* (cadr expr) state))
			(else state)
		)
	))

;Interpret while-loop statement
;Takes a condition expression, a result of evaluating the condition, an expression to be executed, and the state
;Each iteration would be run twice:
;For the first time, condition_evaluation_result should be the same as condition. The condition would then be evaluated and passed to the second run
;For the second run, condition_evaluation_result should have been evaluated and would be used to determine if the loop should abort or continue.
;Runs loop recursively and returns the final state
(define terp_loop
	(lambda (condition condition_evaluation_result expr state)
		(cond
			((eval_needexpan? condition_evaluation_result) (terp_loop condition (car (eval_auto condition state)) expr (cadr (eval_auto condition state))))
			(condition_evaluation_result (terp_loop condition condition expr (interpret* expr state)))
			(else state)
		)
	))

;Convert boolean to string
;Takes a boolean
;Returns "true" for #t and "false" for #f
(define return_boolean
	(lambda (bool)
		(if bool
			"true"
			"false"
		)
	))

;Output the interpretation result
(define return
	(lambda (expr state)
		(cond
			((eval_needexpan? expr) (return (car (eval_auto expr state)) (cadr (eval_auto expr state))))
			((boolean? expr) (return_boolean expr))
			(else expr)
		)
	))

;Main Interpretation Function
(define interpret*
	(lambda (expr state)
		(cond
			((null? expr) state)
			((null? (car expr)) (interpret* (cdr expr) state))
			((list? (car expr)) (interpret* (cdr expr) (interpret* (car expr) state)))
			((equal? (car expr) 'var) (terp_var (cdr expr) state))
			((equal? (car expr) 'if) (terp_if (cadr expr) (cddr expr) state))
			((equal? (car expr) 'while) (terp_loop (cadr expr) (cadr expr) (caddr expr) state))
			((equal? (car expr) 'return) (return (cadr expr) state))
			(else (cadr (eval_auto expr state)))
		)
	))

;Load the sample parser
(load "simpleParser.scm")

;Interface Function
;The state is initialized to '(() ()), as two empty list
(define interpret
	(lambda (fname)
		(interpret* (parser fname) '(() ()))
	))