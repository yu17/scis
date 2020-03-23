;EECS 345 Interpreter Project Part 1
;Group 1. Jiaqi Yu, Yiquan Zhu, Renjie Xi

;Following functions of which the names are prefixed with "eval" are the value evaluation functions.

;Check if the expression needs expansion
;Takes an expression
;Returns #t if and only if the expression is non of number, boolean, or string.
(define eval_needexpan?
	(lambda (expr)
		(not (or (number? expr) (boolean? expr) (string? expr)))
	))

;The following evaluation functions, in general, follows the following procedure:
;1. Takes a list of operands and the state
;2. Check each of the operand if it needs expansion (i.e, it is not a literal value) and evaluate each of them and update the state accordingly if necessary. 
;3. Perform the corresponding operation on the operands

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

;Evaluate symbols, including variable names, true/false
;Takes an expression and the state
;Returns the corresponding value of the expression and the state
;Note that this function would not modify the state. It takes and returns the state only to be consistent with all the other functions.
(define eval_symbol
	(lambda (expr state)
		(cond
			((null? expr) (list '() state))
			((number? expr) (list expr state))
			((boolean? expr) (list expr state))
			((string? expr) (list expr state))
			((equal? (string-upcase (symbol->string expr)) "TRUE") (list #t state))
			((equal? (string-upcase (symbol->string expr)) "FALSE") (list #f state))
			(else (list (SL_get expr state) state))
		)
	))


;Evaluate any expression
;Takes an expression and the state
;Checks the type of the expression and distributes it to the corresponding handler
;Returns the evaluation result and the new state with posible assignment handled
(define eval_auto
	(lambda (expr state)
		(cond
;			((null? expr) (list '() state))
;			((number? expr) (list expr state))
;			((boolean? expr) (list expr state))
;			((string? expr) (list expr state))
;			((equal? expr 'true) (list #t state))
;			((equal? expr 'false) (list #f state))
			((not (list? expr)) (eval_symbol expr state))
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















(define eval_expr_auto
	(lambda (expr state return_v)
	))

(define intpn_expr_auto
	(lambda (expr state return_s)
	))

(define intpn_var
	(lambda (stmt state return_s)
		(if (null? (cdr stmt))
			(SL_add (list (car stmt) '()) state return_s)
			(eval_expr_auto (cadr stmt) state (lambda (v)
				(intpn_expr_auto (cadr stmt) state (lambda (s)
					(SL_add (list (car stmt) v) s return_s)))))
		)))

(define intpn_if
	(lambda (stmt state c-return c-break c-continue c-throw return_s)
		(bool_expr_auto (car stmt) state (lambda (b)
			(intpn_expr_auto (car stmt) state (lambda (s)
				(cond
					(b (intpn_stmt_auto (cadr stmt) s c-return c-break c-continue c-throw return_s))
					((not (null? (cddr stmt))) (intpn_stmt_auto (caddr stmt) s c-return c-break c-continue c-throw return_s))
					(else (return_s s))
				)))))
	))

(define intpn_while
	(lambda (stmt state c-return c-break c-continue c-throw return_s)
		(bool_expr_auto (car stmt) state (lambda (b)
			(intpn_expr_auto (car stmt) state (lambda (s)
				(if b
					(intpn_stmt_auto (cadr stmt) s c-return
						return_s
						(lambda (s2) (intpn_while stmt s2 c-return c-break c-continue c-throw return_s))
						c-throw
						(lambda (s2) (intpn_while stmt s2 c-return c-break c-continue c-throw return_s)))
					(return_s s)))
			)))
	))

(define intpn_return
	(lambda (stmt state c-return)
		(eval_expr_auto stmt state c-return)
	))

(define intpn_begin
	(lambda (stmt state c-return c-break c-continue c-throw return_s)
		(SL_pushlayer state (lambda (s)
			(intpn_stmt_auto (cdr stmt) s c-return c-break c-continue c-throw (lambda (s2)
				SL_popLayer s2 return_s))))
	))

(define intpn_break
	(lambda (state c-break)
		(SL_popLayer state c-break)
	))

(define intpn_continue
	(lambda (state c-continue)
		(SL_popLayer state c-continue)
	))

(define intpn_try
	(lambda (stmt state c-return c-break c-continue c-throw return_s)
		(SL_pushLayer state (lambda (s)
			(intpn_stmt_auto (car stmt) s c-return c-break c-continue
				(lambda (s e)
					(intpn_catch (cdadr stmt) s e c-return c-break c-continue c-throw (lambda (s2)
						(intpn_finally (cdaddr stmt) s2 c-return c-break c-continue c-throw return_s))))
			)))
	))
						

(define intpn_throw
	(lambda (stmt state c-throw)
		(SL_popLayer state (lambda (s) (c-throw s (car stmt))))
	))

(define intpn_catch
	(lambda (stmt state error c-return c-break c-continue c-throw return_s)
		(SL_pushLayer state (lambda (s)
			(SL_add (list (caar stmt) error) state (lambda (s2)
				(intpn_stmt_auto (cadr stmt) s2 c-return c-break c-continue c-throw (lambda (s3)
					(SL_popLayer s3 return_s)))
					))))
	))

(define intpn_finally
	(lambda (stmt state c-return c-break c-continue c-throw return_s)
		(SL_pushLayer state (lambda (s)
			(intpn_stmt_auto (car stmt) s c-return c-break c-continue c-throw (lambda (s2)
				(SL_popLayer s2 return_s)))))
	))

;Main Interpretation Function

(define intpn_stmt_auto
	(lambda (stmt state c-return c-break c-continue c-throw return_s)
		(cond
			((null? stmt) (return_s state))
			((null? (car stmt)) (intpn_stmt_auto (cdr stmt) state c-return c-break c-continue c-throw return_s))
			((list? (car stmt)) (intpn_stmt_auto (car stmt) state c-return c-break c-continue c-throw
				(lambda (s) (intpn_stmt_auto (cdr stmt) s  c-return c-break c-continue c-throw return_s))))
			((equal? (car stmt) 'var) (intpn_var (cdr stmt) state return_s))
			((equal? (car stmt) 'if) (intpn_if (cdr stmt) state c-return c-break c-continue c-throw return_s))
			((equal? (car stmt) 'while) (intpn_while (cdr stmt) state c-return c-break c-continue c-throw return_s))
			((equal? (car stmt) 'return) (intpn_return (cdr stmt) state c-return))
			((equal? (car stmt) 'begin) (intpn_begin (cdr stmt) state c-return c-break c-continue c-throw return_s))
			((equal? (car stmt) 'break) (intpn_break state c-break))
			((equal? (car stmt) 'continue) (intpn_continue state c-continue))
			((equal? (car stmt) 'try) (intpn_try (cdr stmt) state c-return c-break c-continue c-throw return_s))
			((equal? (car stmt) 'throw) (intpn_throw (cdr stmt) state c-throw))
			(else (intpn_expr_auto stmt state c-return c-break c-continue c-throw))
		)
	))

;Load the sample parser
(load "simpleParser.scm")

;Load the state operation functions
(load "stateList.scm")

;Interface Function
(define interpret
	(lambda (fname)
		(interpret* (parser fname) (SL_init))
	))