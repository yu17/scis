;EECS 345 Interpreter Project Part 2
;Group 39. Jiaqi Yu, Yiquan Zhu, Renjie Xi

;Value function. Keeps the literals and convert the variables to literals.
(define eval_expr_symbol
	(lambda (expr env c-return c-break c-continue c-throw return_v)
		(cond
			((or (number? expr)
				(boolean? expr)) (return_v expr))
			((equal? (string-upcase (symbol->string expr)) "TRUE") (return_v #t))
			((equal? (string-upcase (symbol->string expr)) "FALSE") (return_v #f))
			(else (EL_SL_get expr env return_v)))
	))

;Value function. Call eval_expr_auto on the operands first and then return the sum of the results.
(define eval_expr_add
	(lambda (operands env c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e c-return c-break c-continue c-throw (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Adding non-number values!")
						(return_v (+ v1 v2)))
				))))))
	))

;Value function. Call eval_expr_auto on the operands first and then return the difference or the negation of the results.
(define eval_expr_sub
	(lambda (operands env c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (e)
				(cond
					((not (number? v1)) (error "Subtracting/Negating non-number values!"))
					((not (null? (cdr operands))) (eval_expr_auto (cadr operands) e c-return c-break c-continue c-throw (lambda (v2)
						(if (not (number? v2))
							(error "Subtracting non-number values!")
							(return_v (- v1 v2)))
					)))
					(else (return_v (- v1)))
			)))))
		))

;Value function. Call eval_expr_auto on the operands first and then return the product of the results.
(define eval_expr_mult
	(lambda (operands env c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e c-return c-break c-continue c-throw (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Multiplying non-number values!")
						(return_v (* v1 v2)))
				))))))
	))

;Value function. Call eval_expr_auto on the operands first and then return the difference of the results.
(define eval_expr_div
	(lambda (operands env c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e c-return c-break c-continue c-throw (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Dividing non-number values!")
						(return_v (quotient v1 v2)))
				))))))
	))

;Value function. Call eval_expr_auto on the operands first and then return the modulation of the results.
(define eval_expr_mod
	(lambda (operands env c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e c-return c-break c-continue c-throw (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Dividing non-number values!")
						(return_v (modulo v1 v2)))
				))))))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return the logic and of the results.
(define eval_expr_and
	(lambda (operands env c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (v1)
			(if (or (not v1) (equal? v1 0))
				(return_v #f)
				(intpn_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (e)
					(eval_expr_auto (cadr operands) e c-return c-break c-continue c-throw (lambda (v2)
						(return_v (and v2 (not (equal? v2 0))))
					))))
				)))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return the logic or of the results.
(define eval_expr_or
	(lambda (operands env c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (v1)
			(if (and v1 (not (equal? v1 0)))
				(return_v #t)
				(intpn_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (e)
					(eval_expr_auto (cadr operands) e c-return c-break c-continue c-throw (lambda (v2)
						(return_v (not (or (not v2) (equal? v2 0))))
					))))
				)))
	))

;Value(boolean) function. Call eval_expr_auto on the operand first and then return the logic not of the result.
(define eval_expr_not
	(lambda (operands env c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (v)
			(return_v (or (not v) (equal? v 0)))
		))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return whether the results are equal.
(define eval_expr_eq
	(lambda (operands env c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e c-return c-break c-continue c-throw (lambda (v2)
					(return_v (equal? v1 v2))
				))))))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return whether the results are not equal.
(define eval_expr_neq
	(lambda (operands env return_v)
		(eval_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e c-return c-break c-continue c-throw (lambda (v2)
					(return_v (not (equal? v1 v2)))
				))))))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return whether the first result is less than the second one.
(define eval_expr_lt
	(lambda (operands env c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e c-return c-break c-continue c-throw (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Comparing non-number values!")
						(return_v (< v1 v2)))
				))))))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return whether the first result is greater than the second one.
(define eval_expr_gt
	(lambda (operands env c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e c-return c-break c-continue c-throw (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Comparing non-number values!")
						(return_v (> v1 v2)))
				))))))
	))

;;Value(boolean) function. Call eval_expr_auto on the operands first and then return whether the first result is less or equal to than the second one.
(define eval_expr_le
	(lambda (operands env c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e c-return c-break c-continue c-throw (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Comparing non-number values!")
						(return_v (<= v1 v2)))
				))))))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return whether the first result is less than or equal to the second one.
(define eval_expr_ge
	(lambda (operands env c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e c-return c-break c-continue c-throw (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Comparing non-number values!")
						(return_v (>= v1 v2)))
				))))))
	))

;Value function. Return the evaluation result of the value being assigned.
(define eval_expr_assign
	(lambda (operands env c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (cadr operands) env c-return c-break c-continue c-throw return_v)
	))

;The sorter function for all the value function. Choose the corresponding evaluation function according to the first atom in the expression and pass the arguments to that function.
(define eval_expr_auto
	(lambda (expr env c-return c-break c-continue c-throw return_v)
		(cond
			((not (list? expr)) (eval_expr_symbol expr env c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '+) (eval_expr_add (cdr expr) env c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '-) (eval_expr_sub (cdr expr) env c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '*) (eval_expr_mult (cdr expr) env c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '/) (eval_expr_div (cdr expr) env c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '%) (eval_expr_mod (cdr expr) env c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '&&) (eval_expr_and (cdr expr) env c-return c-break c-continue c-throw return_v))
			((equal? (car expr) (string->symbol "||")) (eval_expr_or (cdr expr) env c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '!) (eval_expr_not (cdr expr) env c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '==) (eval_expr_eq (cdr expr) env c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '!=) (eval_expr_neq (cdr expr) env c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '<) (eval_expr_lt (cdr expr) env c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '>) (eval_expr_gt (cdr expr) env c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '<=) (eval_expr_le (cdr expr) env c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '>=) (eval_expr_ge (cdr expr) env c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '=) (eval_expr_assign (cdr expr) env c-return c-break c-continue c-throw return_v))
			((equal? (car expr) 'funcall) (eval_funcall (cdr expr) env c-return c-break c-continue c-throw return_v))
			(else (error (string-append "Invalid operator " (symbol->string (car expr)) "."))))
	))

;The dummy function that converts the results of the value functions to booleans.
(define bool_expr_auto
	(lambda (expr env c-return c-break c-continue c-throw return_b)
		(eval_expr_auto expr env c-return c-break c-continue c-throw (lambda (v)
			(return_b (and v (not (equal? v 0))))
		))
	))

;env function. Used for all non-assignment expressions. Return the new env after interpreting the operands.
(define intpn_expr_noeffectoperator
	(lambda (operands env c-return c-break c-continue c-throw return_e)
		(intpn_expr_auto (car operands) env c-return c-break c-continue c-throw (lambda (e)
			(if (not (null? (cdr operands)))
				(intpn_expr_auto (cadr operands) e c-return c-break c-continue c-throw return_e)
				(return_e e))))
	))

;env function. Used for the assignment expression. Return the new env after the assignment.
(define intpn_expr_assign
	(lambda (operands env c-return c-break c-continue c-throw return_e)
		(eval_expr_auto (cadr operands) env c-return c-break c-continue c-throw (lambda (v)
			(intpn_expr_auto (cadr operands) env c-return c-break c-continue c-throw (lambda (e)
				(EL_SL_set (list (car operands) v) e return_e)))))
	))
		
;env function. The sorter function for the env function of the expressions.
(define intpn_expr_auto
	(lambda (expr env c-return c-break c-continue c-throw return_e)
		(cond
			((not (list? expr)) (return_e env))
			((or (equal? (car expr) '+)
				(equal? (car expr) '-)
				(equal? (car expr) '*)
				(equal? (car expr) '/)
				(equal? (car expr) '%)
				(equal? (car expr) '&&)
				(equal? (car expr) (string->symbol "||"))
				(equal? (car expr) '!)
				(equal? (car expr) '==)
				(equal? (car expr) '!=)
				(equal? (car expr) '<)
				(equal? (car expr) '>)
				(equal? (car expr) '<=)
				(equal? (car expr) '>=)) (intpn_expr_noeffectoperator (cdr expr) env c-return c-break c-continue c-throw return_e))
			((equal? (car expr) '=) (intpn_expr_assign (cdr expr) env c-return c-break c-continue c-throw return_e))
			((equal? (car expt) 'funcall) (intpn_funcall (cdr expr) env c-return c-break c-continue c-throw return_e))
		)
	))

;env function. Process the declaration.
(define intpn_var
	(lambda (stmt env c-return c-break c-continue c-throw return_e)
		(if (null? (cdr stmt))
			(EL_SL_add (list (car stmt) '()) env return_e)
			(eval_expr_auto (cadr stmt) env c-return c-break c-continue c-throw (lambda (v)
				(intpn_expr_auto (cadr stmt) env c-return c-break c-continue c-throw (lambda (e)
					(EL_SL_add (list (car stmt) v) e return_e)))))
		)))

;env function. Process the if envment.
(define intpn_if
	(lambda (stmt env c-return c-break c-continue c-throw return_e)
		(bool_expr_auto (car stmt) env c-return c-break c-continue c-throw (lambda (b)
			(intpn_expr_auto (car stmt) env c-return c-break c-continue c-throw (lambda (e)
				(cond
					(b (intpn_stmt_auto (cadr stmt) e c-return c-break c-continue c-throw return_e))
					((not (null? (cddr stmt))) (intpn_stmt_auto (caddr stmt) e c-return c-break c-continue c-throw return_e))
					(else (return_e e))
				)))))
	))

;env function. Process the while envment.
;Creates the c-break (which is essentially the return_e, which points to the caller before the loop) and the c-continue (which is essentially running the next loop) continuations. 
(define intpn_while
	(lambda (stmt env c-return c-break c-continue c-throw return_e)
		(bool_expr_auto (car stmt) env c-return c-break c-continue c-throw (lambda (b)
			(intpn_expr_auto (car stmt) env c-return c-break c-continue c-throw (lambda (e)
				(if b
					(intpn_stmt_auto (cadr stmt) e c-return
						return_e
						(lambda (e2) (intpn_while stmt e2 c-return c-break c-continue c-throw return_e))
						c-throw
						(lambda (e2) (intpn_while stmt e2 c-return c-break c-continue c-throw return_e)))
					(return_e e)))
			)))
	))

;Value function. Process the return envment and calls c-return to return.
(define intpn_return
	(lambda (stmt env c-return c-break c-continue c-throw return_e)
		(if (null? c-return)
			(return_e env)
			(eval_expr_auto (car stmt) env c-return c-break c-continue c-throw c-return))
	))

;env function. Process the envment blocks. Creates a new layer when entering the block and remove it when leaving. It also adds a intermediate function to c-throw that removes a layer so that when throw is called in side a code block, the env layer is still properly removed.
(define intpn_begin
	(lambda (stmt env c-return c-break c-continue c-throw return_e)
		(EL_pushLayer env (lambda (e)
			(intpn_stmt_auto stmt e c-return c-break c-continue
			(lambda (e2 err)
				(EL_popLayer e2 (lambda (e3) (c-throw e3 err))))
			(lambda (e2)
				(EL_popLayer e2 return_e)))))
	))

;Goto function. Remove a env layer and calls c-break.
(define intpn_break
	(lambda (env c-break)
		(EL_popLayer env c-break)
	))

;Goto function. Remove a env layer and calls c-continue.
(define intpn_continue
	(lambda (env c-continue)
		(EL_popLayer env c-continue)
	))

;env function. Process the try block. Creates a new layer for the envment block, checks if catch and finally exist and execute them. When it is properly returned, a env layer is removed before executing finally. Otherwise, that layer is removed by the throw function.
(define intpn_try
	(lambda (stmt env c-return c-break c-continue c-throw return_e)
		(EL_pushLayer env (lambda (e)
			(intpn_stmt_auto (car stmt) e c-return c-break c-continue
				(lambda (e2 err)
					(if (not (null? (cadr stmt)))
						(intpn_catch (cdadr stmt) e2 err c-return c-break c-continue c-throw (lambda (e3)
							(if (not (null? (caddr stmt)))
								(intpn_finally (cdaddr stmt) e3 c-return c-break c-continue c-throw return_e)
								(return_e e3))
								))
						(if (not (null? (caddr) stmt))
							(intpn_finally (cdaddr stmt) e3 c-return c-break c-continue c-throw return_e)
							(return_e e2))))
				(lambda (e2) (EL_popLayer e2 (lambda (e3)
					(if (not (null? (caddr stmt)))
						(intpn_finally (cdaddr stmt) e3 c-return c-break c-continue c-throw return_e)
						(return_e e3))
				)))
			)))
	))
						
;Goto function. Remove a env layer and calls c-throw.
(define intpn_throw
	(lambda (stmt env c-throw)
		(EL_popLayer env (lambda (e) (c-throw e (car stmt))))
	))

;env function. Similar to what the begin function do.
(define intpn_catch
	(lambda (stmt env error c-return c-break c-continue c-throw return_e)
		(EL_pushLayer env (lambda (e)
			(EL_SL_add (list (caar stmt) error) e (lambda (e2)
				(intpn_stmt_auto (cadr stmt) e2 c-return c-break c-continue
				(lambda (e3 err)
					(EL_popLayer e3 (lambda (e4) (c-throw e4 err))))
				(lambda (e3)
					(EL_popLayer e3 return_e)))
					))))
	))

;env function. Similar to what the begin function do.
(define intpn_finally
	(lambda (stmt env c-return c-break c-continue c-throw return_e)
		(EL_pushLayer env (lambda (e)
			(intpn_stmt_auto (car stmt) e c-return c-break c-continue
			(lambda (e2 err)
					(EL_popLayer e2 (lambda (e3) (c-throw e3 err))))
			(lambda (e2)
				(EL_popLayer e2 return_e)))))
	))

(define intpn_function
	(lambda (stmt env c-return c-break c-continue c-throw return_e)
		(if (equal? (car stmt) 'main)
			(EL_FL_add stmt env (lambda (e)
				(eval_funcall '(main) e '() c-break c-continue c-throw c-return)))
			(EL_FL_add stmt env return_e))
	))

(define intpn_funcall_arglist
	(lambda (formals actuals env c-return c-break c-continue c-throw return_e)
		(cond
			((and (null? formals) (null? actuals)) (return_e env))
			((or (null? formals) (null? actuals)) (error "Invalid function call. Too many or few arguments."))
			(eval_expr_auto (car actuals) env c-return c-break c-continue c-throw (lambda (v)
				(intpn_expr_auto (car actuals) env c-return c-break c-continue c-throw (lambda (e)
					(EL_SL_add (list (car formals) v) e (lambda (e2)
						(intpn_funcall_arglist (cdr formals) (cdr actuals) e2 c-return c-break c-continue c-throw return_e)))
				))
			))
		)
	))

;env function. funcall
(define intpn_funcall
	(lambda (stmt env c-return c-break c-continue c-throw return_e)
		(EL_pushLayer env (lambda (e)
			(EL_FL_get (car stmt) env (lambda (f)
				(intpn_funcall_arglist (car f) (cdr stmt) e c-return c-break c-continue c-throw (lambda (e2)
					(intpn_stmt_auto (cadr f) e2 '() c-break c-continue c-throw return_e)
				))
			))
		))
	))

(define eval_funcall
	(lambda (stmt env c-return c-break c-continue c-throw return_v)
		(EL_pushLayer env (lambda (e)
			(EL_FL_get (car stmt) env (lambda (f)
				(intpn_funcall_arglist (car f) (cdr stmt) e c-return c-break c-continue c-throw (lambda (e2)
					(intpn_stmt_auto (cadr f) e2 return_v c-break c-continue c-throw missing_return)
				))
			))
		))
	))

;The main sorter function of the interpreter. Takes the result from the parser and execute them with the appropriate interpretation functions.
(define intpn_stmt_auto
	(lambda (stmt env c-return c-break c-continue c-throw return_e)
		(cond
			((null? stmt) (return_e env))
			((null? (car stmt)) (intpn_stmt_auto (cdr stmt) env c-return c-break c-continue c-throw return_e))
			((list? (car stmt)) (intpn_stmt_auto (car stmt) env c-return c-break c-continue c-throw
				(lambda (e) (intpn_stmt_auto (cdr stmt) e c-return c-break c-continue c-throw return_e))))
			((equal? (car stmt) 'var) (intpn_var (cdr stmt) env c-return c-break c-continue c-throw return_e))
			((equal? (car stmt) 'if) (intpn_if (cdr stmt) env c-return c-break c-continue c-throw return_e))
			((equal? (car stmt) 'while) (intpn_while (cdr stmt) env c-return c-break c-continue c-throw return_e))
			((equal? (car stmt) 'return) (intpn_return (cdr stmt) env c-return c-break c-continue c-throw return_e))
			((equal? (car stmt) 'begin) (intpn_begin (cdr stmt) env c-return c-break c-continue c-throw return_e))
			((equal? (car stmt) 'break) (intpn_break env c-break))
			((equal? (car stmt) 'continue) (intpn_continue env c-continue))
			((equal? (car stmt) 'try) (intpn_try (cdr stmt) env c-return c-break c-continue c-throw return_e))
			((equal? (car stmt) 'throw) (intpn_throw (cdr stmt) env c-throw))
			((equal? (car stmt) 'function) (intpn_function (cdr stmt) env c-return c-break c-continue c-throw return_e))
			((equal? (car stmt) 'funcall) (intpn_funcall (cdr stmt) env c-return c-break c-continue c-throw return_e))
			(else (intpn_expr_auto stmt env c-return c-break c-continue c-throw return_e))
		)
	))

(define invalid_goto
	(lambda (dummy)
		(error "Invalid break or continue instruction.")
	))

(define invalid_throw
	(lambda (dummy1 dummy2)
		(error "Invalid throw instruction.")
	))

(define missing_return
	(lambda (dummy)
		(error "Missing return statment.")
	))

(define missing_main
	(lambda (dummy)
		(error "Missing main function.")
	))

;Load the sample parser
(load "functionParser.scm")

;Load the env operation functions
(load "envList.scm")

;Interface Function
(define interpret
	(lambda (fname)
		(EL_init (lambda (e)
			(intpn_stmt_auto (parser fname) e
				(lambda (v) (cond
					((equal? v #t) 'true)
					((equal? v #f) 'false)
					(else v)))
				invalid_goto
				invalid_goto
				invalid_throw
				missing_main)
		))
	))