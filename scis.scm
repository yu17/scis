;EECS 345 Interpreter Project Part 3
;Group 38. Jiaqi Yu, Yiquan Zhu, Renjie Xi

;For detailed explanation on the implementation, please refer to readme.md.

;Value function. Keeps the literals and convert the variables to literals.
(define eval_expr_symbol
	(lambda (expr env clis c-return c-break c-continue c-throw return_v)
		(cond
			((or (number? expr)
				(boolean? expr)) (return_v expr))
			((equal? (string-upcase (symbol->string expr)) "TRUE") (return_v #t))
			((equal? (string-upcase (symbol->string expr)) "FALSE") (return_v #f))
			(else (EL_SL_get expr env (lambda (v)
				(if (equal? v '__SCIS_THIS)
					(EL_Sdump env 'this (lambda (e de)
						(intpn_dot_func_expand_top e return_v)))
					(return_v v))
			))))
	))

;Value function. Call eval_expr_auto on the operands first and then return the sum of the results.
(define eval_expr_add
	(lambda (operands env clis c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e clis c-return c-break c-continue c-throw (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Adding non-number values!")
						(return_v (+ v1 v2)))
				))))))
	))

;Value function. Call eval_expr_auto on the operands first and then return the difference or the negation of the results.
(define eval_expr_sub
	(lambda (operands env clis c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (e)
				(cond
					((not (number? v1)) (error "Subtracting/Negating non-number values!"))
					((not (null? (cdr operands))) (eval_expr_auto (cadr operands) e clis c-return c-break c-continue c-throw (lambda (v2)
						(if (not (number? v2))
							(error "Subtracting non-number values!")
							(return_v (- v1 v2)))
					)))
					(else (return_v (- v1)))
			)))))
		))

;Value function. Call eval_expr_auto on the operands first and then return the product of the results.
(define eval_expr_mult
	(lambda (operands env clis c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e clis c-return c-break c-continue c-throw (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Multiplying non-number values!")
						(return_v (* v1 v2)))
				))))))
	))

;Value function. Call eval_expr_auto on the operands first and then return the difference of the results.
(define eval_expr_div
	(lambda (operands env clis c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e clis c-return c-break c-continue c-throw (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Dividing non-number values!")
						(return_v (quotient v1 v2)))
				))))))
	))

;Value function. Call eval_expr_auto on the operands first and then return the modulation of the results.
(define eval_expr_mod
	(lambda (operands env clis c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e clis c-return c-break c-continue c-throw (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Dividing non-number values!")
						(return_v (modulo v1 v2)))
				))))))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return the logic and of the results.
(define eval_expr_and
	(lambda (operands env clis c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (v1)
			(if (or (not v1) (equal? v1 0))
				(return_v #f)
				(intpn_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (e)
					(eval_expr_auto (cadr operands) e clis c-return c-break c-continue c-throw (lambda (v2)
						(return_v (and v2 (not (equal? v2 0))))
					))))
				)))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return the logic or of the results.
(define eval_expr_or
	(lambda (operands env clis c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (v1)
			(if (and v1 (not (equal? v1 0)))
				(return_v #t)
				(intpn_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (e)
					(eval_expr_auto (cadr operands) e clis c-return c-break c-continue c-throw (lambda (v2)
						(return_v (not (or (not v2) (equal? v2 0))))
					))))
				)))
	))

;Value(boolean) function. Call eval_expr_auto on the operand first and then return the logic not of the result.
(define eval_expr_not
	(lambda (operands env clis c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (v)
			(return_v (or (not v) (equal? v 0)))
		))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return whether the results are equal.
(define eval_expr_eq
	(lambda (operands env clis c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e clis c-return c-break c-continue c-throw (lambda (v2)
					(return_v (equal? v1 v2))
				))))))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return whether the results are not equal.
(define eval_expr_neq
	(lambda (operands env clis c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e clis c-return c-break c-continue c-throw (lambda (v2)
					(return_v (not (equal? v1 v2)))
				))))))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return whether the first result is less than the second one.
(define eval_expr_lt
	(lambda (operands env clis c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e clis c-return c-break c-continue c-throw (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Comparing non-number values!")
						(return_v (< v1 v2)))
				))))))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return whether the first result is greater than the second one.
(define eval_expr_gt
	(lambda (operands env clis c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e clis c-return c-break c-continue c-throw (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Comparing non-number values!")
						(return_v (> v1 v2)))
				))))))
	))

;;Value(boolean) function. Call eval_expr_auto on the operands first and then return whether the first result is less or equal to than the second one.
(define eval_expr_le
	(lambda (operands env clis c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e clis c-return c-break c-continue c-throw (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Comparing non-number values!")
						(return_v (<= v1 v2)))
				))))))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return whether the first result is less than or equal to the second one.
(define eval_expr_ge
	(lambda (operands env clis c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (v1)
			(intpn_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (cadr operands) e clis c-return c-break c-continue c-throw (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Comparing non-number values!")
						(return_v (>= v1 v2)))
				))))))
	))

;Value function. Return the evaluation result of the value being assigned.
(define eval_expr_assign
	(lambda (operands env clis c-return c-break c-continue c-throw return_v)
		(eval_expr_auto (cadr operands) env clis c-return c-break c-continue c-throw return_v)
	))

;The sorter function for all the value function. Choose the corresponding evaluation function according to the first atom in the expression and pass the arguments to that function.
(define eval_expr_auto
	(lambda (expr env clis c-return c-break c-continue c-throw return_v)
		(cond
			((not (list? expr)) (eval_expr_symbol expr env clis c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '+) (eval_expr_add (cdr expr) env clis c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '-) (eval_expr_sub (cdr expr) env clis c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '*) (eval_expr_mult (cdr expr) env clis c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '/) (eval_expr_div (cdr expr) env clis c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '%) (eval_expr_mod (cdr expr) env clis c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '&&) (eval_expr_and (cdr expr) env clis c-return c-break c-continue c-throw return_v))
			((equal? (car expr) (string->symbol "||")) (eval_expr_or (cdr expr) env clis c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '!) (eval_expr_not (cdr expr) env clis c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '==) (eval_expr_eq (cdr expr) env clis c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '!=) (eval_expr_neq (cdr expr) env clis c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '<) (eval_expr_lt (cdr expr) env clis c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '>) (eval_expr_gt (cdr expr) env clis c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '<=) (eval_expr_le (cdr expr) env clis c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '>=) (eval_expr_ge (cdr expr) env clis c-return c-break c-continue c-throw return_v))
			((equal? (car expr) '=) (eval_expr_assign (cdr expr) env clis c-return c-break c-continue c-throw return_v))
			((equal? (car expr) 'funcall) (eval_funcall (cdr expr) env clis c-return c-break c-continue c-throw return_v))
			((equal? (car expr) 'dot) (eval_dot_var (cdr expr) env clis c-return c-break c-continue c-throw return_v))
			((equal? (car expr) 'new) (eval_new (cdr expr) clis return_v))
			(else (error (string-append "Invalid operator " (symbol->string (car expr)) "."))))
	))

;The dummy function that converts the results of the value functions to booleans.
(define bool_expr_auto
	(lambda (expr env clis c-return c-break c-continue c-throw return_b)
		(eval_expr_auto expr env clis c-return c-break c-continue c-throw (lambda (v)
			(return_b (and v (not (equal? v 0))))
		))
	))

;State function. Used for all non-assignment expressions. Return the new env after interpreting the operands.
(define intpn_expr_noeffectoperator
	(lambda (operands env clis c-return c-break c-continue c-throw return_e)
		(intpn_expr_auto (car operands) env clis c-return c-break c-continue c-throw (lambda (e)
			(if (not (null? (cdr operands)))
				(intpn_expr_auto (cadr operands) e clis c-return c-break c-continue c-throw return_e)
				(return_e e))))
	))

;State function. Used for the assignment expression. Return the new env after the assignment.
(define intpn_expr_assign
	(lambda (operands env clis c-return c-break c-continue c-throw return_e)
		(eval_expr_auto (cadr operands) env clis c-return c-break c-continue c-throw (lambda (v)
			(intpn_expr_auto (cadr operands) env clis c-return c-break c-continue c-throw (lambda (e)
				(if (list? (car operands))
					;Assuming dot expression
					(intpn_dot_routine_enter (cdar operands) e clis c-return c-break c-continue c-throw (lambda (e2 de)
						(EL_SL_set (list (caddar operands) v) e2 (lambda (e3)
							(intpn_dot_routine_exit (cdar operands) e3 de clis (lambda (e4 cl)
								(if (null? e4)
									(return_e e)
									(return_e e4))
							))
						))
					))
					(EL_SL_set (list (car operands) v) e return_e))
			))
		))
	))
		
;State function. The sorter function for the State function of the expressions.
(define intpn_expr_auto
	(lambda (expr env clis c-return c-break c-continue c-throw return_e)
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
				(equal? (car expr) '>=)
				(equal? (car expr) 'new)) (intpn_expr_noeffectoperator (cdr expr) env clis c-return c-break c-continue c-throw return_e))
			((equal? (car expr) '=) (intpn_expr_assign (cdr expr) env clis c-return c-break c-continue c-throw return_e))
			((equal? (car expr) 'funcall) (intpn_funcall (cdr expr) env clis c-return c-break c-continue c-throw return_e))
			((equal? (car expr) 'dot) (intpn_dot_var (cdr expr) env clis c-return c-break c-continue c-throw return_e))
			(else (error (string-append "Invalid operator " (symbol->string (car expr)) "."))))
	))

;State function. Process the declaration.
(define intpn_var
	(lambda (stmt env clis c-return c-break c-continue c-throw return_e)
		(if (null? (cdr stmt))
			(EL_SL_add (list (car stmt) '()) env return_e)
			(eval_expr_auto (cadr stmt) env clis c-return c-break c-continue c-throw (lambda (v)
				(intpn_expr_auto (cadr stmt) env clis c-return c-break c-continue c-throw (lambda (e)
					(EL_SL_add (list (car stmt) v) e return_e)))))
		)))

;State function. Process the if envment.
(define intpn_if
	(lambda (stmt env clis c-return c-break c-continue c-throw return_e)
		(bool_expr_auto (car stmt) env clis c-return c-break c-continue c-throw (lambda (b)
			(intpn_expr_auto (car stmt) env clis c-return c-break c-continue c-throw (lambda (e)
				(cond
					(b (intpn_stmt_auto (cadr stmt) e clis c-return c-break c-continue c-throw return_e))
					((not (null? (cddr stmt))) (intpn_stmt_auto (caddr stmt) e clis c-return c-break c-continue c-throw return_e))
					(else (return_e e))
				)))))
	))

;State function. Process the while envment.
;Creates the c-break (which is essentially the return_e, which points to the caller before the loop) and the c-continue (which is essentially running the next loop) continuations. 
(define intpn_while
	(lambda (stmt env clis c-return c-break c-continue c-throw return_e)
		(bool_expr_auto (car stmt) env clis c-return c-break c-continue c-throw (lambda (b)
			(intpn_expr_auto (car stmt) env clis c-return c-break c-continue c-throw (lambda (e)
				(if b
					(intpn_stmt_auto (cadr stmt) e clis
						c-return
						return_e
						(lambda (e2) (intpn_while stmt e2 clis c-return c-break c-continue c-throw return_e))
						c-throw
						(lambda (e2) (intpn_while stmt e2 clis c-return c-break c-continue c-throw return_e)))
					(return_e e)))
			)))
	))

;Value function. Process the return expression and its side effects on the state and calls c-return to return.
(define intpn_return
	(lambda (stmt env clis c-return c-break c-continue c-throw return_e)
		(eval_expr_auto (car stmt) env clis c-return c-break c-continue c-throw (lambda (v)
			(intpn_expr_auto (car stmt) env clis c-return c-break c-continue c-throw (lambda (e)
				(c-return e v)))
		))
	))

;State function. Process the envment blocks. Creates a new layer when entering the block and remove it when leaving. It also adds a intermediate function to c-throw that removes a layer so that when throw is called in side a code block, the env layer is still properly removed.
(define intpn_begin
	(lambda (stmt env clis c-return c-break c-continue c-throw return_e)
		(EL_pushLayer env (lambda (e)
			(intpn_stmt_auto stmt e clis
				(lambda (e2 v) (EL_popLayer e2 (lambda (e3) (c-return e3 v))))
				(lambda (e2) (EL_popLayer e2 c-break))
				(lambda (e2) (EL_popLayer e2 c-continue))
				(lambda (e2 err)
					(EL_popLayer e2 (lambda (e3) (c-throw e3 err))))
				(lambda (e2)
					(EL_popLayer e2 return_e)))))
	))

;Goto function. Calls c-break.
(define intpn_break
	(lambda (env c-break)
		(c-break env)
	))

;Goto function. Calls c-continue.
(define intpn_continue
	(lambda (env c-continue)
		(c-continue env)
	))

;State function. Process the try block. Creates a new layer for the envment block, checks if catch and finally exist and execute them. When it is properly returned, a env layer is removed before executing finally. Otherwise, that layer is removed by the throw function.
(define intpn_try
	(lambda (stmt env clis c-return c-break c-continue c-throw return_e)
		(EL_pushLayer env (lambda (e)
			(intpn_stmt_auto (car stmt) e clis
				(lambda (e2 v) (EL_popLayer e2 (lambda (e3) (c-return e3 v))))
				(lambda (e2) (EL_popLayer e2 c-break))
				(lambda (e2) (EL_popLayer e2 c-continue))
				(lambda (e2 err) (EL_popLayer e2 (lambda (e3)
					(if (not (null? (cadr stmt)))
						(intpn_catch (cdadr stmt) e3 clis err c-return c-break c-continue c-throw (lambda (e4)
							(if (not (null? (caddr stmt)))
								(intpn_finally (cdaddr stmt) e4 clis c-return c-break c-continue c-throw return_e)
								(return_e e4))
								))
						(if (not (null? (caddr) stmt))
							(intpn_finally (cdaddr stmt) e3 clis c-return c-break c-continue c-throw return_e)
							(return_e e3)))
				)))
				(lambda (e2) (EL_popLayer e2 (lambda (e3)
					(if (not (null? (caddr stmt)))
						(intpn_finally (cdaddr stmt) e3 clis c-return c-break c-continue c-throw return_e)
						(return_e e3))
				)))
			)))
	))
						
;Goto function. Remove a env layer and calls c-throw.
(define intpn_throw
	(lambda (stmt env clis c-return c-break c-continue c-throw return_e)
		(eval_expr_auto (car stmt) env clis c-return c-break c-continue c-throw (lambda (v)
			(intpn_expr_auto (car stmt) env clis c-return c-break c-continue c-throw (lambda (e)
				(c-throw e v)))
		))
	))

;State function. Similar to what the begin function do.
(define intpn_catch
	(lambda (stmt env clis error c-return c-break c-continue c-throw return_e)
		(EL_pushLayer env (lambda (e)
			(EL_SL_add (list (caar stmt) error) e (lambda (e2)
				(intpn_stmt_auto (cadr stmt) e2 clis
				(lambda (e3 v) (EL_popLayer e3 (lambda (e4) (c-return e4 v))))
				(lambda (e3) (EL_popLayer e3 c-break))
				(lambda (e3) (EL_popLayer e3 c-continue))
				(lambda (e3 err)
					(EL_popLayer e3 (lambda (e4) (c-throw e4 err))))
				(lambda (e3)
					(EL_popLayer e3 return_e)))
			))
		))
	))

;State function. Similar to what the begin function do.
(define intpn_finally
	(lambda (stmt env clis c-return c-break c-continue c-throw return_e)
		(EL_pushLayer env (lambda (e)
			(intpn_stmt_auto (car stmt) e clis
			(lambda (e2 v) (EL_popLayer e2 (lambda (e3) (c-return e3 v))))
			(lambda (e2) (EL_popLayer e2 c-break))
			(lambda (e2) (EL_popLayer e2 c-continue))
			(lambda (e2 err)
					(EL_popLayer e2 (lambda (e3) (c-throw e3 err))))
			(lambda (e2)
				(EL_popLayer e2 return_e)))
		))
	))

;State function. Add the function definition to the FunctionList stack. If the name of the function is 'main', it also calls evaluation on this function.
(define intpn_function
	(lambda (stmt env clis c-return c-break c-continue c-throw return_e)
		(EL_FL_add stmt env return_e)
	))

;State function. Receive the list of the formal arguments and the list of the actual arguments, and pushing them on to the StateList.
(define intpn_funcall_arglist
	(lambda (formals actuals env clis c-return c-break c-continue c-throw return_e)
		(cond
			((and (null? formals) (null? actuals)) (return_e env))
			((or (null? formals) (null? actuals)) (error "Invalid function call. Too many or few arguments."))
			(else (eval_expr_auto (car actuals) env clis c-return c-break c-continue c-throw (lambda (v)
				(intpn_expr_auto (car actuals) env clis c-return c-break c-continue c-throw (lambda (e)
						(intpn_funcall_arglist (cdr formals) (cdr actuals) e clis c-return c-break c-continue c-throw (lambda (e2) (EL_SL_add (list (car formals) v) e2 return_e)))
				))
			)))
		)
	))

;State function. Interprets the function calls. Creates the execution environment for the function and call interpretation on the content of the function. Returns the result state after execution. Won't produce error even if the return statement is missing.
(define intpn_funcall
	(lambda (stmt env clis c-return c-break c-continue c-throw return_e)
		(if (list? (car stmt))
			;Assuming dot expression
			(intpn_dot_routine_enter (cdar stmt) env clis c-return c-break c-continue c-throw (lambda (e de)
				(EL_pushLayer env (lambda (e_func)
					(EL_FL_get (caddar stmt) e (lambda (f)
						(intpn_funcall_arglist (car f) (cdr stmt) e_func clis c-return c-break c-continue c-throw (lambda (e2)
							(intpn_dot_func_transfer_toplayer e e2 (lambda (e3)
								(EL_Fdump e3 (caddar stmt) (lambda (e4 de2)
									(intpn_stmt_auto (cadr f) e4 clis
										(lambda (e5 v) (EL_Frestore e5 de2 (lambda (e6) 
											(EL_popLayer e6 (lambda (e7)
												(intpn_dot_routine_exit (cdar stmt) e7 de clis (lambda (e8 cl) 
													(if (null? e8)
														(return_e env)
														(return_e e8))
												))
											))
										)))
										(lambda (e5) (EL_Frestore e5 de2 (lambda (e6)
											(EL_popLayer e6 (lambda (e7)
												(intpn_dot_routine_exit (cdar stmt) e7 de clis (lambda (e8 cl) 
													(if (null? e8)
														(c-break env)
														(c-break e8))
												))
											))
										)))
										(lambda (e5) (EL_Frestore e5 de2 (lambda (e6)
											(EL_popLayer e6 (lambda (e7)
												(intpn_dot_routine_exit (cdar stmt) e7 de clis (lambda (e8 cl) 
													(if (null? e8)
														(c-continue env)
														(c-continue e8))
												))
											))
										)))
										(lambda (e5 err) (EL_Frestore e5 de2 (lambda (e6)
											(EL_popLayer e6 (lambda (e7)
												(intpn_dot_routine_exit (cdar stmt) e7 de clis (lambda (e8 cl) 
													(if (null? e8)
														(c-throw env err)
														(c-throw e8 err))
												))
											))
										)))
										(lambda (e5) (EL_Frestore e5 de2 (lambda (e6)
											(EL_popLayer e6 (lambda (e7)
												(intpn_dot_routine_exit (cdar stmt) e7 de clis (lambda (e8 cl) 
													(if (null? e8)
														(return_e env)
														(return_e e8))
												))
											))
										)))
									)))
							))
						))
					))
				))
			))
			(EL_pushLayer env (lambda (e)
				(EL_FL_get (car stmt) env (lambda (f)
					(intpn_funcall_arglist (car f) (cdr stmt) e clis c-return c-break c-continue c-throw (lambda (e2)
						(EL_Fdump e2 (car stmt) (lambda (e3 de)
							(intpn_stmt_auto (cadr f) e3 clis
								(lambda (e4 v) (EL_Frestore e4 de (lambda (e5) (EL_popLayer e5 return_e))))
								(lambda (e4) (EL_Frestore e4 de (lambda (e5) (EL_popLayer e5 c-break))))
								(lambda (e4) (EL_Frestore e4 de (lambda (e5) (EL_popLayer e5 c-continue))))
								(lambda (e4 err) (EL_Frestore e4 de (lambda (e5) (EL_popLayer e5 (lambda (e6) (c-throw e6 err))))))
								(lambda (e4) (EL_Frestore e4 de (lambda (e5) (EL_popLayer e5 return_e)))))
						))
					))
				))
			))
		)
	))

;Value function. 
;State function. Interprets the function calls. Creates the execution environment for the function and call interpretation on the content of the function. Uses c-return to return the value of the return statement. Throws an error if the return statement is not found at all.
(define eval_funcall
	(lambda (stmt env clis c-return c-break c-continue c-throw return_v)
		(if (list? (car stmt))
			;Assuming dot expression
			(intpn_dot_routine_enter (cdar stmt) env clis c-return c-break c-continue c-throw (lambda (e de)
				(EL_pushLayer env (lambda (e_func)
					(EL_FL_get (caddar stmt) e (lambda (f)
						(intpn_funcall_arglist (car f) (cdr stmt) e_func clis c-return c-break c-continue c-throw (lambda (e2)
							(intpn_dot_func_transfer_toplayer e e2 (lambda (e3)
								(EL_Fdump e3 (caddar stmt) (lambda (e4 de2)
									(intpn_stmt_auto (cadr f) e4 clis
										(lambda (e5 v) (return_v v))
										(lambda (e5) (EL_Frestore e5 de2 (lambda (e6)
											(EL_popLayer e6 (lambda (e7)
												(intpn_dot_routine_exit (cdar stmt) e7 de clis (lambda (e8 cl) 
													(if (null? e8)
														(c-break env)
														(c-break e8))
												))
											))
										)))
										(lambda (e5) (EL_Frestore e5 de2 (lambda (e6)
											(EL_popLayer e6 (lambda (e7)
												(intpn_dot_routine_exit (cdar stmt) e7 de clis (lambda (e8 cl) 
													(if (null? e8)
														(c-continue env)
														(c-continue e8))
												))
											))
										)))
										(lambda (e5 err) (EL_Frestore e5 de2 (lambda (e6)
											(EL_popLayer e6 (lambda (e7)
												(intpn_dot_routine_exit (cdar stmt) e7 de clis (lambda (e8 cl) 
													(if (null? e8)
														(c-throw env err)
														(c-throw e8 err))
												))
											))
										)))
										(lambda (v) (return_v v))
									)))
							))
						))
					))
				))
			))
			(EL_pushLayer env (lambda (e)
				(EL_FL_get (car stmt) env (lambda (f)
					(intpn_funcall_arglist (car f) (cdr stmt) e clis c-return c-break c-continue c-throw (lambda (e2)
						(EL_Fdump e2 (car stmt) (lambda (e3 de)
							(intpn_stmt_auto (cadr f) e3 clis
							(lambda (e4 v) (return_v v))
							(lambda (e4) (EL_Frestore e4 de (lambda (e5) (EL_popLayer e5 c-break))))
							(lambda (e4) (EL_Frestore e4 de (lambda (e5) (EL_popLayer e5 c-continue))))
							(lambda (e4 err) (EL_Frestore e4 de (lambda (e5) (EL_popLayer e5 (lambda (e6) (c-throw e6 err))))))
							(lambda (v) (return_v v)))
						))
					))
				))
			))
		)
	))

;The main sorter function of the interpreter. Takes the result from the parser and execute them with the appropriate interpretation functions.
(define intpn_stmt_auto
	(lambda (stmt env clis c-return c-break c-continue c-throw return_e)
		(cond
			((null? stmt) (return_e env))
			((null? (car stmt)) (intpn_stmt_auto (cdr stmt) env clis c-return c-break c-continue c-throw return_e))
			((list? (car stmt)) (intpn_stmt_auto (car stmt) env clis c-return c-break c-continue c-throw
				(lambda (e) (intpn_stmt_auto (cdr stmt) e clis c-return c-break c-continue c-throw return_e))))
			((equal? (car stmt) 'var) (intpn_var (cdr stmt) env clis c-return c-break c-continue c-throw return_e))
			((equal? (car stmt) 'if) (intpn_if (cdr stmt) env clis c-return c-break c-continue c-throw return_e))
			((equal? (car stmt) 'while) (intpn_while (cdr stmt) env clis c-return c-break c-continue c-throw return_e))
			((equal? (car stmt) 'return) (intpn_return (cdr stmt) env clis c-return c-break c-continue c-throw return_e))
			((equal? (car stmt) 'begin) (intpn_begin (cdr stmt) env clis c-return c-break c-continue c-throw return_e))
			((equal? (car stmt) 'break) (intpn_break env c-break))
			((equal? (car stmt) 'continue) (intpn_continue env c-continue))
			((equal? (car stmt) 'try) (intpn_try (cdr stmt) env clis c-return c-break c-continue c-throw return_e))
			((equal? (car stmt) 'throw) (intpn_throw (cdr stmt) env clis c-return c-break c-continue c-throw c-throw))
			((equal? (car stmt) 'function) (intpn_function (cdr stmt) env clis c-return c-break c-continue c-throw return_e))
			((equal? (car stmt) 'funcall) (intpn_funcall (cdr stmt) env clis c-return c-break c-continue c-throw return_e))
			(else (intpn_expr_auto stmt env clis c-return c-break c-continue c-throw return_e))
		)
	))

;The dot operator is handled with two functions. The entering routine extracts the correct environment for the right operand to be properly evaluated, and the exiting routine puts back the modified extracted environment.
;The dot operator is used where a varname or fname would be expected. Therefore, it returns two item: the name to be looked up, and the extracted environment.
;The left operand is expected to be one of the following, with its corresponding handling process:
;this: lookup 'this' in the current environment, return the layers of and after which 'this' is found.
;super: lookup 'this' in the current environment, return the layers only after which 'this' is found with a 'this' appended onto the top layer of the returning layers.
;symbol being special 'this' marker: same as 'this'
;symbol being an instance: return the environment of this instance
;symbol being a class: return the static environment of this class
(define intpn_dot_routine_enter
	(lambda (stmt env clis c-return c-break c-continue c-throw return_e_de)
		(cond
			((list? (car stmt)) (intpn_stmt_auto (car stmt) env clis c-return c-break c-continue c-throw (lambda (e)
				(eval_expr_auto (car stmt) env clis c-return c-break c-continue c-throw (lambda (v)
					(return_e_de v e)))
			)))
			((equal? (car stmt) 'this) (EL_Sdump env 'this (lambda (e de)
				(intpn_dot_func_expand_top e (lambda (e2)
					(return_e_de e2 de)))
			)))
			((equal? (car stmt) 'super) (EL_Sdump env 'this (lambda (e de)
				(intpn_dot_func_warp_top e (lambda (e2)
					(return_e_de e2 de)))
			)))
			((EL_SL_check (car stmt) env (lambda (b) b)) (EL_SL_get (car stmt) env (lambda (ve)
				(if (equal? ve '__SCIS_THIS)
					(EL_Sdump env 'this (lambda (e de)
						(intpn_dot_func_expand_top e (lambda (e2)
							(return_e_de e2 de)))
					))
					(return_e_de ve env))
			)))
			((CL_check (car stmt) clis (lambda (b) b)) (CL_get (car stmt) clis (lambda (c)
				(return_e_de (cadr c) env))
			))
			(else (error (string-append "Unknown dot entering routing operand " (symbol->string (car stmt)) "."))))
	))


(define intpn_dot_routine_exit
	(lambda (stmt env denv clis return_e_cl)
		(cond
			((list? (car stmt)) (return_e_cl denv clis))
			((equal? (car stmt) 'this) (intpn_dot_func_collapse_top env (lambda (e)
				(EL_Srestore e denv (lambda (e2) (return_e_cl e2 clis)))
			)))
			((equal? (car stmt) 'super) (intpn_dot_func_restore_top env (lambda (e)
				(EL_Srestore e denv (lambda (e2) (return_e_cl e2 clis)))
			)))
			((EL_SL_check (car stmt) denv (lambda (b) b)) (EL_SL_get (car stmt) denv (lambda (ve)
				(if (equal? ve '__SCIS_THIS)
					(intpn_dot_func_collapse_top env (lambda (e)
						(EL_Srestore e denv (lambda (e2) (return_e_cl e2 clis)))
						))
					(EL_SL_set (list (car stmt) env) denv (lambda (e)
						(return_e_cl e clis))
					))
			)))
			((CL_check (car stmt) clis (lambda (b) b)) (CL_set (car stmt) env clis (lambda (cl)
				(return_e_cl '() cl))
			))
			(else (error (string-append "Unknown dot exiting routine operand " (symbol->string denv) "."))))
	))

;Move the top layer of one env onto another. Used after resolving the argument list of function call, so as to move the resolved list of arguments onto the environment of the class instance on which the function is executed.
(define intpn_dot_func_transfer_toplayer
	(lambda (env e_func return_e)
		(EL_dumpone e_func (lambda (dummy top_cache)
			(EL_restoreone env top_cache return_e)
		))
	))

;To retain the child class information when super member is called, we save a copy of the top layer of the env (i.e. the child class layer) into the super class env, so that it could be expanded when 'this is called in the future.
;All the following four functions are the workaround so that we are able to refer to the runtime type when 'this' is referred to in a super class function.
(define intpn_dot_func_warp_top
	(lambda (env return_e)
		(EL_SL_add (list '__SCIS_COLLAPSED_THIS (list (caar env) (caadr env))) (list (cdar env) (cdadr env)) return_e)
	))

(define intpn_dot_func_restore_top
	(lambda (env return_e)
		(if (EL_SL_check '__SCIS_COLLAPSED_THIS env (lambda (b) b))
			(EL_SL_get '__SCIS_COLLAPSED_THIS env (lambda (ev)
				(EL_SL_rm '__SCIS_COLLAPSED_THIS env (lambda (e)
					(return_e (list (cons (car ev) (car e)) (cons (cadr ev) (cadr e))))
				))
			))
		)
	))

(define intpn_dot_func_expand_top
	(lambda (env return_e)
		(if (S_check '__SCIS_COLLAPSED_THIS (caar env) (lambda (b) b))
			(EL_SL_get '__SCIS_COLLAPSED_THIS env (lambda (e)
				(intpn_dot_func_expand_top (list (cons (car e) (car env)) (cons (cadr e) (cadr env))) return_e)
			))
			(return_e env))
	))

(define intpn_dot_func_collapse_top
	(lambda (env return_e)
		(EL_popLayer env (lambda (e_nxtlyr)
			(if (EL_SL_check '__SCIS_COLLAPSED_THIS e_nxtlyr (lambda (b) b))
				(EL_SL_set (list '__SCIS_COLLAPSED_THIS (list (caar env) (caadr env))) (list (cdar env) (cdadr env)) (lambda (e)
					(intpn_dot_func_collapse_top e return_e)
				))
				(return_e env))
		))
	))

;Handles the dot operator when used in assignment/expression evaluation.
(define intpn_dot_var
	(lambda (stmt env clis c-return c-break c-continue c-throw return_e)
		(if (list? (car stmt))
			(intpn_stmt_auto (car stmt) env clis c-return c-break c-continue c-throw return_e)
			(return_e env))
	))

(define eval_dot_var
	(lambda (stmt env clis c-return c-break c-continue c-throw return_v)
		(intpn_dot_routine_enter stmt env clis c-return c-break c-continue c-throw (lambda (e de)
			(EL_SL_get (cadr stmt) e (lambda (v)
				(if (equal? v '__SCIS_THIS)
					(EL_Sdump env 'this (lambda (e2 de) (return_v e2)))
					(return_v v))
			))
		))
	))

;The new operator looks up the class name from the ClassList, extracts the non-static member lists from the definition, and checks if it is extended from another class. If it is, the parent class(es) is/are recursively looked up and the non-static membet lists are appended.
(define eval_new
	(lambda (stmt clis return_v)
		(CL_get (car stmt) clis (lambda (c)
			(if (null? (car c))
				(EL_SL_add (list 'this '__SCIS_THIS) (caddr c) return_v)
				(eval_new (car c) clis (lambda (e2)
					(EL_SL_add (list 'this '__SCIS_THIS) (caddr c) (lambda (e1)
						(EL_append e1 e2 return_v)
					))
				)))
		))
	))

;Handles assignment on declaration of variables in classes.
(define intpn_class_var
	(lambda (stmt env return_e)
		(if (null? (cdr stmt))
			(EL_SL_add (list (car stmt) '()) env return_e)
			(eval_expr_auto (cadr stmt) env '() '() '() '() '() (lambda (v)
				(intpn_expr_auto (cadr stmt) env '() '() '() '() '() (lambda (e)
					(EL_SL_add (list (car stmt) v) e return_e)))))
		)))

(define intpn_class_function
	(lambda (stmt env return_e)
		(EL_FL_add stmt env return_e)
	))

;Extracts the functions and variables in the class definition and build the corresponding EnvironmentList structure.
(define intpn_class_builder
	(lambda (stmt clis ClsMeta_name ClsMeta_ext stat-env env return_cl)
		(cond
			((null? stmt) (CL_add clis ClsMeta_name ClsMeta_ext stat-env env return_cl))
			((not (list? (car stmt))) (error (string-append "Error parsing class declarations. " (symbol->string (car stmt)) " is not a proper start of a class member.")))
			((equal? (caar stmt) 'static-var) (intpn_class_var (cdar stmt) stat-env
				(lambda (e) (intpn_class_builder (cdr stmt) clis ClsMeta_name ClsMeta_ext e env return_cl))
			))
			((equal? (caar stmt) 'static-function) (intpn_class_function (cdar stmt) stat-env
				(lambda (e) (intpn_class_builder (cdr stmt) clis ClsMeta_name ClsMeta_ext e env return_cl))
			))
			((equal? (caar stmt) 'var) (intpn_class_var (cdar stmt) env
				(lambda (e) (intpn_class_builder (cdr stmt) clis ClsMeta_name ClsMeta_ext stat-env e return_cl))
			))
			((equal? (caar stmt) 'function) (intpn_class_function (cdar stmt) env
				(lambda (e) (intpn_class_builder (cdr stmt) clis ClsMeta_name ClsMeta_ext stat-env e return_cl))
			))
			(else (error "Bad class member!")))
	))

(define intpn_class
	(lambda (stmt clis return_cl)
		(EL_init (lambda (e1)
			(EL_init (lambda (e2)
				(if (null? (cadr stmt))
					(intpn_class_builder (caddr stmt) clis (car stmt) '() e1 e2 return_cl)
					(intpn_class_builder (caddr stmt) clis (car stmt) (cdadr stmt) e1 e2 return_cl))
			))
		))
	))

;Outest layer wrapper. Interprets class declarations. To be initially called with '() as clis.
(define parse_class
	(lambda (stmt clis return_cl)
		(cond
			((null? stmt) (return_cl clis))
			((list? (car stmt)) (parse_class (car stmt) clis 
				(lambda (cl) (parse_class (cdr stmt) cl return_cl))))
			((equal? (car stmt) 'class) (intpn_class (cdr stmt) clis return_cl))
			(else (error "Bad class declaration!")))
	))

;Error functions. Produces errors on invalid jumps.
(define invalid_goto
	(lambda (dummy)
		(error "Invalid break or continue instruction.")
	))

(define invalid_throw
	(lambda (dummy1 dummy2)
		(error "Invalid throw instruction.")
	))

(define missing_return
	(lambda ()
		(error "Missing return statment.")
	))

(define missing_main
	(lambda (dummy1 dummy2)
		(error "Missing main function.")
	))

;Load the sample parser
(load "classParser.scm")

;Load the env operation functions
(load "envList.scm")

;Interface Function
;(define interpret
;	(lambda (fname)
;		(EL_init (lambda (e)
;			(intpn_stmt_auto (parser fname) e
;				missing_main
;				invalid_goto
;				invalid_goto
;				invalid_throw
;				missing_return)
;		))
;	))

(define interpret
	(lambda (filename cname)
		(parse_class (parser filename) '() (lambda (clis)
			(EL_init (lambda (e)
				(eval_funcall (list (list 'dot (string->symbol cname) 'main)) e clis
					(lambda (e2 v) (cond
						((equal? v #t) 'true)
						((equal? v #f) 'false)
						(else v)))
					invalid_goto
					invalid_goto
					invalid_throw
					(lambda (v) (cond
						((equal? v #t) 'true)
						((equal? v #f) 'false)
						(else v)))
				)
			))
		))
	))