;EECS 345 Interpreter Project Part 2
;Group 39. Jiaqi Yu, Yiquan Zhu, Renjie Xi

;Value function. Keeps the literals and convert the variables to literals.
(define eval_expr_symbol
	(lambda (expr state return_v)
		(cond
			((or (number? expr)
				(boolean? expr)) (return_v expr))
			((equal? (string-upcase (symbol->string expr)) "TRUE") (return_v #t))
			((equal? (string-upcase (symbol->string expr)) "FALSE") (return_v #f))
			(else (SL_get expr state return_v)))
	))

;Value function. Call eval_expr_auto on the operands first and then return the sum of the results.
(define eval_expr_add
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (cadr operands) s (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Adding non-number values!")
						(return_v (+ v1 v2)))
				))))))
	))

;Value function. Call eval_expr_auto on the operands first and then return the difference or the negation of the results.
(define eval_expr_sub
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(cond
					((not (number? v1)) (error "Subtracting/Negating non-number values!"))
					((not (null? (cdr operands))) (eval_expr_auto (cadr operands) s (lambda (v2)
						(if (not (number? v2))
							(error "Subtracting non-number values!")
							(return_v (- v1 v2)))
					)))
					(else (return_v (- v1)))
			)))))
		))

;Value function. Call eval_expr_auto on the operands first and then return the product of the results.
(define eval_expr_mult
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (cadr operands) s (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Multiplying non-number values!")
						(return_v (* v1 v2)))
				))))))
	))

;Value function. Call eval_expr_auto on the operands first and then return the difference of the results.
(define eval_expr_div
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (cadr operands) s (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Dividing non-number values!")
						(return_v (quotient v1 v2)))
				))))))
	))

;Value function. Call eval_expr_auto on the operands first and then return the modulation of the results.
(define eval_expr_mod
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (cadr operands) s (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Dividing non-number values!")
						(return_v (modulo v1 v2)))
				))))))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return the logic and of the results.
(define eval_expr_and
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(if (or (not v1) (equal? v1 0))
				(return_v #f)
				(intpn_expr_auto (car operands) state (lambda (s)
					(eval_expr_auto (cadr operands) s (lambda (v2)
						(return_v (and v2 (not (equal? v2 0))))
					))))
				)))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return the logic or of the results.
(define eval_expr_or
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(if (and v1 (not (equal? v1 0)))
				(return_v #t)
				(intpn_expr_auto (car operands) state (lambda (s)
					(eval_expr_auto (cadr operands) s (lambda (v2)
						(return_v (not (or (not v2) (equal? v2 0))))
					))))
				)))
	))

;Value(boolean) function. Call eval_expr_auto on the operand first and then return the logic not of the result.
(define eval_expr_not
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v)
			(return_v (or (not v) (equal? v 0)))
		))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return whether the results are equal.
(define eval_expr_eq
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (cadr operands) s (lambda (v2)
					(return_v (equal? v1 v2))
				))))))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return whether the results are not equal.
(define eval_expr_neq
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (cadr operands) s (lambda (v2)
					(return_v (not (equal? v1 v2)))
				))))))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return whether the first result is less than the second one.
(define eval_expr_lt
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (cadr operands) s (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Comparing non-number values!")
						(return_v (< v1 v2)))
				))))))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return whether the first result is greater than the second one.
(define eval_expr_gt
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (cadr operands) s (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Comparing non-number values!")
						(return_v (> v1 v2)))
				))))))
	))

;;Value(boolean) function. Call eval_expr_auto on the operands first and then return whether the first result is less or equal to than the second one.
(define eval_expr_le
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (cadr operands) s (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Comparing non-number values!")
						(return_v (<= v1 v2)))
				))))))
	))

;Value(boolean) function. Call eval_expr_auto on the operands first and then return whether the first result is less than or equal to the second one.
(define eval_expr_ge
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (cadr operands) s (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Comparing non-number values!")
						(return_v (>= v1 v2)))
				))))))
	))

;Value function. Return the evaluation result of the value being assigned.
(define eval_expr_assign
	(lambda (operands state return_v)
		(eval_expr_auto (cadr operands) state return_v)
	))

;The sorter function for all the value function. Choose the corresponding evaluation function according to the first atom in the expression and pass the arguments to that function.
(define eval_expr_auto
	(lambda (expr state return_v)
		(cond
			((not (list? expr)) (eval_expr_symbol expr state return_v))
			((equal? (car expr) '+) (eval_expr_add (cdr expr) state return_v))
			((equal? (car expr) '-) (eval_expr_sub (cdr expr) state return_v))
			((equal? (car expr) '*) (eval_expr_mult (cdr expr) state return_v))
			((equal? (car expr) '/) (eval_expr_div (cdr expr) state return_v))
			((equal? (car expr) '%) (eval_expr_mod (cdr expr) state return_v))
			((equal? (car expr) '&&) (eval_expr_and (cdr expr) state return_v))
			((equal? (car expr) (string->symbol "||")) (eval_expr_or (cdr expr) state return_v))
			((equal? (car expr) '!) (eval_expr_not (cdr expr) state return_v))
			((equal? (car expr) '==) (eval_expr_eq (cdr expr) state return_v))
			((equal? (car expr) '!=) (eval_expr_neq (cdr expr) state return_v))
			((equal? (car expr) '<) (eval_expr_lt (cdr expr) state return_v))
			((equal? (car expr) '>) (eval_expr_gt (cdr expr) state return_v))
			((equal? (car expr) '<=) (eval_expr_le (cdr expr) state return_v))
			((equal? (car expr) '>=) (eval_expr_ge (cdr expr) state return_v))
			((equal? (car expr) '=) (eval_expr_assign (cdr expr) state return_v))
			(else (error (string-append "Invalid operator " (symbol->string (car expr)) "."))))
	))

;The dummy function that converts the results of the value functions to booleans.
(define bool_expr_auto
	(lambda (expr state return_b)
		(eval_expr_auto expr state (lambda (v)
			(return_b (and v (not (equal? v 0))))
		))
	))

;State function. Used for all non-assignment expressions. Return the new state after interpreting the operands.
(define intpn_expr_noeffectoperator
	(lambda (operands state return_s)
		(intpn_expr_auto (car operands) state (lambda (s)
			(if (not (null? (cdr operands)))
				(intpn_expr_auto (cadr operands) s return_s)
				(return_s s))))
	))

;State function. Used for the assignment expression. Return the new state after the assignment.
(define intpn_expr_assign
	(lambda (operands state return_s)
		(eval_expr_auto (cadr operands) state (lambda (v)
			(intpn_expr_auto (cadr operands) state (lambda (s)
				(SL_set (list (car operands) v) s return_s)))))
	))
		
;State function. The sorter function for the state function of the expressions.
(define intpn_expr_auto
	(lambda (expr state return_s)
		(cond
			((not (list? expr)) (return_s state))
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
				(equal? (car expr) '>=)) (intpn_expr_noeffectoperator (cdr expr) state return_s))
			((equal? (car expr) '=) (intpn_expr_assign (cdr expr) state return_s)))
	))

;State function. Process the declaration.
(define intpn_var
	(lambda (stmt state return_s)
		(if (null? (cdr stmt))
			(SL_add (list (car stmt) '()) state return_s)
			(eval_expr_auto (cadr stmt) state (lambda (v)
				(intpn_expr_auto (cadr stmt) state (lambda (s)
					(SL_add (list (car stmt) v) s return_s)))))
		)))

;State function. Process the if statement.
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

;State function. Process the while statement.
;Creates the c-break (which is essentially the return_s, which points to the caller before the loop) and the c-continue (which is essentially running the next loop) continuations. 
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

;Value function. Process the return statement and calls c-return to return.
(define intpn_return
	(lambda (stmt state c-return)
		(eval_expr_auto (car stmt) state c-return)
	))

;State function. Process the statement blocks. Creates a new layer when entering the block and remove it when leaving. It also adds a intermediate function to c-throw that removes a layer so that when throw is called in side a code block, the state layer is still properly removed.
(define intpn_begin
	(lambda (stmt state c-return c-break c-continue c-throw return_s)
		(SL_pushLayer state (lambda (s)
			(intpn_stmt_auto stmt s c-return c-break c-continue
			(lambda (s2 e)
				(SL_popLayer s2 (lambda (s3) (c-throw s3 e))))
			(lambda (s2)
				(SL_popLayer s2 return_s)))))
	))

;Goto function. Remove a state layer and calls c-break.
(define intpn_break
	(lambda (state c-break)
		(SL_popLayer state c-break)
	))

;Goto function. Remove a state layer and calls c-continue.
(define intpn_continue
	(lambda (state c-continue)
		(SL_popLayer state c-continue)
	))

;State function. Process the try block. Creates a new layer for the statement block, checks if catch and finally exist and execute them. When it is properly returned, a state layer is removed before executing finally. Otherwise, that layer is removed by the throw function.
(define intpn_try
	(lambda (stmt state c-return c-break c-continue c-throw return_s)
		(SL_pushLayer state (lambda (s)
			(intpn_stmt_auto (car stmt) s c-return c-break c-continue
				(lambda (s e)
					(if (not (null? (cadr stmt)))
						(intpn_catch (cdadr stmt) s e c-return c-break c-continue c-throw (lambda (s2)
							(if (not (null? (caddr stmt)))
								(intpn_finally (cdaddr stmt) s2 c-return c-break c-continue c-throw return_s)
								(return_s s2))
								))
						(if (not (null? (caddr) stmt))
							(intpn_finally (cdaddr stmt) s2 c-return c-break c-continue c-throw return_s)
							(return_s s))))
				(lambda (s) (SL_popLayer s (lambda (s2)
					(if (not (null? (caddr stmt)))
						(intpn_finally (cdaddr stmt) s2 c-return c-break c-continue c-throw return_s)
						(return_s s2))
				)))
			)))
	))
						
;Goto function. Remove a state layer and calls c-throw.
(define intpn_throw
	(lambda (stmt state c-throw)
		(SL_popLayer state (lambda (s) (c-throw s (car stmt))))
	))

;State function. Similar to what the begin function do.
(define intpn_catch
	(lambda (stmt state error c-return c-break c-continue c-throw return_s)
		(SL_pushLayer state (lambda (s)
			(SL_add (list (caar stmt) error) s (lambda (s2)
				(intpn_stmt_auto (cadr stmt) s2 c-return c-break c-continue
				(lambda (s3 e)
					(SL_popLayer s3 (lambda (s4) (c-throw s4 e))))
				(lambda (s3)
					(SL_popLayer s3 return_s)))
					))))
	))

;State function. Similar to what the begin function do.
(define intpn_finally
	(lambda (stmt state c-return c-break c-continue c-throw return_s)
		(SL_pushLayer state (lambda (s)
			(intpn_stmt_auto (car stmt) s c-return c-break c-continue
			(lambda (s2 e)
					(SL_popLayer s2 (lambda (s3) (c-throw s3 e))))
			(lambda (s2)
				(SL_popLayer s2 return_s)))))
	))

;The main sorter function of the interpreter. Takes the result from the parser and execute them with the appropriate interpretation functions.
(define intpn_stmt_auto
	(lambda (stmt state c-return c-break c-continue c-throw return_s)
		(cond
			((null? stmt) (return_s state))
			((null? (car stmt)) (intpn_stmt_auto (cdr stmt) state c-return c-break c-continue c-throw return_s))
			((list? (car stmt)) (intpn_stmt_auto (car stmt) state c-return c-break c-continue c-throw
				(lambda (s) (intpn_stmt_auto (cdr stmt) s c-return c-break c-continue c-throw return_s))))
			((equal? (car stmt) 'var) (intpn_var (cdr stmt) state return_s))
			((equal? (car stmt) 'if) (intpn_if (cdr stmt) state c-return c-break c-continue c-throw return_s))
			((equal? (car stmt) 'while) (intpn_while (cdr stmt) state c-return c-break c-continue c-throw return_s))
			((equal? (car stmt) 'return) (intpn_return (cdr stmt) state c-return))
			((equal? (car stmt) 'begin) (intpn_begin (cdr stmt) state c-return c-break c-continue c-throw return_s))
			((equal? (car stmt) 'break) (intpn_break state c-break))
			((equal? (car stmt) 'continue) (intpn_continue state c-continue))
			((equal? (car stmt) 'try) (intpn_try (cdr stmt) state c-return c-break c-continue c-throw return_s))
			((equal? (car stmt) 'throw) (intpn_throw (cdr stmt) state c-throw))
			(else (intpn_expr_auto stmt state return_s))
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

;Load the sample parser
(load "simpleParser.scm")

;Load the state operation functions
(load "stateList.scm")

;Interface Function
(define interpret
	(lambda (fname)
		(SL_init (lambda (s)
			(intpn_stmt_auto (parser fname) s
				(lambda (v) (cond
					((equal? v #t) 'true)
					((equal? v #f) 'false)
					(else v)))
				invalid_goto
				invalid_goto
				invalid_throw
				'())
		))
	))