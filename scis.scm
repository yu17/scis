;EECS 345 Interpreter Project Part 2
;Group 39. Jiaqi Yu, Yiquan Zhu, Renjie Xi


(define eval_expr_symbol
	(lambda (expr state return_v)
		(cond
			((or (number? expr)
				(boolean? expr)
				(string? expr)) (return_v expr))
			((equal? (string-upcase (symbol->string expr)) "TRUE") (return_v #t))
			((equal? (string-upcase (symbol->string expr)) "FALSE") (return_v #f))
			(else (SL_get expr state return_v)))
	))

(define eval_expr_add
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (car operands) s (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Adding non-number values!")
						(return_v (+ v1 v2)))
				))))))
	))

(define eval_expr_sub
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(cond
					((not (number? v1)) (error "Subtracting/Negating non-number values!"))
					((not (null? (cdr operands))) (eval_expr_auto (car operands) s (lambda (v2)
						(if (not (number? v2))
							(error "Subtracting non-number values!")
							(return_v (- v1 v2)))
					)))
					(return_v (- v1)))
			))))
		))

(define eval_expr_mult
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (car operands) s (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Multiplying non-number values!")
						(return_v (* v1 v2)))
				))))))
	))

(define eval_expr_div
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (car operands) s (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Dividing non-number values!")
						(return_v (quotient v1 v2)))
				))))))
	))

(define eval_expr_mod
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (car operands) s (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Dividing non-number values!")
						(return_v (modulo v1 v2)))
				))))))
	))

(define eval_expr_and
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(if (or (not v1) (equal? v1 0))
				(return_v #f)
				(intpn_expr_auto (car operands) state (lambda (s)
					(eval_expr_auto (car operands) s (lambda (v2)
						(return_v (and v2 (not (equal? v2 0))))
					))))
				)))
	))

(define eval_expr_or
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(if (and v1 (not (equal? v1 0)))
				(return_v #t)
				(intpn_expr_auto (car operands) state (lambda (s)
					(eval_expr_auto (car operands) s (lambda (v2)
						(return_v (not (or (not v2) (equal? v2 0))))
					))))
				)))
	))

(define eval_expr_not
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v)
			(return_v (or (not v) (equal? v 0)))
		))
	))

(define eval_expr_eq
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (car operands) s (lambda (v2)
					(return_v (equal? v1 v2))
				))))))
	))

(define eval_expr_neq
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (car operands) s (lambda (v2)
					(return_v (not (equal? v1 v2)))
				))))))
	))

(define eval_expr_lt
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (car operands) s (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Comparing non-number values!")
						(return_v (< v1 v2)))
				))))))
	))

(define eval_expr_gt
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (car operands) s (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Comparing non-number values!")
						(return_v (> v1 v2)))
				))))))
	))

(define eval_expr_le
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (car operands) s (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Comparing non-number values!")
						(return_v (<= v1 v2)))
				))))))
	))

(define eval_expr_ge
	(lambda (operands state return_v)
		(eval_expr_auto (car operands) state (lambda (v1)
			(intpn_expr_auto (car operands) state (lambda (s)
				(eval_expr_auto (car operands) s (lambda (v2)
					(if (not (and (number? v1) (number? v2)))
						(error "Comparing non-number values!")
						(return_v (>= v1 v2)))
				))))))
	))

(define eval_expr_assign
	(lambda (operands state return_v)
		(eval_expr_auto (cadr operands) state return_v)
	))

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

(define intpn_expr_noeffectoperator
	(lambda (operands state return_s)
		(intpn_expr_auto (car operands) state (lambda (s)
			(if (not (null? (cdr operands)))
				(intpn_expr_auto (cadr operands) s return_s)
				(return_s s))))
	))

(define intpn_expr_assign
	(lambda (operands state return_s)
		(eval_expr_auto (cadr operands) state (lambda (v)
			(intpn_expr_auto (cadr operands) state (lambda (s)
				(SL_set (list (car operands) v) s return_s)))))
	))
		

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
			(else (intpn_expr_auto stmt state c-return c-break c-continue c-throw return_s))
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