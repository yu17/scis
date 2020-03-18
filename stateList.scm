; StateList functions
; Following functions of which the names are prefixed with "SL" are the state operation functions.

(define SL_pushLayer
	(lambda (s_list)
		(cons '(() ()) s_list)
	))

(define SL_popLayer
	(lambda (s_list)
		(cdr s_list)
	))

;The state is initialized to '(() ()), as two empty list
;Returns the initial empty state list
(define SL_init
	(lambda ()
		(SL_pushLayer '())
		))

;Check if a variable is declared
;Takes a variable name and the state
;Returns #t if the varname is found in state and #f otherwise
(define L_check
	(lambda (varname s_layer)
		(cond
			((null? (car s_layer)) #f)
			((equal? varname (caar s_layer)) #t)
			(else (L_check varname (list (cdar s_layer) (cdadr s_layer))))
		)
	))

(define SL_check
	(lambda (varname s_list)
		(if (L_check varname (car s_list))
			#t
			(SL_check varname (cdr s_list))
		)
	))

;Add a new pair of variable into the state list
;Takes a pair of variable name and value and the state
;Returns a new state with varpair concatenated at the head
(define L_add
	(lambda (varpair s_layer)
		(if (L_check (car varpair) s_layer)
			(error (string-append "The variable " (symbol->string (car varpair)) " is being redefined."))
			(list (cons (car varpair) (car s_layer)) (cons (cadr varpair) (cadr s_layer)))
		)
	))

(define SL_add
	(lambda (varpair s_list)
		(cons (L_add varpair (car s_list)) (cdr s_list))
	))

;Remove a variable from the state list
;Takes a variable name and the state
;Returns a new state with the varname entry removed
(define L_rm
	(lambda (varname s_layer)
		(cond
			((null? (car s_layer)) s_layer)
			((equal? varname (caar s_layer)) (list (cdar s_list) (cdadr s_list)))
			(else (L_add (list (caar s_layer) (caadr s_layer)) (L_rm varname (list (cdar s_layer) (cdadr s_layer)))))
		)
	))

(define SL_rm
	(lambda (varname s_list)
		(cons (L_rm varname (car s_list)) (cdr s_list))
	))

;Get the variable value from the state list
;Takes a variable name and the state
;Returns the corresponding value for varname
(define L_get
	(lambda (varname s_layer)
		(cond
			((null? (car s_layer)) '())
			((and (equal? varname (caar s_layer)) (null? (caadr s_layer))) (error (string-append "The variable " (symbol->string varname) " is used before initialization."))) 
			((equal? varname (caar s_layer)) (caadr s_layer))
			(else (L_get varname (list (cdar s_layer) (cdadr s_layer))))
		)
	))

(define SL_get
	(lambda (varname s_list)
		(cond
			((null? s_list) (error (string-append "The variable " (symbol->string varname) " is not declared.")))
			((null? (L_get varname (car s_list))) (SL_get varname (cdr s_list)))
			(else (L_get varname (car s_list)))
		)
	))

;Assign value to a variable
;Takes a pair of variable name and value and the state
;Returns a new state with the corresponding value of the varpair replaced by the new varpair
(define L_set
	(lambda (varpair s_layer)
		(cond
			((null? (car s_layer)) s_layer)
			((equal? (car varpair) (caar s_layer)) (L_add varpair (list (cdar s_layer) (cdadr s_layer))))
			(else (L_add (list (caar s_layer) (caadr s_layer)) (L_set varpair (list (cdar s_layer) (cdadr s_layer)))))
		)
	))

(define SL_set
	(lambda (varpair s_list)
		(cond
			((null? s_list) (error (string-append "The variable " (symbol->string (car varpair)) " is not declared.")))
			((L_check (car varpair) (car s_list)) (cons (L_set varpair (car s_list)) (cdr s_list)))
			(else (cons (car s_list) (SL_set varpair (cdr s_list))))
		)
	))