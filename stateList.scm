; StateList functions
; Following functions of which the names are prefixed with "SL" are the state operation functions.

;The state is initialized to '(() ()), as two empty list
;Returns the initial empty state list
(define SL_init
	(lambda ()
		'(() ())
		))

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