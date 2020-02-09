(define SL_add
	(lambda (varpair state)
		(cond
			((null? (car state)) (list (list (car varpair)) (cdr varpair)))
			(else (list (append (list (car varpair)) (car state)) (append (cdr varpair) (cadr state))))
		)
	))

(define SL_rm
	(lambda (varname state)
		(cond
			((null? (car state)) state)
			((eq? varname (caar state)) (list (cdar state) (cdadr state)))
			(else (SL_add (list (caar state) (caadr state)) (SL_rm varname (list (cdar state) (cdadr state)))))
		)
	))

(define SL_get
	(lambda (varname state)
		(cond
			((null? (car state)) (error (string-append "The variable " (symbol->string varname) " is not delcared.")))
			((eq? varname (caar state)) (caadr state))
			(else (SL_get varname (list (cdar state) (cdadr state))))
		)
	))

(define SL_set
	(lambda (varpair state)
		(cond
			((null? (car state)) (error (string-append "The variable " (symbol->string (car varpair)) " is not delcared.")))
			((eq? (car varpair) (caar state)) (SL_add varpair (list (cdar state) (cdadr state))))
			(else (SL_add (list (caar state) (caadr state)) (SL_set varpair (list (cdar state) (cdadr state)))))
		)
	))

