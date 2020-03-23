; StateList functions
; Following functions of which the names are prefixed with "SL" are the state operation functions.

;state,list
(define SL_pushLayer
	(lambda (s_list c-return_s)
		(c-return_s (cons '(() ()) s_list))
	))

;state,list
(define SL_popLayer
	(lambda (s_list c-return_s)
		(c-return_s (cdr s_list))
	))

;state,list
(define SL_init
	(lambda (c-return_s)
		(SL_pushLayer '() c-return_s)
		))

;bool
(define L_check
	(lambda (varname s_layer c-return_b)
		(cond
			((null? (car s_layer)) (c-return_b #f))
			((equal? varname (caar s_layer)) (c-return_b #t))
			(else (L_check varname (list (cdar s_layer) (cdadr s_layer)) c-return_b))
		)
	))

;bool
(define SL_check
	(lambda (varname s_list c-return_b)
		(L_check varname (car s_list) (lambda (v) (if (v)
			(c-return_b v)
			(SL_check varname (cdr s_list) c-return_b)
		)))
	))

;state,layer
(define L_add
	(lambda (varpair s_layer c-return_s)
		(L_check (car varpair) s_layer (lambda (V) (if (v)
			(error (string-append "The variable " (symbol->string (car varpair)) " is being redefined."))
			(c-return_s (list (cons (car varpair) (car s_layer)) (cons (cadr varpair) (cadr s_layer))))
		)))
	))

;state,list
(define SL_add
	(lambda (varpair s_list c-return_s)
		(L_add varpair (car s_list) (lambda (v) (c-return_s (cons v (cdr s_list)))))
	))

;state,layer
(define L_rm
	(lambda (varname s_layer c-return_s)
		(cond
			((null? (car s_layer)) c-return_s(s_layer))
			((equal? varname (caar s_layer)) (c-return_s (list (cdar s_list) (cdadr s_list))))
			(else (L_rm varname (list (cdar s_layer) (cdadr s_layer)) (lambda (v) 
				(L_add (list (caar s_layer) (caadr s_layer)) v c-return_s))
			))
		)
	))

;state,list
(define SL_rm
	(lambda (varname s_list c-return_s)
		(L_rm varname (car s_list) (lambda (v) (c-return_s (cons v (cdr s_list)))))
	))

;value
(define L_get
	(lambda (varname s_layer c-return_v)
		(cond
			((null? (car s_layer)) (c-return_v '()))
			((and (equal? varname (caar s_layer)) (null? (caadr s_layer))) (error (string-append "The variable " (symbol->string varname) " is used before initialization.")))
			((equal? varname (caar s_layer)) (c-return_v (caadr s_layer)))
			(else (L_get varname (list (cdar s_layer) (cdadr s_layer)) c-return_v))
		)
	))

;value
(define SL_get
	(lambda (varname s_list c-return_v)
		(if (null? s_list)
			(error (string-append "The variable " (symbol->string varname) " is not declared."))
			(L_get varname (car s_list) (lambda (v)
				(if (null? v)
					(SL_get varname (cdr s_list) c-return_v)
					(c-return_v v))
			))
		)
	))

;state,layer;status
(define L_set
	(lambda (varpair s_layer c-return_s)
		(cond
			((null? (car s_layer)) (c-return_s s_layer))
			((equal? (car varpair) (caar s_layer)) (L_add varpair (list (cdar s_layer) (cdadr s_layer))))
			(else (L_add (list (caar s_layer) (caadr s_layer)) (L_set varpair (list (cdar s_layer) (cdadr s_layer)))))
		)
	))

;state,list
(define SL_set
	(lambda (varpair s_list c-return_s)
		(cond
			((null? s_list) (error (string-append "The variable " (symbol->string (car varpair)) " is not declared.")))
			(L_check (car varpair) (car s_list) (lambda (v1)
				(if v1
					(L_set varpair (car s_list) (lambda (v2) 
						(c-return_s (cons v2 (cdr s_list)))))
					(SL_set varpair (cdr s_list) (lambda (v2)
						(c-return_s (cons (car s_list) v2)))))
			)))
	))