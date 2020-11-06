;For detailed explanation on the implementation, please refer to readme.md.

;StateList functions
;Following functions of which the names are prefixed with "SL" are the state stack operation functions.

;state,list
(define SL_pushLayer
	(lambda (s_list return_s)
		(return_s (cons '(() ()) s_list))
	))

;state,list
(define SL_popLayer
	(lambda (s_list return_s)
		(return_s (cdr s_list))
	))

;state,list
(define SL_init
	(lambda (return_s)
		(SL_pushLayer '() return_s)
		))

;bool
(define S_check
	(lambda (varname s_layer return_b)
		(cond
			((null? (car s_layer)) (return_b #f))
			((equal? varname (caar s_layer)) (return_b #t))
			(else (S_check varname (list (cdar s_layer) (cdadr s_layer)) return_b))
		)
	))

;bool
(define SL_check
	(lambda (varname s_list return_b)
		(if (null? s_list)
			(return_b #f)
			(S_check varname (car s_list) (lambda (b) (cond
				(b (return_b b))
				((null? (cdr s_list)) (return_b b))
				(else (SL_check varname (cdr s_list) return_b))
			))))
	))

;state,layer
(define S_add
	(lambda (varpair s_layer return_s)
		(S_check (car varpair) s_layer (lambda (b) (if b
			(error (string-append "The variable " (symbol->string (car varpair)) " is being redefined."))
			(return_s (list (cons (car varpair) (car s_layer)) (cons (cadr varpair) (cadr s_layer))))
		)))
	))

;state,list
(define SL_add
	(lambda (varpair s_list return_s)
		(S_add varpair (car s_list) (lambda (v) (return_s (cons v (cdr s_list)))))
	))

;state,layer
(define S_rm
	(lambda (varname s_layer return_s)
		(cond
			((null? (car s_layer)) (return_s s_layer))
			((equal? varname (caar s_layer)) (return_s (list (cdar s_layer) (cdadr s_layer))))
			(else (S_rm varname (list (cdar s_layer) (cdadr s_layer)) (lambda (v) 
				(S_add (list (caar s_layer) (caadr s_layer)) v return_s))
			))
		)
	))

;state,list
(define SL_rm
	(lambda (varname s_list return_s)
		(S_rm varname (car s_list) (lambda (v) (return_s (cons v (cdr s_list)))))
	))

;value
(define S_get
	(lambda (varname s_layer return_v)
		(cond
			((null? (car s_layer)) (return_v '()))
			((and (equal? varname (caar s_layer)) (null? (caadr s_layer))) (error (string-append "The variable " (symbol->string varname) " is used before initialization.")))
			((equal? varname (caar s_layer)) (return_v (caadr s_layer)))
			(else (S_get varname (list (cdar s_layer) (cdadr s_layer)) return_v))
		)
	))

;value
(define SL_get
	(lambda (varname s_list return_v)
		(if (null? s_list)
			(error (string-append "The variable " (symbol->string varname) " is not declared."))
			(S_get varname (car s_list) (lambda (v)
				(if (null? v)
					(SL_get varname (cdr s_list) return_v)
					(return_v v))
			))
		)
	))

;state,layer;status
(define S_set
	(lambda (varpair s_layer return_s)
		(cond
			((null? (car s_layer)) (return_s s_layer))
			((equal? (car varpair) (caar s_layer)) (S_add varpair (list (cdar s_layer) (cdadr s_layer)) return_s))
			(else (S_set varpair (list (cdar s_layer) (cdadr s_layer)) (lambda (s)
				(S_add (list (caar s_layer) (caadr s_layer)) s return_s))))
		)
	))

;state,list
(define SL_set
	(lambda (varpair s_list return_s)
		(cond
			((null? s_list) (error (string-append "The variable " (symbol->string (car varpair)) " is not declared.")))
			(else (S_check (car varpair) (car s_list) (lambda (b)
				(if b
					(S_set varpair (car s_list) (lambda (v) 
						(return_s (cons v (cdr s_list)))))
					(SL_set varpair (cdr s_list) (lambda (v)
						(return_s (cons (car s_list) v)))))
			))))
	))

;FunctionList funcctions
;Following functions of which the names are prefixzed with "FL" are the function stack operation functions.
(define FL_pushLayer
	(lambda (f_list return_f)
		(return_f (cons '() f_list))
	))

(define FL_popLayer
	(lambda (f_list return_f)
		(return_f (cdr f_list))
	))

(define FL_init
	(lambda (return_f)
		(FL_pushLayer '() return_f)
	))

(define F_check
	(lambda (fname f_layer return_b)
		(cond
			((null? f_layer) (return_b #f))
			((equal? (caar f_layer) fname) (return_b #t))
			(else (F_check fname (cdr f_layer) return_b))
		)
	))

(define F_add
	(lambda (func f_layer return_f)
		(F_check (car func) f_layer (lambda (b) (if b
			(error (string-append "The function " (symbol->string (car func)) " is being redefined."))
			(return_f (cons func f_layer))
		)))
	))

(define FL_add
	(lambda (func f_list return_f)
		(F_add func (car f_list) (lambda (fl) (return_f (cons fl (cdr f_list)))))
	))

(define F_get
	(lambda (fname f_layer return_f)
		(cond
			((null? f_layer) (return_f '()))
			((equal? (caar f_layer) fname) (return_f (cdar f_layer)))
			(else (F_get fname (cdr f_layer) return_f))
		)
	))

(define FL_get
	(lambda (fname f_list return_f)
		(if (null? f_list)
			(error (string-append "The function " (symbol->string fname) " is not defined."))
			(F_get fname (car f_list) (lambda (f)
				(if (null? f)
					(FL_get fname (cdr f_list) return_f)
					(return_f f))
			))
		)
	))

;EnvironmentList functions
;Following functions of which the names are prefixed with "EL" are the wrapper functions for the Statestack and the Functionstack.
(define EL_Fdumper
	(lambda (e_list de_list fname return_e_de)
		(F_check fname (caadr e_list) (lambda (b)
		(if (not b)
			(EL_Fdumper
				(list
					(cdar e_list)
					(cdadr e_list))
				(list
					(cons (caar e_list) (car de_list))
					(cons (caadr e_list) (cadr de_list)))
				fname
				return_e_de)
			(return_e_de e_list de_list))
		))
	))

(define EL_Fdump
	(lambda (e_list fname return_e_de)
			(EL_Fdumper
				(list
					(cdar e_list)
					(cdadr e_list))
				'(() ())
				fname
				(lambda (e de)
					(return_e_de
						(list
							(cons (caar e_list) (car e))
							(cons (caadr e_list) (cadr e)))
						de)))
	))
		

(define EL_Frestorer
	(lambda (e_list de_list return_e)
		(if (not (null? (cadr de_list)))
			(EL_Frestorer
				(list
					(cons (caar de_list) (car e_list))
					(cons (caadr de_list) (cadr e_list)))
				(list
					(cdar de_list)
					(cdadr de_list))
				return_e)
			(return_e e_list))
	))

(define EL_Frestore
	(lambda (e_list de_list return_e)
		(EL_Frestorer
			(list
				(cdar e_list)
				(cdadr e_list))
			de_list
			(lambda (e)
				(return_e
					(list
					(cons (caar e_list) (car e))
					(cons (caadr e_list) (cadr e)))))
		)
	))

(define EL_Sdumper
	(lambda (e_list de_list varname return_e_de)
		(S_check varname (caar e_list) (lambda (b)
		(if (not b)
			(EL_Sdumper
				(list
					(cdar e_list)
					(cdadr e_list))
				(list
					(cons (caar e_list) (car de_list))
					(cons (caadr e_list) (cadr de_list)))
				varname
				return_e_de)
			(return_e_de e_list de_list))
		))
	))

(define EL_Sdump
	(lambda (e_list varname return_e_de)
			(EL_Sdumper
				e_list
				'(() ())
				varname
				return_e_de)
	))
		

(define EL_Srestorer
	(lambda (e_list de_list return_e)
		(if (not (null? (cadr de_list)))
			(EL_Srestorer
				(list
					(cons (caar de_list) (car e_list))
					(cons (caadr de_list) (cadr e_list)))
				(list
					(cdar de_list)
					(cdadr de_list))
				return_e)
			(return_e e_list))
	))

(define EL_Srestore
	(lambda (e_list de_list return_e)
		(EL_Srestorer
			e_list
			de_list
			return_e)
	))

(define EL_dumpone
	(lambda (e_list return_e_do)
		(return_e_do
			(list
				(cdar e_list)
				(cdadr e_list))
			(list
				(caar e_list)
				(caadr e_list))
		)
	))

(define EL_restoreone
	(lambda (e_list do_list return_e)
		(return_e
			(list
				(cons (car do_list) (car e_list))
				(cons (cadr do_list) (cadr e_list))
			))
	))

(define EL_pushLayer
	(lambda (e_list return_e)
		(SL_pushLayer (car e_list) (lambda (sl)
			(FL_pushLayer (cadr e_list) (lambda (fl)
				(return_e (list sl fl))
			))
		))
	))

(define EL_popLayer
	(lambda (e_list return_e)
		(SL_popLayer (car e_list) (lambda (sl)
			(FL_popLayer (cadr e_list) (lambda (fl)
				(return_e (list sl fl))
			))
		))
	))

(define EL_init
	(lambda (return_e)
		(SL_init (lambda (sl)
			(FL_init (lambda (fl)
				(return_e (list sl fl))
			))
		))
	))

(define EL_SL_add
	(lambda (varpair e_list return_e)
		(SL_add varpair (car e_list) (lambda (sl) (return_e (cons sl (cdr e_list)))))
	))

(define EL_SL_get
	(lambda (varname e_list return_v)
		(SL_get varname (car e_list) return_v)
	))

(define EL_SL_set
	(lambda (varpair e_list return_e)
		(SL_set varpair (car e_list) (lambda (sl) (return_e (cons sl (cdr e_list)))))
	))

(define EL_SL_rm
	(lambda (varname e_list return_e)
		(SL_rm varname (car e_list) (lambda (sl) (return_e (cons sl (cdr e_list)))))
	))

(define EL_FL_add
	(lambda (func e_list return_e)
		(FL_add func (cadr e_list) (lambda (fl) (return_e (list (car e_list) fl))))
	))

(define EL_FL_get
	(lambda (fname e_list return_f)
		(FL_get fname (cadr e_list) return_f)
	))

(define EL_SL_check
	(lambda (varname e_list return_b)
		(SL_check varname (car e_list) return_b)
	))

(define EL_append
	(lambda (e_list1 e_list2 return_e)
		(return_e (list (append (car e_list1) (car e_list2)) (append (cadr e_list1) (cadr e_list2))))
	))

;ClassList data structure idea: (parent (static-var) (static-func) (var) (func))
;Actual implementation: ( (name (parent) (static-EL) (EL)) ... )
;When initializing instance, append (EL) from parent (and subsequent parents) to (EL) of itself

(define CL_add
	(lambda (c_list ClsMeta_name ClsMeta_ext ClsMeta_stat-env ClsMeta_env return_cl)
		(return_cl (cons (list ClsMeta_name ClsMeta_ext ClsMeta_stat-env ClsMeta_env) c_list))
	))

(define CL_check
	(lambda (cname c_list return_b)
		(cond
			((null? c_list) (return_b #f))
			((equal? (caar c_list) cname) (return_b #t))
			(else (CL_check cname (cdr c_list) return_b))
		)
	))

(define CL_get
	(lambda (cname c_list return_c)
		(cond
			((null? c_list) (error (string-append "The class " (symbol->string cname) " is not defined.")))
			((equal? (caar c_list) cname) (return_c (cdar c_list)))
			(else (CL_get cname (cdr c_list) return_c)))
	))

(define CL_set
	(lambda (cname c_stat-env c_list return_cl)
		(cond
			((null? c_list) (return_cl c_list))
			((equal? cname (caar c_list)) (return_cl (cons (list (caar c_list) (cadar c_list) c_stat-env (car (cdddar c_list))) (cdr c_list))))
			(else (CL_set cname c_stat-env (cdr c_list) (lambda (cl)
				(return_cl (cons (car c_list) cl)))
			)))
	))

(define S_set
	(lambda (varpair s_layer return_s)
		(cond
			((null? (car s_layer)) (return_s s_layer))
			((equal? (car varpair) (caar s_layer)) (S_add varpair (list (cdar s_layer) (cdadr s_layer)) return_s))
			(else (S_set varpair (list (cdar s_layer) (cdadr s_layer)) (lambda (s)
				(S_add (list (caar s_layer) (caadr s_layer)) s return_s))))
		)
	))