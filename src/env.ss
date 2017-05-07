(define *prim-proc-names* '(+ - * / add1 sub1 = > < >= <= zero? not quotient cons member car cdr cadr caar cdar cddr caaar cdaar cadar caadr cddar cdadr caddr cdddr list null? assq eq? equal? atom? length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! display newline cadar map apply append eqv? list-tail))

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3
(define empty-env
  (lambda ()
    (empty-env-record)
  )
)

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)
  )
)

(define mutate-env
  (lambda (sym val env)
    (cases environment env
      [empty-env-record ()
        (mutate-global-env! sym val)
      ]
      [extended-env-record (syms vals env)
        (let ([index (list-find-position sym syms)])
          (if index
            (set-at-index! index val vals)
            (mutate-env sym val env)
          )
        )
      ]
      [else
        (errorf 'mutate-env "[ ERROR ]: undefined environment ~% --- the given environment is undefined: ~s" env)
      ]
    )
  )
)

; Returns the index of the sym in los. If it is not is los, returns #f
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)
  )
)

(define set-at-index!
  (lambda (index val ls)
    (if (zero? index)
      (set-car! ls val)
      (set-at-index! (- index 1) val (cdr ls))
    )
  )
)

; Returns the index where pred is true in ls
(define list-index
  (lambda (pred ls)
    (cond
      [(null? ls) #f]
      [(pred (car ls)) 0]
      [else
        (let ([list-index-r (list-index pred (cdr ls))])
	        (if (number? list-index-r)
		          (+ 1 list-index-r)
		          #f
          )
        )
      ]
    )
  )
)

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      [empty-env-record ()
        (fail)
      ]
      [extended-env-record (syms vals env)
	      (let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	            (apply-k succeed (list-ref vals pos))
	            (apply-env env sym succeed fail)
          )
        )
      ]
      (errorf 'apply-env "[ ERROR ]: undefined environment ~% --- the given environment is undefined: ~s" env)
    )
  )
)

(define make-init-env         ; for now, our initial global environment only contains
  (lambda ()
    (extend-env            ; procedure names.  Recall that an environment associates
       *prim-proc-names*   ;  a value (not an expression) with an identifier.
       (map prim-proc
            *prim-proc-names*)
       (empty-env)
    )
  )
)

(define reset-global-env
  (lambda ()
    (set! global-env (make-init-env))
  )
)

(define global-env (make-init-env))

(define mutate-global-env!
  (lambda (sym val)
    (cases environment global-env
      [extended-env-record (syms vals env)
        (let ([index (list-find-position sym syms)])
          (if index
            (set-at-index! index val vals)
            (set! global-env (extended-env-record (cons sym syms) (cons val vals) env)) ; env is the init-env's empty-env
          )
        )
      ]
      [else
        (errorf 'mutate-global-env! "[ ERROR ]: Incompatible argument ~% --- global-env must be an environment of type extended-env-record but is an empty-env-record")
      ]
    )
  )
)
