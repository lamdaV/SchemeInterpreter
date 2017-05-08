(define map-cps
	(lambda (proc-cps L k)
		(if (null? L)
			(apply-k k '())
			(map-cps
				proc-cps
				(cdr L)
				(proc-k proc-cps (car L) k)
			)
		)
	)
)

(define eval-bodies
  (lambda (bodies env k)
    (if (null? (cdr bodies))
      (eval-exp (car bodies) env k)
      ; (eval-bodies (cdr bodies) env)
      (eval-exp (car bodies) env (eval-body-k cdr-bodies env k))
    )
  )
)

; evaluate the list of operands, putting results into a list
(define eval-rands
  (lambda (rands env k)
    (map-cps (lambda (exp c) (eval-exp exp env c)) rands k)
  )
)

(define eval-one-exp
  (lambda (x)
    (top-level-eval (syntax-expand (parse-exp x)))
  )
)

(define top-level-eval
  (lambda (form)
    (eval-exp form
      (empty-env)
      (init-k)
    )
  )
)

; eval-exp is the main component of the interpreter

(define eval-exp
  (let ([identity-proc (lambda (x) x)])
    (lambda (exp env k)
      (cases expression exp
        [lit-exp (datum)
          (apply-k k datum)
        ]
        [var-exp (variable)
          (apply-env env
                     variable
                     k ; procedure to call if var is in env
                     (apply-global-k 'apply-env variable k))
        ]
        [void-exp ()
          (apply-k k (void))
        ]
        [if-then-exp (conditional true-exp)
        ;   (if (eval-exp conditional env)
        ;     (eval-exp true-exp env)
        ;     (void)
        ;   )
          (eval-exp conditional env (branch-one-k true-exp k))
        ]
        [if-else-exp (conditional true-exp false-exp)
          ; (if (eval-exp conditional env)
          ;   (eval-exp true-exp env)
          ;   (eval-exp false-exp env)
          ; )
          (eval-exp conditional env (branch-two-k true-exp false-exp k))
        ]
        [app-exp (operator arguments)
          ; (let ([proc-value (eval-exp operator env)]
          ;       [args (eval-rands arguments env)])
          ;   (apply-proc proc-value args)
          ; )
          (eval-exp operator env (operator-k arguments env k))
        ]
        [let-exp (let-type name variables values body)
          (cond
            [(equal? 'let let-type)
              (if name
                (errorf 'eval-exp "[ ERROR ]: Unsupported let type ~% --- name let unsupported: ~s" name) ; TODO
                ;(eval-bodies body (extend-env variables (eval-rands values env) env))
                (eval-rands values env (eval-rands-k body variables env k))
              )
            ]
            [(equal? 'letrec let-type)
              (if name
                (errorf 'eval-exp "[ ERROR ]: Unsupported let type ~% --- named letrec is unsupported: ~s" (unparse-exp exp))
                (let* ([temp-values (make-list (length values) #f)]
                       [temp-env (extend-env variables temp-values env)])
                  ; for each variables, mutate env
                  ; (for-each (lambda (variable value) (eval-exp value temp-env) variables values)
                  ; (for-each (lambda (variable value) (mutate-env variable (eval-exp value temp-env) temp-env)) variables values)
                  ; (eval-bodies body temp-env k)
                  (eval-exp (car values) temp-env (for-each-k variables values temp-env body k))
                )
              )
            ]
            [else
              (errorf 'eval-exp "[ ERROR ]: Unsupported let type ~% --- let-type: ~s" let-type)
              ; TODO:
            ]
          )
        ]
        [lambda-exp (required optional body)
          (apply-k k (closure (append required (list optional)) body env))
        ]
        [lambda-exact-exp (variables body)
          (apply-k k (closure variables body env))
        ]
        [set!-exp (variable value)
          ; (mutate-env variable (eval-exp value env) env)
          (eval-exp value env (set-k variable env k))
        ]
        [define-exp (identifier value)
          ; (mutate-global-env! identifier (eval-exp value env))
          (eval-exp value env (define-k identifier k))
        ]
        [else
          (errorf 'eval-exp "[ ERROR ]: Malformed syntax ~% --- unexpected expression: ~s" (unparse-exp exp))
        ]
      )
    )
  )
)
