(define eval-bodies
  (lambda (bodies env)
    (if (null? (cdr bodies))
      (eval-exp (car bodies) env)
      (begin
        (eval-exp (car bodies) env)
        (eval-bodies (cdr bodies) env)
      )
    )
  )
)

; evaluate the list of operands, putting results into a list
(define eval-rands
  (lambda (rands env)
    (map (lambda (exp) (eval-exp exp env)) rands)
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
    )
  )
)

; eval-exp is the main component of the interpreter

(define eval-exp
  (let ([identity-proc (lambda (x) x)])
    (lambda (exp env)
      (cases expression exp
        [lit-exp (datum) datum]
        [var-exp (variable)
          (apply-env env
            variable
            identity-proc ; procedure to call if var is in env
            (lambda () ; procedure to call if var is not in env
              (apply-env global-env
                         variable ; look up its value.
                         identity-proc ; procedure to call if variable is in the environment
                         (lambda ()
                           (errorf 'apply-env ; procedure to call if variable not in env
                                  "[ ERROR ]: Unable to find variable in environment ~% --- variable not found in environment: ~s"
                                  variable)))
            )
          )
        ]
        [void-exp () (void)]
        [if-then-exp (conditional true-exp)
          (if (eval-exp conditional env)
            (eval-exp true-exp env)
            (void)
          )
        ]
        [if-else-exp (conditional true-exp false-exp)
          (if (eval-exp conditional env)
            (eval-exp true-exp env)
            (eval-exp false-exp env)
          )
        ]
        [cond-exp (conditionals bodies)
          (eval-exp (syntax-expand exp) env)
        ]
        [begin-exp (body)
          (eval-exp (syntax-expand exp) env)
        ]
        [app-exp (operator arguments)
          (let ([proc-value (eval-exp operator env)]
                [args (eval-rands arguments env)])
            (apply-proc proc-value args)
          )
        ]
        [let-exp (let-type name variables values body)
          (cond
            [(equal? 'let let-type)
              (if name
                (errorf 'eval-exp "[ ERROR ]: Unsupported let type ~% --- name let unsupported: ~s" name) ; TODO
                (eval-bodies body (extend-env variables (eval-rands values env) env))
              )
            ]
            [else
              (errorf 'eval-exp "[ ERROR ]: Unsupported let type ~% --- let-type: ~s" let-type)
              ; TODO:
            ]
          )
        ]
        [lambda-exp (required optional body)
          (if (null? required)
            (closure (cons required optional) body env)
            (closure (append required optional) body env)
          )
        ]
        [lambda-exact-exp (variables body)
          (closure variables body env)
        ]
        [set!-exp (variable value)
          (errorf 'eval-exp "[ ERROR ]: Unsupported operation ~% --- operation: ~s" (unparse-exp exp)) ; TODO
        ]
        [and-exp (conditionals)
          (eval-exp (syntax-expand exp) env)
        ]
        [or-exp (conditionals)
          (eval-exp (syntax-expand exp) env)
        ]
        [else
          (errorf 'eval-exp "[ ERROR ]: Malformed syntax ~% --- unexpected expression: ~s" (unparse-exp exp))
        ]
      )
    )
  )
)
