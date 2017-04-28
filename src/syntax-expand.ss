(define syntax-expand
  (lambda (exp)
  	(cases expression exp
  		[or-exp (conditionals)
  			(if (null? conditionals)
  				(lit-exp #f) ; if there are no args
  				(letrec
	  				([or-expansion
	  					(lambda (conds)
                (let ([new-car (syntax-expand (car conds))])
                  (if (null? (cdr conds))
                    new-car
                    (if-else-exp
                      new-car
                      new-car
                      (or-expansion (cdr conds))
                    )
                  )
                )
	  					)
	  				])
	  				(or-expansion conditionals)
	  			)
  			)
  		]
  		[and-exp (conditionals)
  			(if (null? conditionals)
  				(lit-exp #t)
  				(letrec
	  				([and-expansion
	  					(lambda (conds)
                (let ([new-car (syntax-expand (car conds))])
                  (if (null? (cdr conds))
                    new-car
                    (if-else-exp
                      new-car
                      (and-expansion (cdr conds))
                      (lit-exp #f)
                    )
                  )
                )
	  					)
	  				])
	  				(and-expansion conditionals)
  				)
  			)
  		]
  	  [begin-exp (body)
  	  	(if (null? body)
  	  		(void-exp)
  	  		(app-exp (lambda-exact-exp '() (map syntax-expand body)) '())
  	  	)
  	  ]
  	  [cond-exp (clauses)
  	  	(letrec ([cond-expansion
  	  			(lambda (clauses)
              (let ([c (car clauses)])
                (if (null? (cdr clauses))
                  (cases clause c
                    [cond-clause (conditional body)
                      (if-then-exp (syntax-expand conditional) (expand-begin body))
                    ]
                    [else-clause (body)
                      (expand-begin body)
                    ]
                    [else (errorf 'cond-expansion "[ ERROR ]: Malformed cond-exp ~% --- cond-exp should contain only cond-clause or else-clause: %s" c)]
                  )
                  (cases clause (car clauses)
                    [cond-clause (conditional body)
                      (if-else-exp (syntax-expand conditional)
                                   (expand-begin body)
                                   (cond-expansion (cdr clauses)))
                    ]
                    [else (errorf 'cond-expansion "[ ERROR ]: Malformed cond-exp ~% --- cond-exp should contain only cond-clause or else-clause: %s" c)]
                  )
                )
              )
            )])
          (cond-expansion clauses)
        )
  	  ]
      [case-exp (key clauses)
        (let ([case-expansion
            (lambda (c)
              (cases clause c
                [case-clause (keys body)
                  (cond-clause (app-exp (var-exp 'member) (list key (lit-exp keys))) body)
                ]
                [else-clause (body)
                  c
                ]
                [else (errorf 'cond-expansion "[ ERROR ]: Malformed case-exp ~% --- case-exp should contain only case-clause or else-clause: %s" c)]
              )
            )
          ])
          (syntax-expand (cond-exp (map case-expansion clauses)))
        )
      ]
      [let-exp (let-type name variables values body)
        (cond
          [(equal? let-type 'let*)
            (letrec ([let*-expansion (lambda (variables values)
                                      (if (null? variables)
                                        (map syntax-expand body)
                                        (list (let-exp 'let name (list (car variables)) (list (car values)) (let*-expansion (cdr variables) (cdr values))))
                                      )
                                    )])
              (let-exp 'let name (list (car variables)) (list (car values)) (let*-expansion (cdr variables) (cdr values)))
            )
          ]
          [(and (equal? let-type 'let) (not name))
            (let-exp let-type name variables (map syntax-expand values) (map syntax-expand body))
          ]
          [name
            (let ([letrec-variables (list name)]
                  [letrec-values (list (lambda-exact-exp variables (map syntax-expand body)))]
                  [letrec-body (list (var-exp name))])
              (app-exp
                (let-exp 'letrec #f letrec-variables letrec-values letrec-body) ; operator-exp
                (map syntax-expand values)) ; list args-exp
            )
          ]
          [(equal? let-type 'letrec)
            (let ([expanded-values (map syntax-expand values)]
                  [expanded-body (map syntax-expand body)])
              (let-exp 'letrec #f variables expanded-values expanded-body)
            )
          ]
          [else ; Other let-types
            exp
          ]
        )
      ]
      [app-exp (operator arguments)
        (app-exp (syntax-expand operator) (map syntax-expand arguments))
      ]
      [lambda-exact-exp (variable body)
        (lambda-exact-exp variable (map syntax-expand body))
      ]
      [lambda-exp (required optional body)
        (lambda-exp required optional (map syntax-expand body))
      ]
      [if-then-exp (conditional true-exp)
        (if-then-exp (syntax-expand conditional) (syntax-expand true-exp))
      ]
      [if-else-exp (conditional true-exp false-exp)
        (if-else-exp (syntax-expand conditional) (syntax-expand true-exp) (syntax-expand false-exp))
      ]
      [set!-exp (variable value)
        (set!-exp variable (syntax-expand value))
      ]
      [while-exp (test body)
        (while-exp (syntax-expand test) (map syntax-expand body))
      ]
  	  [else exp]
  	)
  )
)

(define expand-begin
	(lambda (x)
		(syntax-expand (begin-exp x))
	)
)
