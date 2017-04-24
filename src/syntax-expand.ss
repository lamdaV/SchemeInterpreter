(define syntax-expand
  (lambda (exp)
  	(cases expression exp
  		[or-exp (conditionals)
  			(if (null? conditionals)
  				(lit-exp #f) ; if there are no args
  				(letrec
	  				([or-expansion
	  					(lambda (conds)
	  						(if (null? (cdr conds))
	  							(syntax-expand (car conds))
	  							(if-else-exp
		  							(syntax-expand (car conds))
		  							(car conds)
		  							(or-exp (cdr conds))
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
	  						(if (null? (cdr conds))
	  							(syntax-expand (car conds))
	  							(if-else-exp
		  							(syntax-expand (car conds))
		  							(and-exp (cdr conds))
		  							(lit-exp #f)
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
  	  		(app-exp (lambda-exact-exp '() body) '())
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
          [else ; Other let-types
            exp
          ]
        )
      ]
      [while-exp (test body)
        (if-then-exp (syntax-expand test) (expand-begin (append body (list exp))))
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
