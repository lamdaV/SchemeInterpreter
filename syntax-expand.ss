(define syntax-expand
  (lambda (exp)
  	(cases expression exp 
  		[or-exp (conditionals)
  			(if (null? conditionals)
  				(lit-exp #t)
  				(letrec 
	  				([or-expansion
	  					(lambda (conds)
	  						(if (null? (cdr conds))
	  							(syntax-expand (car conds))
	  							(if-else-exp
		  							(syntax-expand (car conds))
		  							(lit-exp #t)
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
  	  [cond-exp (conditions bodies)
  	  	(letrec
  	  		([cond-expansion 
  	  			(lambda (conditions bodies)
  	  				(if (null? (cdr conditions))
  	  					(if (equal? 'else (car conditions))
  	  						(expand-begin (car bodies))
  	  						(if-then-exp
  	  							(syntax-expand (car conditions))
  	  							(expand-begin (car bodies))
  	  						)
  	  					)
  	  					(if-else-exp
	  	  					(syntax-expand (car conditions))
	  	  					(expand-begin (car bodies))
	  	  					(cond-expansion (cdr conditions) (cdr bodies))
	  	  				)
  	  				)
  	  			)
  	  		])
  	  		(cond-expansion conditions bodies)
  	  	)
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