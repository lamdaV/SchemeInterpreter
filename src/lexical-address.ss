(define place
	(lambda (sym positions index)
		(cond
			[(null? positions) -1]
			[(equal? sym (car positions)) index]
			[else (place sym (cdr positions) (add1 index))]
		)
	)
)

(define lex-note
	(lambda (datum depths depth-counter)
		(if (null? depths)
			(free datum)
			(let ([loc (place datum (car depths) 0)])
				(if (equal? loc -1)
					(lex-note datum (cdr depths) (add1 depth-counter))
					(bound depth-counter loc)
				)
			)
		)
	)
)

(define lexical-address
	(lambda (exp)
		(letrec (
			[notate
				(lambda (exp depths)
					(cases expression exp
						[var-exp (variable)
							(lex-exp (lex-note variable depths 0))
						]
						[lit-exp (datum)
							exp
						]
					  [app-exp (operator arguments)
					    (let ([lex-operator (notate operator depths)]
		  	  					[lex-arguments (map-notate arguments depths)])
		  	  			(app-exp lex-operator lex-arguments)
		  	  		)
					  ]
					  [lambda-exact-exp (variables body)
					  	(lambda-exact-exp variables (map-notate body (cons variables depths)))
					  ]
					  [lambda-exp (required optional body)
					  	(lambda-exp required optional
					  		(map-notate body (cons (append required (list optional)) depths)))
					  ]
					  [let-exp (let-type name variables values body)
							(if (equal? 'let let-type)
						  	(let ([notated-values (map-notate values depths)]
						  				[notated-body (map-notate body (cons variables depths))])
						  		(let-exp let-type name variables notated-values notated-body)
						  	)
								(let ([notated-values (map-notate values (cons variables depths))]
						  				[notated-body (map-notate body (cons variables depths))])
						  		(let-exp let-type name variables notated-values notated-body)
						  	)
							)
					  ]
					  [if-then-exp (conditional true-exp)
					  	(if-then-exp (notate conditional depths) (notate true-exp depths))
					  ]
					  [if-else-exp (conditional true-exp false-exp)
					  	(if-else-exp (notate conditional depths)
					  							 (notate true-exp depths)
					  							 (notate false-exp depths))
					  ]
					  [set!-exp (variable value)
							(cases lex-address variable
								[undefined (sym)
									(set!-exp (lex-note sym depths 0) (notate value depths))
								]
								[else
									(errorf 'lexical-address "[ ERROR ]: Malformed set!-exp ~% --- variable of set!-exp should be undefined before lexical-address conversion: ~s" variable)
								]
							)
					  ]
					  [void-exp ()
					  	exp
					  ]
					  [define-exp (identifier value)
					    (define-exp identifier (notate value depths))
					  ]
					  [else
					  	(errorf 'lexical-address "[ ERROR ]: unexpected expression in lexical-address ~% --- lexical address non-core syntax expression: ~s" exp)
					  ]
					)
				)
			]
			[map-notate
				(lambda (exp-list depths)
					(map (lambda (exp) (notate exp depths)) exp-list)
				)
			])
			(notate exp '())
		)
	)
)
