; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Returns true if the given list ls is an improper list.
(define improper?
  (lambda (ls)
    (and (pair? ls)
         (not (list? (cdr ls))))
  )
)

; Returns if a given value is a literal in scheme.
(define literal?
  (lambda (x)
    (ormap (lambda (proc) (proc x))
           (list null? number? vector? symbol? boolean? string?))
  )
)

; Parser code.
(define parse-exp
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(literal? datum) (lit-exp datum)]
      [(pair? datum)
        (cond
          [(eqv? (car datum) 'quote) (lit-exp (cadr datum))]
          [(eqv? (car datum) 'lambda) ; (lambda (variables) body)
            (if (> 3 (length datum))
              (eopl:error 'parse-exp "[ ERROR ]: malformed lambda expression ~% --- lambda requires an identifier, variable (x, (x), or (x . y)), and body ~s" datum)
              (let ([variables (cadr datum)]
                    [body (map parse-exp (cddr datum))])
                (cond
                  [(symbol? variables) (lambda-exp '() variables body)] ; optional/many argument lambda
                  [(improper? variables) ; required + optional/many argument lambda
                    (letrec (
                      [parse-improper-list ; (a b . c) --> ((a b) (c)) where the car is the "proper list" and the cadr is the "improper list"
                        (lambda (ls required optional)
                          (if (symbol? ls)
                            (cons (reverse required) (list (cons ls optional)))
                            (let* ([top (car ls)]
                                   [next (cdr ls)]
                                   [new-required (cons top required)])
                              (parse-improper-list next new-required optional)
                            )
                          )
                        )
                      ])
                      (let* ([parse-variables (parse-improper-list variables '() '())]
                             [required (car parse-variables)]
                             [optional (caadr parse-variables)])
                        (lambda-exp required optional body)
                      )
                    )
                  ]
                  [(and (list? variables) (andmap symbol? variables)) ; exact argument lambda
                    (let ([concrete-variables (map parse-exp variables)])
                      (lambda-exact-exp variables body)
                    )
                  ]
                  [(not (list? variables))
                    (eopl:error 'parse-exp "[ ERROR ]: malformed lambda variables ~% --- variables must either be a symbol or a list of symbols: ~s in ~s" variables datum)
                  ]
                  [(not (andmap symbol? variables))
                    (eopl:error 'parse-exp "[ ERROR ]: malformed lambda variables ~% ---variables must be symbols ~s in ~s" variables datum)
                  ]
                  [else
                    (eopl:error 'parse-exp "[ ERROR ]: malformed lambda variables ~% --- cause unknown: ~s in ~s" variables datum)
                  ]
                )
              )
            )
          ]
          [(or (eqv? (car datum) 'let)
               (eqv? (car datum) 'let*)
               (eqv? (car datum) 'letrec)) ; (let (assignment) body)
            (let ([letLength (length datum)]
                  [parse-let (lambda (assignment-pair) ; let-helper
                                (cond
                                  [(null? assignment-pair)
                                    (eopl:error 'parse-exp "[ ERROR ]: malformed let assignment. ~% --- a variable assignment cannot be empty: ~s" assignment-pair)
                                  ]
                                  [(improper? assignment-pair)
                                    (eopl:error 'parse-exp "[ ERROR ]: malformed let assignment. ~% --- a variable assignment cannot be an improper list: ~s" assignment-pair)
                                  ]
                                  [(not (equal? 2 (length assignment-pair)))
                                    (eopl:error 'parse-exp "[ ERROR ]: malformed let assignment ~% --- all variable assignment must of length 2: ~s" assignment-pair)
                                  ]
                                  [else
                                    (let ([variable (car assignment-pair)]
                                          [value (cadr assignment-pair)])
                                      (if (not (symbol? variable))
                                        (eopl:error 'parse-exp "[ ERROR ]: malformed let variable assignment ~% --- all variables being assigned must be a symbol: ~s from ~s" variable assignment-pair)
                                        (list variable (parse-exp value))
                                      )
                                    )
                                  ]
                                )
                             )
                  ])
              (cond
                [(> 3 letLength)
                  (eopl:error 'parse-exp "[ ERROR ]: malformed let expression ~% --- let requires an identifier, variable assignment, and body: ~s" datum)
                ]
                [(symbol? (cadr datum)) ; named length
                  (let ([name (cadr datum)]
                        [variable-assignment (caddr datum)]
                        [body (cdddr datum)])
                    (cond
                      [(null? body)
                        (eopl:error 'parse-exp "[ ERROR ]: malformed NAMED let expression ~% --- body cannot be empty: ~s in ~s" body)
                      ]
                      [(not (list? variable-assignment))
                        (eopl:error 'parse-exp "[ ERROR ]: malformed NAMED let variable assignment ~% --- variable assignment must be a list of list: ~s" variable-assignment)
                      ]
                      [(improper? variable-assignment)
                        (eopl:error 'parse-exp "[ ERROR ]: malformed NAMED let variable assignment ~% --- variable assignment cannot be an improper list: ~s" variable-assignment)
                      ]
                      [else
                        (let* ([variable-value (map parse-let variable-assignment)]
                               [variable (map car variable-value)]
                               [value (map cadr variable-value)]
                               [evaluated-body (map parse-exp body)]
                               [let-type (car datum)])
                          (let-exp let-type name variable value evaluated-body)
                        )
                      ]
                    )
                  )
                ]
                [else ; regular lets
                  (let ([variable-assignment (cadr datum)]
                        [body (cddr datum)])
                    (cond
                      [(null? body)
                        (eopl:error 'parse-exp "[ ERROR ]: malformed let expression ~% --- body cannot be empty: ~s" body)
                      ]
                      [(not (list? variable-assignment))
                        (eopl:error 'parse-exp "[ ERROR ]: malformed let variable assignment ~% --- variable assignment must be a list of list: ~s" variable-assignment)
                      ]
                      [(improper? variable-assignment)
                        (eopl:error 'parse-exp "[ ERROR ]: malformed let variable assignment ~% --- variable assignment cannot be an improper list: ~s" variable-assignment)
                      ]
                      [else
                        (let* ([variable-value (map parse-let variable-assignment)]
                               [variable (map car variable-value)]
                               [value (map cadr variable-value)]
                               [evaluated-body (map parse-exp body)]
                               [let-type (car datum)])
                          (let-exp let-type #f variable value evaluated-body)
                        )
                      ]
                    )
                  )
                ]
              )
            ) ; end of let body
          ]
          [(eqv? (car datum) 'if) ; (if (conditional) (true-exp) (false-exp))
            (let ([ifLength (length datum)])
              (cond
                [(equal? ifLength 3)
                  (let ([conditional (parse-exp (cadr datum))]
                        [true-exp (parse-exp (caddr datum))])
                    (if-then-exp conditional true-exp)
                  )
                ]
                [(equal? ifLength 4)
                  (let ([conditional (parse-exp (cadr datum))]
                        [true-exp (parse-exp (caddr datum))]
                        [false-exp (parse-exp (cadddr datum))])
                    (if-else-exp conditional true-exp false-exp)
                  )
                ]
                [else
                  (eopl:error 'parse-exp "[ ERROR ]: malformed if statement ~% --- if statements must include an identifier, conditional, true-expression, and/or false-expression: ~s" datum)
                ]
              )
            )
          ]
          [(eqv? (car datum) 'set!) ; (set! (variable) (value))
            (cond
              [(not (equal? 3 (length datum)))
                (eopl:error 'parse-exp "[ ERROR ]: malformed set! ~% --- set! statements must include an identifier, variable symbol, and value: ~s" datum)
              ]
              [(not (symbol? (caddr datum)))
                (eopl:error 'parse-exp "[ ERROR ]: malformed set! ~% --- set! variable must be a symbol: ~s in ~s" (caddr datum) datum)
              ]
              [else
                  (let ([variable (cadr datum)]
                        [value (caddr datum)])
                    (set!-exp variable (parse-exp value))
                  )
              ]
            )
          ]
          [else ; (app-exp ...)
            (if (improper? datum)
              (eopl:error 'parse-exp "[ ERROR ]: malformed app-exp ~% --- improper list ~s" datum)
              (let ([operator (car datum)]
                    [arguments (cdr datum)])
                (cond
                  [(number? operator)
                    (eopl:error 'parse-exp "[ ERROR ]: malformed app-exp operator ~% --- operator cannot be a number: ~s in ~s" operator datum)
                  ]
                  [(boolean? operator)
                    (eopl:error 'parse-exp "[ ERROR ]: malformed app-exp operator ~% --- operator cannot be a boolean: ~s in ~s" operator datum)
                  ]
                  [(vector? operator)
                    (eopl:error 'parse-exp "[ ERROR ]: malformed app-exp operator ~% --- operator cannot be a vector: ~s in ~s" operator datum)
                  ]
                  [else
                    (app-exp (parse-exp operator) (map parse-exp arguments))
                  ]
                )
              )
            )
          ]
        )
      ]
      [else (eopl:error 'parse-exp "[ ERROR ]: malformed expression ~% --- this expression does not match any expected expression: ~s" datum)]
    )
  )
)

; Unparser
(define unparse-exp
  (lambda (datum)
    (cases expression datum
      [var-exp (variable)
        variable
      ]
      [lit-exp (literal)
        literal
      ]
      [lambda-exp (required optional body)
        (let ([decode-body (map unparse-exp body)])
          (if (null? required) ; If the required arguments is an empty list, this is (lambda x ...) and not (lambda (x . y) ...)
            (cons* 'lambda optional decode-body)
            (cons* 'lambda (append required optional) decode-body)
          )
        )
      ]
      [lambda-exact-exp (variables body)
        (let ([decode-body (map unparse-exp body)])
          (cons* 'lambda variables decode-body)
        )
      ]
      [app-exp (operator arguments)
        (let ([decode-operator (unparse-exp operator)]
              [decode-arguments (map unparse-exp arguments)])
          (cons decode-operator decode-arguments)
        )
      ]
      [let-exp (let-type name variable value body)
        (let ([decode-value (map unparse-exp value)]
              [decode-body (map unparse-exp body)])
          (if name
            (cons* let-type name (map list variable decode-value) decode-body)
            (cons* let-type (map list variable decode-value) decode-body)
          )
        )
      ]
      [if-then-exp (conditional true-exp)
        (let ([decode-conditional (unparse-exp conditional)]
              [decode-true-exp (unparse-exp conditional)])
          (list 'if decode-conditional decode-true-exp)
        )
      ]
      [if-else-exp (conditional true-exp false-exp)
        (let ([decode-conditional (unparse-exp conditional)]
              [decode-true-exp (unparse-exp true-exp)]
              [decode-false-exp (unparse-exp false-exp)])
          (list 'if decode-conditional decode-true-exp decode-false-exp)
        )
      ]
      [set!-exp (variable value)
        (let ([decode-value (unparse-exp value)])
          (cons* 'set! variable value)
        )
      ]
    )
  )
)
