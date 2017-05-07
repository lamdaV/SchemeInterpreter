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
          [(eqv? (car datum) 'void)
            (if (equal? 1 (length datum))
              (void-exp)
              (errorf 'parse-exp "[ ERROR ]: malformed void exppression ~% --- void expression takes no arguments ~s" datum)
            )
          ]
          [(eqv? (car datum) 'quote) (lit-exp (cadr datum))]
          [(eqv? (car datum) 'lambda) ; (lambda (variables) body)
            (if (> 3 (length datum))
              (errorf 'parse-exp "[ ERROR ]: malformed lambda expression ~% --- lambda requires an identifier, variable (x, (x), or (x . y)), and body ~s" datum)
              (let ([variables (cadr datum)]
                    [body (map parse-exp (cddr datum))])
                (cond
                  [(symbol? variables) (lambda-exp '() (implicit-parameter variables) body)] ; optional/many argument lambda
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
                        (lambda-exp (map explicit-parameter required) (implicit-parameter optional) body)
                      )
                    )
                  ]
                  [(and (list? variables) (andmap symbol? variables)) ; exact argument lambda
                    (lambda-exact-exp (map explicit-parameter variables) body)
                  ]
                  [(not (list? variables))
                    (errorf 'parse-exp "[ ERROR ]: malformed lambda variables ~% --- variables must either be a symbol or a list of symbols: ~s in ~s" variables datum)
                  ]
                  [(not (andmap symbol? variables))
                    (errorf 'parse-exp "[ ERROR ]: malformed lambda variables ~% ---variables must be symbols ~s in ~s" variables datum)
                  ]
                  [else
                    (errorf 'parse-exp "[ ERROR ]: malformed lambda variables ~% --- cause unknown: ~s in ~s" variables datum)
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
                                    (errorf 'parse-exp "[ ERROR ]: malformed let assignment. ~% --- a variable assignment cannot be empty: ~s" assignment-pair)
                                  ]
                                  [(improper? assignment-pair)
                                    (errorf 'parse-exp "[ ERROR ]: malformed let assignment. ~% --- a variable assignment cannot be an improper list: ~s" assignment-pair)
                                  ]
                                  [(not (equal? 2 (length assignment-pair)))
                                    (errorf 'parse-exp "[ ERROR ]: malformed let assignment ~% --- all variable assignment must of length 2: ~s" assignment-pair)
                                  ]
                                  [else
                                    (let ([variable (car assignment-pair)]
                                          [value (cadr assignment-pair)])
                                      (if (not (symbol? variable))
                                        (errorf 'parse-exp "[ ERROR ]: malformed let variable assignment ~% --- all variables being assigned must be a symbol: ~s from ~s" variable assignment-pair)
                                        (list variable (parse-exp value))
                                      )
                                    )
                                  ]
                                )
                             )
                  ])
              (cond
                [(> 3 letLength)
                  (errorf 'parse-exp "[ ERROR ]: malformed let expression ~% --- let requires an identifier, variable assignment, and body: ~s" datum)
                ]
                [(symbol? (cadr datum)) ; named length
                  (let ([name (cadr datum)]
                        [variable-assignment (caddr datum)]
                        [body (cdddr datum)])
                    (cond
                      [(null? body)
                        (errorf 'parse-exp "[ ERROR ]: malformed NAMED let expression ~% --- body cannot be empty: ~s in ~s" body)
                      ]
                      [(not (list? variable-assignment))
                        (errorf 'parse-exp "[ ERROR ]: malformed NAMED let variable assignment ~% --- variable assignment must be a list of list: ~s" variable-assignment)
                      ]
                      [(improper? variable-assignment)
                        (errorf 'parse-exp "[ ERROR ]: malformed NAMED let variable assignment ~% --- variable assignment cannot be an improper list: ~s" variable-assignment)
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
                        (errorf 'parse-exp "[ ERROR ]: malformed let expression ~% --- body cannot be empty: ~s" body)
                      ]
                      [(not (list? variable-assignment))
                        (errorf 'parse-exp "[ ERROR ]: malformed let variable assignment ~% --- variable assignment must be a list of list: ~s" variable-assignment)
                      ]
                      [(improper? variable-assignment)
                        (errorf 'parse-exp "[ ERROR ]: malformed let variable assignment ~% --- variable assignment cannot be an improper list: ~s" variable-assignment)
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
                  (errorf 'parse-exp "[ ERROR ]: malformed if statement ~% --- if statements must include an identifier, conditional, true-expression, and/or false-expression: ~s" datum)
                ]
              )
            )
          ]
          [(eqv? (car datum) 'set!) ; (set! (variable) (value))
            (cond
              [(not (equal? 3 (length datum)))
                (errorf 'parse-exp "[ ERROR ]: malformed set! ~% --- set! statements must include an identifier, variable symbol, and value: ~s" datum)
              ]
              ;[(not (symbol? (caddr datum)))
              ;  (errorf 'parse-exp "[ ERROR ]: malformed set! ~% --- set! variable must be a symbol: ~s in ~s" (caddr datum) datum)
              ;]
              [else
                  (let ([variable (cadr datum)]
                        [value (caddr datum)])
                    (set!-exp variable (parse-exp value))
                  )
              ]
            )
          ]
          [(eqv? (car datum) 'begin)
            (let ([body (map parse-exp (cdr datum))])
              (begin-exp body)
            )
          ]
          [(eqv? (car datum) 'cond)
            (cond
              [(not (> (length datum) 1))
                (errorf 'parse-exp "[ ERROR ]: malformed cond ~% --- cond statements must include an identifier, and one or more conditions and expressions ~s" datum)
              ]
              [(not (andmap list? (cdr datum)))
                (errorf 'parse-exp "[ ERROR ]: malformed cond! ~% --- all conditional expressions must be a list ~s" datum)
              ]
              [else
                (letrec ([parse-cond (lambda (x)
                                       (let ([identifier (car x)]
                                             [body (map parse-exp (cdr x))])
                                         (if (equal? identifier 'else)
                                            (else-clause body)
                                            (cond-clause (parse-exp identifier) body)
                                         )
                                       )
                                     )]
                         [conditional-expressions (cdr datum)])
                  (cond-exp (map parse-cond conditional-expressions))
                )
              ]
            )
          ]
          [(eqv? (car datum) 'and)
            (and-exp (map parse-exp (cdr datum)))
          ]
          [(eqv? (car datum) 'or)
            (or-exp (map parse-exp (cdr datum)))
          ]
          [(eqv? (car datum) 'case)
            (cond
              [(< (length datum) 3)
                (errorf 'parse-exp "[ ERROR ]: malformed case-exp ~% --- case expressions have an identifier, key, and one or more clauses: ~s" datum)
              ]
              [else
                (let ([key (parse-exp (cadr datum))]
                      [clauses (map (lambda (x)
                                      (let ([identifier (car x)]
                                            [body (map parse-exp (cdr x))])
                                        (if (equal? 'else identifier)
                                          (else-clause body)
                                          (case-clause identifier body)
                                        )
                                      )
                                    )
                                    (cddr datum))])
                  (case-exp key clauses)
                )
              ]
            )
          ]
          [(eqv? (car datum) 'while)
            (cond
              [(< (length datum) 3)
                (errorf 'parse-exp "[ ERROR ]: malformed while-exp ~% --- while expressions have an identifier, test, and one or many bodies: ~s" datum)
              ]
              [else
                (let ([test (parse-exp (cadr datum))]
                      [body (map parse-exp (cddr datum))])
                  (while-exp test body)
                )
              ]
            )
          ]
          [(eqv? (car datum) 'define)
            (cond
              [(not (equal? (length datum) 3))
                (errorf 'parse-exp "[ ERROR ]: malformed define-exp ~% --- define expressions have an identifier and value: ~s" datum)
              ]
              [(not (symbol? (2nd datum)))
                (errorf 'parse-exp "[ ERROR ]: malformed define-exp ~% --- first argument should be a symbol: ~s in ~s" (2nd datum) datum)
              ]
              [else
                (define-exp (2nd datum) (parse-exp (3rd datum)))
              ]
            )
          ]
          [(eqv? (car datum) 'for) ; (for ((init ...) : test : update ...) body ...)
            (letrec ([colon-count (lambda (inits count)
                                    (cond
                                      [(null? inits) count]
                                      [(equal? (car inits) ':) (colon-count (cdr inits) (add1 count))]
                                      [else (colon-count (cdr inits) count)]
                                    ))])
              (cond
                [(< (length datum) 2) ; most basic case (for (...))
                  (errorf 'parse-exp "[ ERROR ]: malformed for-exp ~% --- for expressions have an identifier, list of initializers, zero or more bodies: ~s" datum)
                ]
                [(not (list? (cadr datum))) ; (init ...)
                  (errorf 'parse-exp "[ ERROR ]: malformed for-exp ~% --- for expressions initializers must be a list: ~s in ~s" (caadr datum) datum)
                ]
                [(not (equal? (colon-count (cadr datum) 0) 2))
                  (errorf 'parse-exp "[ ERROR ]: malformed for-exp ~% --- for expressions initializers must contain exactly two colons ((init ...) : test : update ...): ~s" (cadr datum))
                ]
                [(not (and (equal? ': (cadr (cadr datum))) (equal? ': (cadddr (cadr datum)))))
                  (errorf 'parse-exp "[ ERROR ]: malformed for-exp ~% --- for expressions must have exactly one test ((init ...) : test : update ...): ~s" datum)
                ]
                [else
                  (let* ([checks (cadr datum)] ; ((init ...) : test : update ...)
                         [initializers (map parse-exp (car checks))] ; mapping to this (i1 i2 i3 ...)
                         [test (parse-exp (caddr checks))]
                         [updates (map parse-exp (cddddr checks))]
                         [body (map parse-exp (cddr datum))])
                    (for-exp initializers test updates body)
                  )
                ]
              )
            )
          ]
          [else ; (app-exp ...)
            (if (improper? datum)
              (errorf 'parse-exp "[ ERROR ]: malformed app-exp ~% --- improper list ~s" datum)
              (let ([operator (car datum)]
                    [arguments (cdr datum)])
                (cond
                  [(number? operator)
                    (errorf 'parse-exp "[ ERROR ]: malformed app-exp operator ~% --- operator cannot be a number: ~s in ~s" operator datum)
                  ]
                  [(boolean? operator)
                    (errorf 'parse-exp "[ ERROR ]: malformed app-exp operator ~% --- operator cannot be a boolean: ~s in ~s" operator datum)
                  ]
                  [(vector? operator)
                    (errorf 'parse-exp "[ ERROR ]: malformed app-exp operator ~% --- operator cannot be a vector: ~s in ~s" operator datum)
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
      [else (errorf 'parse-exp "[ ERROR ]: malformed expression ~% --- this expression does not match any expected expression: ~s" datum)]
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
      [cond-exp (clauses)
        (letrec ([unparse-clause (lambda (x)
                                   (cases clause x
                                     [cond-clause (condition body)
                                       (cons (unparse-exp condition)
                                             (map unparse-exp body))
                                     ]
                                     [else-clause (body)
                                       (cons 'else (map unparse-exp body))
                                     ]
                                     [else (errorf 'unparse-clause "[ ERROR ]: Malformed cond-exp ~% --- cond-exp should contain only cond-clause or else-clause: %s" x)]
                                   )
                                 )])
          (cons 'cond (map unparse-clause clauses))
        )
      ]
      [case-exp (key clauses)
        (letrec ([unparse-clause (lambda (x)
                                   (cases clause x
                                     [case-clause (keys body)
                                       (cons keys
                                             (map unparse-exp body))
                                     ]
                                     [else-clause (body)
                                       (cons 'else (map unparse-exp body))
                                     ]
                                     [else (errorf 'unparse-clause "[ ERROR ]: Malformed cond-exp ~% --- cond-exp should contain only cond-clause or else-clause: %s" x)]
                                   )
                                 )])
          (cons* 'case (unparse-exp key) (map unparse-clause clauses))
        )
      ]
      [lambda-exp (required optional body)
        (letrec ([parse-parameter (lambda (params)
                                    (let parse ([params params])
                                      (if (null? params)
                                        '()
                                        (cases parameter (car params)
                                          [explicit-parameter (sym)
                                            (cons sym (parse (cdr params)))
                                          ]
                                          [implicit-parameter (sym)
                                            sym
                                          ]
                                        )
                                      )
                                    )
                                  )]
                 [decode-body (map unparse-exp body)])
          (cons* 'lambda (parse-parameter (append required (list optional))) decode-body)
        )
      ]
      [lambda-exact-exp (variables body)
        (let ([decode-body (map unparse-exp body)])
          (cons* 'lambda (map dereference-parameter variables) decode-body)
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
              [decode-true-exp (unparse-exp true-exp)])
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
      [begin-exp (body)
        (let ([decode-value (map unparse-exp body)])
          (cons 'begin decode-value)
        )
      ]
      [void-exp ()
        (list 'void)
      ]
      [and-exp (conditionals)
        (cons 'and conditionals)
      ]
      [or-exp (conditionals)
        (cons 'or conditionals)
      ]
      [while-exp (test body)
        (let ([decode-test (unparse-exp test)]
              [decode-body (map unparse-exp body)])
          (cons* 'while decode-test decode-body)
        )
      ]
      [define-exp (identifier value)
        (let ([decode-value (unparse-exp value)])
          (list 'define identifier decode-value)
        )
      ]
      [for-exp (initializers test update body)
        (let ([decode-inits (map unparse-exp initializers)]
              [decode-test (unparse-exp test)]
              [decode-update (map unparse-exp update)]
              [decode-body (map unparse-exp body)])
          (cons* 'for (cons* decode-inits ': decode-test ': decode-update) decode-body)
        )
      ]
    )
  )
)
