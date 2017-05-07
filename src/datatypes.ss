;; Parsed expression datatypes

(define clause? ; ((objs ...) e1 e2 ...)
  (lambda (x)
    (let ([length (length x)])
      (cond
        [(< length 2) #f]
        [(not (list? (car x))) #f]
        [(not (andmap expression? (cdr x))) #f]
        [else #t]
      )
    )
  )
)

(define-datatype clause clause?
  [cond-clause
    (conditonal expression?)
    (body (list-of expression?))
  ]
  [case-clause
    (keys list?)
    (body (list-of expression?))
  ]
  [else-clause
    (body (list-of expression?))
  ]
)

(define cond-clause?
  (lambda (c)
    (cases clause c
      [cond-clause (conditional body)
        #t
      ]
      [else-clause (body)
        #t
      ]
      [else
        #f
      ]
    )
  )
)

(define case-clause?
  (lambda (c)
    (cases clause c
      [case-clause (conditional body)
        #t
      ]
      [else-clause (body)
        #t
      ]
      [else
        #f
      ]
    )
  )
)

; Expression datatype.
(define-datatype expression expression?
  [var-exp (variable symbol?)]
  [lit-exp (literal (lambda (x) #t))]
  [app-exp
    (operator expression?)
    (arguments (list-of expression?))
  ]
  [lambda-exact-exp ; used to handle (lambda (x y z) body) [exact argument lambdas]
    (variables (list-of parameter?))
    (body (list-of expression?))
  ]
  [lambda-exp ; used to handle (lambda (x . y) body) or (lambda x body) [required + optional/many argument lambda]
    (required (list-of parameter?)) ; It may be an empty list. If so, it is (lambda x body)
    (optional parameter?)
    (body (list-of expression?))
  ]
  [let-exp ; for let, let*, letrec
    (let-type symbol?) ; lets are handled the same, might as well keep track of the call type.
    (name (lambda (x) (if (boolean? x) (not x) x)))
    (variables (list-of symbol?))
    (values (list-of expression?))
    (body (list-of expression?))
  ]
  [if-then-exp ; one-armed if
    (conditional expression?)
    (true-exp expression?)
  ]
  [if-else-exp ; two-armed if
    (conditional expression?)
    (true-exp expression?)
    (false-exp expression?)
  ]
  [set!-exp
    (variable symbol?)
    (value expression?)
  ]
  [begin-exp
    (body (list-of expression?))
  ]
  [void-exp]
  [cond-exp
    (clauses (list-of cond-clause?))
  ]
  [and-exp
    (conditionals (list-of expression?))
  ]
  [or-exp
    (conditionals (list-of expression?))
  ]
  [case-exp
    (key list?)
    (clauses (list-of case-clause?))
  ]
  [while-exp
    (test expression?)
    (body (list-of expression?))
  ]
  [define-exp
    (identifier symbol?)
    (value expression?)
  ]
  [for-exp
    (initializers (list-of expression?)) ; may be empty
    (test expression?) ; must exist
    (update (list-of expression?)) ; may be empty
    (body (list-of expression?))
  ]
)

;; environment type definitions
(define scheme-value?
  (lambda (x) #t)
)

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
    (syms (list-of symbol?))
    (vals (list-of scheme-value?))
    (env environment?)
  ]
)

(define pair-of-symbols?
  (lambda (x)
    (cond
      [(symbol? x) #t]
      [(null? x) #t]
      [(equal? '() (car x)) #t]
      [(symbol? (car x)) (pair-of-symbols? (cdr x))]
      [else #f]
    )
  )
)

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)
  ]
  [closure
    (variables (list-of parameter?))
    (bodies (list-of expression?))
    (env environment?)
  ]
)

(define dereference-parameter
  (lambda (param)
    (cases parameter param
      [explicit-parameter (sym)
        sym
      ]
      [implicit-parameter (sym)
        sym
      ]
      [else
        (errorf 'dereference-parameter "[ ERROR ]: Unknown parameter type ~% --- the given parameter is not defined: ~s" param)
      ]
    )
  )
)

(define-datatype parameter parameter?
  [explicit-parameter ; (lambda (x y z) ...) and x y z are explicit
    (sym symbol?)
  ]
  [implicit-parameter ; (lambda x ...) or (lambda (y . x) ...) and x is implicit
    (sym symbol?)
  ]
)
