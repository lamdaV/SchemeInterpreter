
;; Parsed expression datatypes

; Expression datatype.
(define-datatype expression expression?
  [var-exp (variable symbol?)]
  [lit-exp (literal (lambda (x) #t))]
  [app-exp
    (operator expression?)
    (arguments (list-of expression?))
  ]
  [lambda-exact-exp ; used to handle (lambda (x y z) body) [exact argument lambdas]
    (variables (list-of symbol?))
    (body (list-of expression?))
  ]
  [lambda-exp ; used to handle (lambda (x . y) body) or (lambda x body) [required + optional/many argument lambda]
    (required (list-of symbol?))
    (optional symbol?)
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
)

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)
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
