(define apply-k
  (lambda (k v)
    (cases exp-k k
      [init-k () v]

      ; one-armed-if
      [branch-one-k (true-exp env c)
        (let ([conditional-value v])
          (if conditional-value
            (eval-exp true-exp env c)
          )
        )
      ]

      ; two-armed-if
      [branch-two-k (true-exp false-exp env c)
        (let ([conditional-value v])
          (if conditional-value
            (eval-exp true-exp env c)
            (eval-exp false-exp env c)
          )
        )
      ]

      ; app-exp
      [operator-k (arguments env c)
        (let ([operator v])
          (eval-rands arguments env (app-k operator c))
        )
      ]
      [app-k (operator c)
        (let ([arguments v])
          (apply-proc operator arguments c) ; TODO: applyproc needs a continuation.
        )
      ]

      ; set!
      [set-k (variable env c)
        (let ([value v])
          (apply-k c (mutate-env variable value env))
        )
      ]

      ; define
      [define-k (identifier c)
        (let ([value v])
          (apply-k c (mutate-global-env! identifier value))
        )
      ]

      ; map-cps
      [proc-k (proc-cps car-L c)
        (let ([ls v])
          (proc-cps car-L (cons-k ls c))
        )
      ]
      [cons-k (ls c)
        (let ([changed-car v])
          (apply-k c (cons changed-car ls))
        )
      ]

      ; eval-bodies
      [eval-body-k (cdr-bodies env c)
        (eval-bodies cdr-bodies env c)
      ]

      [apply-global-k (fail-sym variable c)
        (apply-env global-env
                   variable ; look up its value.
                   c ; procedure to call if variable is in the environment
                   (error-k fail-symbol variable "[ ERROR ]: Unable to find variable in environment ~% --- variable not found in environment: ~s" fail-sym))
      ]
      [error-k (fail-sym variable message)
        (errorf fail-sym message variable)
      ]

      ;letrec
      [for-each-k (variables values env body c)
        (let
          ([evaluated-value v]
           [mutating-variable (car variables)]
           [values-left (cdr values)]
           [variables-left (cdr variables)])
          (if (null? (cdr values))
            (begin
              (mutate-env mutating-variable evaluated-value env)
              (eval-bodies body env c)
            )
            (begin
              (mutate-env mutating-variable evaluated-value env)
              (eval-exp (car cdr-values) env (for-each-k variables-left values-left env body c))
            )
          )
        )
      ]

      ; let
      [eval-rands-k (body variables env c)
        (let* ([evaluated-args v]
               [extended-env (extend-env variables evaluated-args env)])
          (eval-bodies body extended-env c)
        )
      ]
    )
  )
)

(define apply-continuation
  (lambda (k . v)
    (apply k v)
  )
)

; Usually an interpreter must define each
; built-in procedure individually.  We are "cheating" a little bit.
(define apply-prim-proc
  (lambda (prim-proc args k)
    (let ([arg-length (length args)])
      (case prim-proc
        [(+)
          (if (andmap number? args)
            (apply + args)
            (errorf 'apply-prim-proc "[ ERROR ]: Malformed + argument ~% --- + expects arguments a list of numbers: ~s" args)
          )
        ]
        [(-)
          (if (andmap number? args)
            (apply - args)
            (errorf 'apply-prim-proc "[ ERROR ]: Malformed - argument ~% --- - expects arguments a list of numbers: ~s" args)
          )
        ]
        [(*)
          (if (andmap number? args)
            (apply * args)
            (errorf 'apply-prim-proc "[ ERROR ]: Malformed * argument ~% --- * expects arguments a list of numbers: ~s" args)
          )
        ]
        [(/)
          (if (andmap number? args)
            (apply / args)
            (errorf 'apply-prim-proc "[ ERROR ]: Malformed / argument ~% --- / expects arguments a list of numbers: ~s" args)
          )
        ]
        [(quotient)
          (cond
            [(not (and (equal? arg-length 2) (andmap integer? args)))
              (errorf 'apply-prim-proc "[ ERROR ]: Malformed quotient argument ~% --- quotient expects exactly two integer arguments: ~s" args)
            ]
            [else
              (apply quotient args)
            ]
          )
        ]
        [(<)
          (if (and (not (null? args)) (andmap number? args))
            (apply < args)
            (errorf 'apply-prim-proc "[ ERROR ]: Malformed < argument ~% --- < expects arguments a list of numbers: ~s" args)
          )
        ]
        [(>)
          (if (and (not (null? args)) (andmap number? args))
            (apply > args)
            (errorf 'apply-prim-proc "[ ERROR ]: Malformed > argument ~% --- > expects arguments a list of numbers: ~s" args)
          )
        ]
        [(>=)
          (if (and (not (null? args)) (andmap number? args))
            (apply >= args)
            (errorf 'apply-prim-proc "[ ERROR ]: Malformed >= argument ~% --- >= expects arguments a list of numbers: ~s" args)
          )
        ]
        [(<=)
          (if (and (not (null? args)) (andmap number? args))
            (apply <= args)
            (errorf 'apply-prim-proc "[ ERROR ]: Malformed <= argument ~% --- <= expects arguments a list of numbers: ~s" args)
          )
        ]
        [(not)
          (if (equal? 1 arg-length)
            (not (1st args))
            (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- not expects one argument: ~s in ~s" arg-length args)
          )
        ]
        [(zero?)
          (if (equal? 1 arg-length)
            (zero? (1st args))
            (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- zero? expects one argument: ~s in ~s" arg-length args)
          )
        ]
        [(add1)
          (if (equal? 1 arg-length)
            (add1 (1st args))
            (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- add1 expects one argument: ~s in ~s" arg-length args)
          )
        ]
        [(sub1)
          (if (equal? 1 arg-length)
            (sub1 (1st args))
            (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- sub1 expects one argument: ~s in ~s" arg-length args)
          )
        ]
        [(cons)
          (if (equal? 2 arg-length)
            (cons (1st args) (2nd args))
            (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- cons expects two arguments: ~s in ~s" arg-length args)
          )
        ]
        [(cdr)
          (cond
            [(not (equal? 1 arg-length)) (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- cdr expects one argument: ~s in ~s" arg-length args)]
            [(not (pair? (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed cdr arguments ~% --- cdr expects a list argument: ~s in ~s" (1st args) args)]
            [else (cdr (1st args))]
          )
        ]
        [(car)
          (cond
            [(not (equal? 1 arg-length)) (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- car expects one argument: ~s in ~s" arg-length args)]
            [(not (list? (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed car arguments ~% --- car expects a list argument: ~s in ~s" (1st args) args)]
            [else (car (1st args))]
          )
        ]
        [(cadr)
          (cond
            [(not (equal? 1 arg-length)) (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- cadr expects one argument: ~s in ~s" arg-length args)]
            [(not (list? (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed cadr arguments ~% --- cadr expects a list argument: ~s in ~s" (1st args) args)]
            [else (cadr (1st args))]
          )
        ]
        [(cddr)
          (cond
            [(not (equal? 1 arg-length)) (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- cddr expects one argument: ~s in ~s" arg-length args)]
            [(not (list? (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed cddr arguments ~% --- cddr expects a list argument: ~s in ~s" (1st args) args)]
            [else (cddr (1st args))]
          )
        ]
        [(cdar)
          (cond
            [(not (equal? 1 arg-length)) (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- cdar expects one argument: ~s in ~s" arg-length args)]
            [(not (list? (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed cdar arguments ~% --- cdar expects a list argument: ~s in ~s" (1st args) args)]
            [else (cdar (1st args))]
          )
        ]
        [(caar)
          (cond
            [(not (equal? 1 arg-length)) (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- caar expects one argument: ~s in ~s" arg-length args)]
            [(not (list? (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed caar arguments ~% --- caar expects a list argument: ~s in ~s" (1st args) args)]
            [else (caar (1st args))]
          )
        ]
        [(cdddr)
          (cond
            [(not (equal? 1 arg-length)) (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- cdddr expects one argument: ~s in ~s" arg-length args)]
            [(not (list? (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed cdddr arguments ~% --- cdddr expects a list argument: ~s in ~s" (1st args) args)]
            [else (cdddr (1st args))]
          )
        ]
        [(caddr)
          (cond
            [(not (equal? 1 arg-length)) (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- caddr expects one argument: ~s in ~s" arg-length args)]
            [(not (list? (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed caddr arguments ~% --- caddr expects a list argument: ~s in ~s" (1st args) args)]
            [else (caddr (1st args))]
          )
        ]
        [(cdadr)
          (cond
            [(not (equal? 1 arg-length)) (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- cdadr expects one argument: ~s in ~s" arg-length args)]
            [(not (list? (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed cdadr arguments ~% --- cdadr expects a list argument: ~s in ~s" (1st args) args)]
            [else (cdadr (1st args))]
          )
        ]
        [(cddar)
          (cond
            [(not (equal? 1 arg-length)) (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- cddar expects one argument: ~s in ~s" arg-length args)]
            [(not (list? (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed cddar arguments ~% --- cddar expects a list argument: ~s in ~s" (1st args) args)]
            [else (cddar (1st args))]
          )
        ]
        [(cdaar)
          (cond
            [(not (equal? 1 arg-length)) (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- cdaar expects one argument: ~s in ~s" arg-length args)]
            [(not (list? (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed cdaar arguments ~% --- cdaar expects a list argument: ~s in ~s" (1st args) args)]
            [else (cdaar (1st args))]
          )
        ]
        [(cadar)
          (cond
            [(not (equal? 1 arg-length)) (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- cadar expects one argument: ~s in ~s" arg-length args)]
            [(not (list? (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed cadar arguments ~% --- cadar expects a list argument: ~s in ~s" (1st args) args)]
            [else (cadar (1st args))]
          )
        ]
        [(caaar)
          (cond
            [(not (equal? 1 arg-length)) (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- caaar expects one argument: ~s in ~s" arg-length args)]
            [(not (list? (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed caaar arguments ~% --- caaar expects a list argument: ~s in ~s" (1st args) args)]
            [else (caaar (1st args))]
          )
        ]
        [(null?)
          (if (equal? 1 arg-length)
            (null? (1st args))
            (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- null expects one argument: ~s in ~s" arg-length args)
          )
        ]
        [(assq)
          (cond
            [(not (equal? 2 arg-length)) (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- assq expects two arguments: ~s in ~s" arg-length args)]
            [(not (andmap pair? (2nd args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed assq arguments ~% --- assq expects its second argument to be a list of lists: ~s in ~s" (2nd args) args)]
            [else (assq (1st args) (2nd args))]
          )
        ]
        [(eq?)
          (if (equal? 2 arg-length)
            (eq? (1st args) (2nd args))
            (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- eq? expects two arguments: ~s in ~s" arg-length args)
          )
        ]
        [(eqv?)
          (if (equal? 2 arg-length)
            (eqv? (1st args) (2nd args))
            (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- eqv? expects two arguments: ~s in ~s" arg-length args)
          )
        ]
        [(equal?)
          (if (equal? 2 arg-length)
            (equal? (1st args) (2nd args))
            (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- equal? expects two arguments: ~s in ~s" arg-length args)
          )
        ]
        [(atom?)
          (if (equal? 1 arg-length)
            (atom? (1st args))
            (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- atom? expects one argument: ~s in ~s" arg-length args)
          )
        ]
        [(length)
          (cond
            [(not (equal? 1 arg-length)) (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- length expects one argument: ~s in ~s" arg-length args)]
            [(not (list? (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed length argument ~% --- length expects a list: ~s in ~s" (1st args) args)]
            [else (length (1st args))]
          )
        ]
        [(list->vector)
          (cond
            [(not (equal? 1 arg-length)) (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- list->vector expects one argument: ~s in ~s" arg-length args)]
            [(not (list? (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed list-vector argument ~% --- list->vector expects a list arguments: ~s in ~s" (1st args) args)]
            [else (list->vector (1st args))]
          )
        ]
        [(list?)
          (if (equal? 1 arg-length)
            (list? (1st args))
            (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- list? expects one argument: ~s in ~s" arg-length args)
          )
        ]
        [(pair?)
          (if (equal? 1 arg-length)
            (pair? (1st args))
            (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- pair? expects one arguments: ~s in ~s" arg-length args)
          )
        ]
        [(procedure?)
          (if (equal? 1 arg-length)
            (proc-val? (1st args))
            (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- procedure? expects one arguments: ~s in ~s" arg-length args)
          )
        ]
        [(vector->list)
          (cond
            [(not (equal? 1 arg-length)) (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- vector->list expects one argument: ~s in ~s" arg-length args)]
            [(not (vector? (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed vector->list argument ~% --- vector->list expects a vector argument: ~s in ~s" (1st args) args)]
            [else (vector->list (1st args))]
          )
        ]
        [(vector)
          (apply vector args)
        ]
        [(make-vector)
          (cond
            [(not (equal? 2 arg-length)) (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- make-vector expects two arguments (size and initial fill value): ~s in ~s" arg-length args)]
            [(not (fixnum? (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed make-vector argument ~% --- make-vector expects the first argument to be a nonnegative fixnum: ~s in ~s" (1st args) args)]
            [(not (positive? (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed make-vector argument ~% --- make-vector expects the first argument to be a nonnegative fixnum: ~s in ~s" (1st args) args)]
          )
        ]
        [(vector-ref)
          (cond
            [(not (equal? 2 arg-length)) (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- vector-ref expects two arguments: ~s in ~s" arg-length args)]
            [(not (vector? (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed vector-ref argument ~% --- vector-ref expects the first argument to be a vector: ~s in ~s" (1st args) args)]
            [(not (ormap (lambda (proc) (proc (2nd args))) (list fixnum? positive? zero?)))
               (errorf 'apply-prim-proc "[ ERROR ]: Malformed vector-ref argument ~% --- vector-ref expects the second argument to be a nonnegative fixnum: ~s in ~s" (2nd args) args)]
            [(>= (2nd args) (vector-length (1st args))) (errorf 'apply-prim-proc "[ ERROR ]: Malformed vector-ref argument ~% --- vector-ref expects the second argument to be less than the length of the vector: ~s > ~s" (2nd args) (length (1st args)))]
            [else (vector-ref (1st args) (2nd args))]
          )
        ]
        [(vector?)
          (if (equal? 1 arg-length)
            (vector? (1st args))
            (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- vector? expects one argument: ~s in ~s" arg-length args)
          )
        ]
        [(number?)
          (if (equal? 1 arg-length)
            (number? (1st args))
            (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- number? expects one argument: ~s in ~s" arg-length args)
          )
        ]
        [(symbol?)
          (if (equal? 1 arg-length)
            (symbol? (1st args))
            (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- symbol? expects one argument: ~s in ~s" arg-length args)
          )
        ]
        [(set-car!)
          (cond
            [(not (equal? 2 arg-length))
              (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- set-car! expects two arguments: ~s in ~s" arg-length args)
            ]
            [(not (pair? (1st args)))
              (errorf 'apply-prim-proc "[ ERROR ]: Malformed set-car! argument ~% --- set-car! expects the first argument to be a pair: ~s in ~s" (1st args) args)
            ]
            [else
              (apply set-car! args)
            ]
          )
        ]
        [(set-cdr!)
          (cond
            [(not (equal? 2 arg-length))
              (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- set-cdr! expects two arguments: ~s in ~s" arg-length args)
            ]
            [(not (pair? (1st args)))
              (errorf 'apply-prim-proc "[ ERROR ]: Malformed set-cdr! argument ~% --- set-cdr! expects the first argument to be a pair: ~s in ~s" (1st args) args)
            ]
            [else
              (apply set-cdr! args)
            ]
          )
        ]
        [(vector-set!)
          (cond
            [(not (equal? 3 arg-length))
              (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- vector-set! expects three arguments: ~s in ~s" arg-length args)
            ]
            [(not (vector? (1st args)))
              (errorf 'apply-prim-proc "[ ERROR ]: Malformed vector-set! argument ~% --- vector-set! expects the first argument to be a vector: ~s in ~s" (1st args) args)
            ]
            [(or (not (integer? (2nd args))) (negative? (2nd args)))
              (errorf 'apply-prim-proc "[ ERROR ]: Malformed vector-set! argument ~% --- vector-set! expects the second argument to be a nonnegative integer: ~s in ~s" (2nd args) args)
            ]
            [else
              (apply vector-set! args)
            ]
          )
        ]
        [(display)
          (cond
            [(equal? 2 arg-length)
              (if (output-port? (2nd args))
                (apply display args)
                (errorf 'apply-prim-proc "[ ERROR ]: Malformed display argument ~% --- display's second argument must be an output port: ~s in ~s" (2nd args) args)
              )
            ]
            [(equal? 1 arg-length)
              (display (1st args))
            ]
            [else
              (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of display arguments ~% --- number of arguments is incompatible with display: ~s in ~s" arg-length args)
            ]
          )
        ]
        [(newline)
          (if (zero? arg-length)
            (newline)
            (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of newline arguments ~% --- newline expects zero arguments: ~s in ~s" arg-length args)
          )
        ]
        [(=) (apply = args)]
        [(list) (apply list args)]
        [(apply)
          (cond
            [(< arg-length 2)
              (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of apply arguments ~% --- apply expects at least 2 arguments: ~s in ~s" arg-length args)
            ]
            [(not (proc-val? (car args)))
              (errorf 'apply-prim-proc "[ ERROR ]: Malformed apply argument ~% --- apply's first argument must be a procedure: ~s in ~s" (car args) args)
            ]
            [(not (list? (car (last-pair args))))
              (errorf 'apply-prim-proc "[ ERROR ]: Malformed apply arguments ~% --- apply's last argument be a list: ~s in ~s" (car (last-pair args)) args)
            ]
            [else
              (apply-proc (car args) (apply cons* (cdr args)) k)
            ]
          )
        ]
        [(map)
          (let ([proc (car args)]
                [align-args (apply map list (cdr args))])
            (map-cps (lambda (arg k) (apply-proc proc arg k)) align-args k)
          )
        ]
        [(member)
          (cond
            [(< arg-length 2)
              (errorf 'apply-prim-proc "[ ERROR ]: Incorrect number of member arguments ~% --- member expects at least 2 arguments: ~s in ~s" arg-length args)
            ]
            [(not (list? (2nd args)))
              (errorf 'apply-prim-proc "[ ERROR ]: Malformed member argument ~% --- member expects the second argument to be a list: ~s in ~s" (2nd args) args)
            ]
            [else
              (apply member args)
            ]
          )
        ]
        [(append)
          (letrec ([append-check-cps (lambda (x continuation)
                                       (cond
                                         [(null? (cdr x))
                                           (apply-continuation continuation #t)
                                         ]
                                         [(list? (car x))
                                           (append-check-cps (cdr x)
                                                            continuation)
                                         ]
                                         [else
                                           (apply-continuation continuation #f)
                                         ]
                                       )
                                    )])
            (cond
              [(null? args)
                (append)
              ]
              [(append-check-cps args (lambda (x) x))
                (apply append args)
              ]
              [else
                (errorf 'apply-prim-proc "[ ERROR ]: Malformed append statement ~% --- append takes the form (append lst ... any)")
              ]
            )
          )
        ]
        [(list-tail)
          (cond
            [(not (equal? 2 arg-length))
              (errorf 'apply-prim-proc "[ ERROR ]: Malformed list-tail statement ~% --- list-tail must have exactly 2 arguments: ~s in ~s" arg-length args)
            ]
            [(not (list? (1st args)))
              (errorf 'apply-prim-proc "[ ERROR ]: Malformed list-tail statement ~% --- list-tail first argument must be a list: ~s" (1st args))
            ]
            [(or (negative? (2nd args)) (not (integer? (2nd args))))
              (errorf 'apply-prim-proc "[ ERROR ]: Malformed list-tail statement ~% --- list-tail second argument must be a nonnegative integer: ~s" (2nd args))
            ]
            [else
              (apply list-tail args)
            ]
          )
        ]
        [else
          (errorf 'apply-prim-proc "[ ERROR ]: Bad primitive procedure name ~% --- undefined primitive procedure: ~s" prim-proc)
        ]
      )
    )
  )
)

(define bind-args
  (lambda (variables arguments accumulator)
    (cond
      [(null? variables) (reverse accumulator)] ; (lambda (x y z) ...)
      [(null? (car variables)) (reverse (cons arguments accumulator))]  ; (lambda x ...)
      [else
        (cases parameter (car variables)
          [explicit-parameter (sym)
            (bind-args (cdr variables)
                       (cdr arguments)
                       (cons (car arguments) accumulator))
          ]
          [implicit-parameter (sym) ; (lambda (x . y) ...)
            (reverse (cons arguments accumulator))
          ]
          [else
            (errorf 'bind-args "[ ERROR ]: undefined parameter ~% --- the given parameter is undefined: ~s" s)
          ]
        )
      ]
    )
  )
)

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.
;  User-defined procedures will be added later.
(define apply-proc
  (lambda (proc-value args k)
    (cases proc-val proc-value
      [prim-proc (operator)
        (apply-prim-proc operator args k)
      ]
      [closure (variables bodies env)
        (eval-bodies bodies
          (extend-env (map dereference-parameter variables) (bind-args variables args '()) env)
        )
      ]
			; You will add other cases
      [else (errorf 'apply-proc "[ ERROR ]: Malformed proc-value ~% ---  unsuppported proc-value: ~s" proc-value)]
    )
  )
)
