; top-level-eval evaluates a form in the global environment

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

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
    (map (lambda (x) (eval-exp x env)) rands)
  )
)

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.
;  User-defined procedures will be added later.
(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (operator)
        (apply-prim-proc operator args)
      ]
      [closure (ids bodies env)
        (eval-bodies bodies
          (extend-env ids args env)
        )
      ]
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s"
                    proc-value)])))

(define *prim-proc-names* '(+ - * / add1 sub1 = > < >= <= zero? not cons car cdr cadr caar cdar cddr caaar cdaar cadar caadr cddar cdadr caddr cdddr list null? assq eq? equal? atom? length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! display newline cadar ))

(define init-env         ; for now, our initial global environment only contains
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc
          *prim-proc-names*)
     (empty-env)
  )
)

(define global-env init-env)

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
                (lambda () (eopl:error 'apply-env ; procedure to call if variable not in env
                  "variable not found in environment: ~s"
                   variable)
                )
             )
            )
          )
        ]
        [if-then-exp (conditional true-exp)
          (if (eval-exp conditional env)
            (eval-exp true-exp env)
          )
        ]
        [if-else-exp (conditional true-exp false-exp)
          (if (eval-exp conditional env)
            (eval-exp true-exp env)
            (eval-exp false-exp env)
          )
        ]
        [app-exp (operator arguments)
          (let ([proc-value (eval-exp operator env)]
                [args (eval-rands arguments env)])
            (apply-proc proc-value args)
          )
        ]
        [let-exp (let-type name variables values body)
          (eval-bodies body (extend-env variables (eval-rands values env) env))
        ]
        [lambda-exp (required optional body)
          (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)
          ; TODO: later
        ]
        [lambda-exact-exp (variables body)
          (closure variables body env)
        ]
        [set!-exp (variable value) (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]
      )
    )
  )
)


; Usually an interpreter must define each
; built-in procedure individually.  We are "cheating" a little bit.
(define apply-prim-proc
  (lambda (prim-proc args)
    (let ([argsLength (length args)])
      (case prim-proc
        [(+)
          (if ((list-of number?) args)
            (apply + args)
            (error 'apply-prim-proc "[ ERROR ]: Malformed + argument ~% --- + expects arguments a list of numbers: ~s" args)
          )
        ]
        [(-)
          (if ((list-of number?) args)
            (apply - args)
            (error 'apply-prim-proc "[ ERROR ]: Malformed - argument ~% --- - expects arguments a list of numbers: ~s" args)
          )
        ]
        [(*)
          (if ((list-of number?) args)
            (apply * args)
            (error 'apply-prim-proc "[ ERROR ]: Malformed * argument ~% --- * expects arguments a list of numbers: ~s" args)
          )
        ]
        [(/)
          (if ((list-of number?) args)
            (apply / args)
            (error 'apply-prim-proc "[ ERROR ]: Malformed / argument ~% --- / expects arguments a list of numbers: ~s" args)
          )
        ]
        [(<)
          (if (and (not (null? args)) ((list-of number?) args))
            (apply < args)
            (error 'apply-prim-proc "[ ERROR ]: Malformed / argument ~% --- < expects arguments a list of numbers: ~s" args)
          )
        ]
        [(>)
          (if (and (not (null? args)) ((list-of number?) args))
            (apply > args)
            (error 'apply-prim-proc "[ ERROR ]: Malformed / argument ~% --- > expects arguments a list of numbers: ~s" args)
          )
        ]
        [(>=)
          (if (and (not (null? args)) ((list-of number?) args))
            (apply >= args)
            (error 'apply-prim-proc "[ ERROR ]: Malformed / argument ~% --- >= expects arguments a list of numbers: ~s" args)
          )
        ]
        [(<=)
          (if (and (not (null? args)) ((list-of number?) args))
            (apply <= args)
            (error 'apply-prim-proc "[ ERROR ]: Malformed / argument ~% --- <= expects arguments a list of numbers: ~s" args)
          )
        ]
        [(not)
          (if (equal? 1 argsLength)
            (not (1st args))
            (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- not expects one argument: ~s in ~s" argLength args)
          )
        ]
        [(zero?)
          (if (equal? 1 argsLength)
            (zero? (1st args))
            (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- zero? expects one argument: ~s in ~s" argLength args)
          )
        ]
        [(add1)
          (if (equal? 1 argsLength)
            (add1 (1st args))
            (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- add1 expects one argument: ~s in ~s" argsLength args)
          )
        ]
        [(sub1)
          (if (equal? 1 argsLength)
            (sub1 (1st args))
            (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- sub1 expects one argument: ~s in ~s" argsLength args)
          )
        ]
        [(cons) (cons (1st args) (2nd args))]
        [(cdr)
          (cond
            [(not (equal? 1 argsLength)) (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- cdr expects one argument: ~s in ~s" argsLength args)]
            [(not (list? (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed cdr arguments ~% --- cdr expects a list argument: ~s in ~s" (1st args) args)]
            [else (cdr (1st args))]
          )
        ]
        [(car)
          (cond
            [(not (equal? 1 argsLength)) (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- car expects one argument: ~s in ~s" argsLength args)]
            [(not (list? (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed car arguments ~% --- car expects a list argument: ~s in ~s" (1st args) args)]
            [else (car (1st args))]
          )
        ]
        [(cadr)
          (cond
            [(not (equal? 1 argsLength)) (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- cadr expects one argument: ~s in ~s" argsLength args)]
            [(not (list? (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed cadr arguments ~% --- cadr expects a list argument: ~s in ~s" (1st args) args)]
            [else (cadr (1st args))]
          )
        ]
        [(cddr)
          (cond
            [(not (equal? 1 argsLength)) (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- cddr expects one argument: ~s in ~s" argsLength args)]
            [(not (list? (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed cddr arguments ~% --- cddr expects a list argument: ~s in ~s" (1st args) args)]
            [else (cddr (1st args))]
          )
        ]
        [(cdar)
          (cond
            [(not (equal? 1 argsLength)) (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- cdar expects one argument: ~s in ~s" argsLength args)]
            [(not (list? (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed cdar arguments ~% --- cdar expects a list argument: ~s in ~s" (1st args) args)]
            [else (cdar (1st args))]
          )
        ]
        [(caar)
          (cond
            [(not (equal? 1 argsLength)) (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- caar expects one argument: ~s in ~s" argsLength args)]
            [(not (list? (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed caar arguments ~% --- caar expects a list argument: ~s in ~s" (1st args) args)]
            [else (caar (1st args))]
          )
        ]
        [(cdddr)
          (cond
            [(not (equal? 1 argsLength)) (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- cdddr expects one argument: ~s in ~s" argsLength args)]
            [(not (list? (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed cdddr arguments ~% --- cdddr expects a list argument: ~s in ~s" (1st args) args)]
            [else (cdddr (1st args))]
          )
        ]
        [(caddr)
          (cond
            [(not (equal? 1 argsLength)) (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- caddr expects one argument: ~s in ~s" argsLength args)]
            [(not (list? (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed caddr arguments ~% --- caddr expects a list argument: ~s in ~s" (1st args) args)]
            [else (caddr (1st args))]
          )
        ]
        [(cdadr)
          (cond
            [(not (equal? 1 argsLength)) (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- cdadr expects one argument: ~s in ~s" argsLength args)]
            [(not (list? (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed cdadr arguments ~% --- cdadr expects a list argument: ~s in ~s" (1st args) args)]
            [else (cdadr (1st args))]
          )
        ]
        [(cddar)
          (cond
            [(not (equal? 1 argsLength)) (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- cddar expects one argument: ~s in ~s" argsLength args)]
            [(not (list? (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed cddar arguments ~% --- cddar expects a list argument: ~s in ~s" (1st args) args)]
            [else (cddar (1st args))]
          )
        ]
        [(cdaar)
          (cond
            [(not (equal? 1 argsLength)) (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- cdaar expects one argument: ~s in ~s" argsLength args)]
            [(not (list? (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed cdaar arguments ~% --- cdaar expects a list argument: ~s in ~s" (1st args) args)]
            [else (cdaar (1st args))]
          )
        ]
        [(cadar)
          (cond
            [(not (equal? 1 argsLength)) (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- cadar expects one argument: ~s in ~s" argsLength args)]
            [(not (list? (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed cadar arguments ~% --- cadar expects a list argument: ~s in ~s" (1st args) args)]
            [else (cadar (1st args))]
          )
        ]
        [(caaar)
          (cond
            [(not (equal? 1 argsLength)) (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments  ~% --- caaar expects one argument: ~s in ~s" argsLength args)]
            [(not (list? (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed caaar arguments ~% --- caaar expects a list argument: ~s in ~s" (1st args) args)]
            [else (caaar (1st args))]
          )
        ]
        [(null?)
          (if (equal? 1 argsLength)
            (null? (1st args))
            (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- null expects one argument: ~s in ~s" argsLength args)
          )
        ]
        [(assq)
          (cond
            [(not (equal? 2 argsLength)) (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- assq expects two arguments: ~s in ~s" argsLength args)]
            [(not ((list-of list?) (2nd args))) (error 'apply-prim-proc "[ ERROR ]: Malformed assq arguments ~% --- assq expects its second argument to be a list of lists: ~s in ~s" (2nd args) args)]
            [else (assq (1st args) (2nd args))]
          )
        ]
        [(eq?)
          (if (equal? 2 argsLength)
            (eq? (1st args) (2nd args))
            (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- eqv? expects two arguments: ~s in ~s" argsLength args)
          )
        ]
        [(equal?)
          (if (equal? 2 argsLength)
            (equal? (1st args) (2nd args))
            (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- equal? expects two arguments: ~s in ~s" argsLength args)
          )
        ]
        [(atom?)
          (if (equal? 1 argsLength)
            (atom? (1st args))
            (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- atom? expects one argument: ~s in ~s" argsLength args)
          )
        ]
        [(length)
          (cond
            [(not (equal? 1 argsLength)) (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- length expects one argument: ~s in ~s" argsLength args)]
            [(not (list? (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed length argument ~% --- length expects a list: ~s in ~s" (1st args) args)]
            [else (length (1st args))]
          )
        ]
        [(list->vector)
          (cond
            [(not (equal? 1 argsLength)) (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- list->vector expects one argument: ~s in ~s" argsLength args)]
            [(not (list? (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed list-vector argument ~% --- list->vector expects a list arguments: ~s in ~s" (1st args) args)]
            [else (list->vector (1st args))]
          )
        ]
        [(list?)
          (if (equal? 1 argsLength)
            (list? (1st args))
            (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- list? expects one argument: ~s in ~s" argsLength args)
          )
        ]
        [(pair?)
          (if (equal? 1 argsLength)
            (pair? (1st args))
            (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- pair? expects one arguments: ~s in ~s" argsLength args)
          )
        ]
        [(procedure?)
          (if (equal? 1 argsLength)
            (proc-val? (1st args))
            (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- procedure? expects one arguments: ~s in ~s" argsLength args)
          )
        ]
        [(vector->list)
          (cond
            [(not (equal? 1 argsLength)) (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- vector->list expects one argument: ~s in ~s" argsLength args)]
            [(not (vector? (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed vector->list argument ~% --- vector->list expects a vector argument: ~s in ~s" (1st args) args)]
            [else (vector->list (1st args))]
          )
        ]
        [(vector)
          (apply vector args)
        ]
        [(make-vector)
          (cond
            [(not (equal? 2 argsLength)) (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- make-vector expects two arguments (size and initial fill value): ~s in ~s" argsLength args)]
            [(not (fixnum? (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed make-vector argument ~% --- make-vector expects the first argument to be a nonnegative fixnum: ~s in ~s" (1st args) args)]
            [(not (positive? (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed make-vector argument ~% --- make-vector expects the first argument to be a nonnegative fixnum: ~s in ~s" (1st args) args)]
          )
        ]
        [(vector-ref)
          (cond
            [(not (equal? 2 argsLength)) (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- vector-ref expects two arguments: ~s in ~s" argLength args)]
            [(not (vector? (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed vector-ref argument ~% --- vector-ref expects the first argument to be a vector: ~s in ~s" (1st args) args)]
            [(not (or (fixnum? (2nd args)) (positive? (2nd args)) (zero? (2nd args)))) (error 'apply-prim-proc "[ ERROR ]: Malformed vector-ref argument ~% --- vector-ref expects the second argument to be a nonnegative fixnum: ~s in ~s" (2nd args) args)]
            [(>= (2nd args) (vector-length (1st args))) (error 'apply-prim-proc "[ ERROR ]: Malformed vector-ref argument ~% --- vector-ref expects the second argument to be less than the length of the vector: ~s > ~s" (2nd args) (length (1st args)))]
            [else (vector-ref (1st args) (2nd args))]
          )
        ]
        [(vector?)
          (if (equal? 1 argsLength)
            (vector? (1st args))
            (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- vector? expects one arguments : ~s in ~s" argLength args)
          )
        ]
        [(number?)
          (if (equal? 1 argsLength)
            (number? (1st args))
            (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- number? expects one arguments : ~s in ~s" argLength args)
          )
        ]
        [(symbol?)
          (if (equal? 1 argsLength)
            (symbol? (1st args))
            (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- symbol? expects one arguments : ~s in ~s" argLength args)
          )
        ]
        [(set-car!)
          ; TODO: fix this ADAM
          (error 'apply-prim-proc "[ ERROR ]: Unsupported Operation Exception ~% --- Contact Adam immediately: set-car!")
        ]
        [(set-cdr!)
          ; TODO: fix this ADAM
          (error 'apply-prim-proc "[ ERROR ]: Unsupported Operation Exception ~% --- Contact Adam immediately: set-cdr!")
        ]
        [(vector-set!)
          ; TODO: fix this ADAM
          (error 'apply-prim-proc "[ ERROR ]: Unsupported Operation Exception ~% --- Contact Adam immediately: vector-set!")
        ]
        [(display)
          (cond
            [(equals? 2 argsLength)
              (if (output-port? (2nd args))
                (apply display args)
                (error 'apply-prim-proc "[ ERROR ]: Malformed display argument ~% --- display's second argument must be an output port: ~s in ~s" (2nd args) args)
              )
            ]
            [(equals? 1 argsLength)
              (display (1st args))
            ]
            [else
              (error 'apply-prim-proc "[ ERROR ]: Incorrect number of display arguments ~% --- number of arguments is incompatible with display: ~s in ~s" argsLength args)
            ]
          )
        ]
        [(newline)
          (if (zero? argsLength)
            (newline)
            (error 'apply-prim-proc "[ ERROR ]: Incorrect number of newline arguments ~% --- newline expects zero arguments: ~s in ~s" argsLength args)
          )
        ]
        [(=) (apply = args)]
        [(list) (apply list args)]
        [else
          (error 'apply-prim-proc "Bad primitive procedure name: ~s" prim-op)
        ]
      )
    )
  )
)

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer)
      (newline)
      (rep) ; tail-recursive, so stack doesn't grow.
    )
  )
)

(define eval-one-exp
  (lambda (x)
    (top-level-eval (parse-exp x))
  )
)
