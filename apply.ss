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
            (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- vector? expects one argument: ~s in ~s" argLength args)
          )
        ]
        [(number?)
          (if (equal? 1 argsLength)
            (number? (1st args))
            (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- number? expects one argument: ~s in ~s" argLength args)
          )
        ]
        [(symbol?)
          (if (equal? 1 argsLength)
            (symbol? (1st args))
            (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- symbol? expects one argument: ~s in ~s" argLength args)
          )
        ]
        [(set-car!)
          (cond
            [(not (equal? 2 argsLength))
              (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- set-car! expects two arguments: ~s in ~s" argLength args)
            ]
            [(not (pair? (1st args)))
              (error 'apply-prim-proc "[ ERROR ]: Malformed set-car! argument ~% --- set-car! expects the first argument to be a pair: ~s in ~s" (1st args) args)
            ]
            [else
              (apply set-car! args)
            ]
          )
        ]
        [(set-cdr!)
          (cond
            [(not (equal? 2 argsLength))
              (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- set-cdr! expects two arguments: ~s in ~s" argLength args)
            ]
            [(not (pair? (1st args)))
              (error 'apply-prim-proc "[ ERROR ]: Malformed set-cdr! argument ~% --- set-cdr! expects the first argument to be a pair: ~s in ~s" (1st args) args)
            ]
            [else
              (apply set-cdr! args)
            ]
          )
        ]
        [(vector-set!)
          (cond
            [(not (equal? 3 argsLength))
              (error 'apply-prim-proc "[ ERROR ]: Incorrect number of arguments ~% --- vector-set! expects three arguments: ~s in ~s" argLength args)
            ]
            [(not (vector? (1st args)))
              (error 'apply-prim-proc "[ ERROR ]: Malformed vector-set! argument ~% --- vector-set! expects the first argument to be a vector: ~s in ~s" (1st args) args)
            ]
            [(or (not (integer? (2nd args))) (negative? (2nd args)))
              (error 'apply-prim-proc "[ ERROR ]: Malformed vector-set! argument ~% --- vector-set! expects the second argument to be a nonnegative integer: ~s in ~s" (2nd args) args)
            ]
            [else
              (apply vector-set! args)
            ]
          )
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
