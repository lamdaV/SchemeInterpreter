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
    (map (lambda (exp) (eval-exp exp env)) rands)
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
      [else (errorf 'apply-proc
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
                         (lambda ()
                           (errorf 'apply-env ; procedure to call if variable not in env
                                  "[ ERROR ]: Unable to find variable in environment ~% --- variable not found in environment: ~s"
                                  variable)))
            )
          )
        ]
        [if-then-exp (conditional true-exp)
          (if (eval-exp conditional env)
            (eval-exp true-exp env)
            (void)
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
          (cond
            [(equal? 'let let-type)
              (if name
                (errorf 'eval-exp "[ ERROR ]: Unsupported let type ~% --- name let unsupported: ~s" name) ; TODO
                (eval-bodies body (extend-env variables (eval-rands values env) env))
              )
            ]
            [else
              (errorf 'eval-exp "[ ERROR ]: Unsupported let type ~% --- let-type: ~s" let-type)
              ; TODO:
            ]
          )
        ]
        [lambda-exp (required optional body)
          ;(errorf 'eval-exp "[ ERROR ]: Unsupported lambda-exp ~% --- lambda expression: ~a" (unparse-exp exp))
          ;; TODO: later
          (if (null? required)
            (closure (list optional) body env)
            (closure (reverse (cons optional (reverse required))) body env)
          )
        ]
        [lambda-exact-exp (variables body)
          (closure variables body env)
        ]
        [set!-exp (variable value)
          (errorf 'eval-exp "[ ERROR ]: Unsupported operation ~% --- operation: ~s" (unparse-exp exp)) ; TODO
        ]
        [else
          (errorf 'eval-exp "[ ERROR ]: Malformed syntax ~% --- unexpected expression: ~s" (unparse-exp exp))
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
