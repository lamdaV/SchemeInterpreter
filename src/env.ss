(define *prim-proc-names* '(+ - * / add1 sub1 = > < >= <= zero? not quotient cons member car cdr cadr caar cdar cddr caaar cdaar cadar caadr cddar cdadr caddr cdddr list null? assq eq? equal? atom? length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! display newline cadar map apply append eqv? list-tail))

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3
(define empty-env
  (lambda ()
    (empty-env-record)
  )
)

(define box-if-not
  (lambda (x)
    (if (box? x)
      x
      (box x)
    )
  )
)

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector (map box-if-not vals)) env)
  )
)

(define apply-proc-if-exists
  (lambda (sym syms exist-proc nonexist-proc)
    (let ([index (list-find-position sym syms)])
      (if index
        (exist-proc index)
        (nonexist-proc)
      )
    )
  )
)

(define vector-cons
  (lambda (val vec)
    (let ([vec-list (vector->list vec)])
      (list->vector (cons val vec-list))
    )
  )
)

(define get-box
  (lambda (depth position env)
    (cases environment env
      [extended-env-record (sym vals outer-env)
        (if (zero? depth)
          (vector-ref vals position)
          (get-box (sub1 depth) position outer-env)
        )
      ]
      [else
        (errorf 'get-box "[ ERROR ]: Invalid depth and position ~% --- the given depth and position do not result is a valid combination: (: ~s ~s)" depth position)
      ]
    )
  )
)

(define get-global-box
  (lambda (sym)
    (cases environment global-env
      [extended-env-record (syms vals env)
        (apply-proc-if-exists sym syms
          (lambda (index) (vector-ref vals index))
          (lambda ()
            (let* ([new-syms (cons sym syms)]
                   [new-box (box #f)]
                   [new-vals (vector-cons new-box vals)])
              (set! global-env (extended-env-record new-syms new-vals (empty-env)))
              new-box
            )
          ))
      ]
      [else
        (errorf 'get-global-box "[ ERROR ]: Incompatible argument ~% --- global-env must be an environment of type extended-env-record but is an empty-env-record")
      ]
    )
  )
)

(define mutate-env
  (lambda (lex-addr new-value env)
    (cases lex-address lex-addr
      [bound (depth position)
        (set-box! (get-box depth position env) new-value)
      ]
      [free (sym)
        (set-box! (get-global-box sym) new-value)
      ]
      [else
        (errorf 'mutate-env "[ ERROR ]: Unexpected lex-addr ~% --- lex-addr must either be bound or free but was undefined: ~s" lex-addr)
      ]
    )
  )
)

; Returns the index of the sym in los. If it is not is los, returns #f
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)
  )
)

(define set-at-index!
  (lambda (index val ls)
    (if (zero? index)
      (set-car! ls val)
      (set-at-index! (- index 1) val (cdr ls))
    )
  )
)

; Returns the index where pred is true in ls
(define list-index
  (lambda (pred ls)
    (cond
      [(null? ls) #f]
      [(pred (car ls)) 0]
      [else
        (let ([list-index-r (list-index pred (cdr ls))])
	        (if (number? list-index-r)
		          (+ 1 list-index-r)
		          #f
          )
        )
      ]
    )
  )
)

; Applies the succeed callback if symbol is found in the environment (local + global)
(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      [empty-env-record ()
        (fail)
      ]
      [extended-env-record (syms vals env)
	      (let ([pos (list-find-position sym syms)])
      	  (if (number? pos)
	            (succeed  (vector-ref vals pos))
	            (apply-env env sym succeed fail)
          )
        )
      ]
      [else
        (errorf 'apply-env "[ ERROR ]: malformed environment ~% --- unexpected environment type: ~s" env)
      ]
    )
  )
)

; Constructs an initial environment populated with basic procedures.
(define make-init-env         ; for now, our initial global environment only contains
  (lambda ()
    (extend-env            ; procedure names.  Recall that an environment associates
       *prim-proc-names*   ;  a value (not an expression) with an identifier.
       (map prim-proc
            *prim-proc-names*)
       (empty-env)
    )
  )
)

; Restore the global-env to the initial environment.
(define reset-global-env
  (lambda ()
    (set! global-env (make-init-env))
  )
)

; Defines a global-env.
(define global-env (make-init-env))
