; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;
; Claude Anderson.  Last modified April, 2014

(load "chez-init.ss")

(define load-all ; make it easy to reload the files
  (lambda ()
    (load "datatypes.ss")
    (load "env.ss")
    (load "apply-proc.ss")
    (load "parse.ss")
    (load "evaluator.ss")
    (load "interpreter.ss")
  )
)

(define print-info
  (lambda (selector)
    (display "[ INFO ]: test ")
    (display selector)
    (display " successfully loaded.")
    (newline)
  )
)

(define load-test
  (lambda (selector)
    (case selector
      [(13)
       (load "test/13-test.ss")
       (print-info selector)
      ]
      [(14)
        (load "test/14-test.ss")
        (print-info selector)
      ]
      [else
        (display "[ ERROR ]: Unsupported test selector")
        (newline)
        (display " --- selector ")
        (display selector)
        (display " is not defined")
        (newline)
      ]
    )
  )
)

(load-all)

(define l load-all) ; even easier!
