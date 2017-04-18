; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;
; Claude Anderson.  Last modified April, 2014

(load "chez-init.ss")

(define load-all ; make it easy to reload the files
  (lambda ()
    (load "datatypes.ss")
    (load "parse.ss")
    (load "env.ss")
    (load "apply.ss")
    (load "interpreter.ss")
  )
)

(define load-test
  (lambda ()
    (load "13-test.ss")
  )
)

(load-all)

(define l load-all) ; even easier!
