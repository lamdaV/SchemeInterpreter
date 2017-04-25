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
    (load "syntax-expand.ss")
    (load "evaluator.ss")
    (load "interpreter.ss")
  )
)

(define print-info
  (lambda (selector)
    (display (string-append "[ INFO ]: test " (number->string selector) " successfully loaded\n"))
  )
)

(define print-error
  (lambda (selector)
    (display (string-append "[ ERROR ]: Unsupported selector\n --- selector " (number->string selector) " is not supported\n"))
  )
)

(define SUPPORTED_SELECTOR (list 13 14))

(define load-test
  (lambda (selector)
    (if (ormap (lambda (x) (equal? selector x)) SUPPORTED_SELECTOR)
      (begin
        (load (string-append "../test/" (number->string selector) "-test.ss"))
        (print-info selector)
      )
      (print-error selector)
    )
  )
)

(load-all)

(define l load-all) ; even easier!
