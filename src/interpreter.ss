; top-level-eval evaluates a form in the global environment
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

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
