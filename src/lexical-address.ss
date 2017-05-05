(define make-depthPosition
  (lambda (variables depth position accumulator)
    (if [null? variables]
      accumulator
      (let ([current (car variables)]
            [next (cdr variables)])
        (make-depthPosition next depth (add1 position)
          (cons (list current depth position) accumulator))
      )
    )
  )
)

(define depthPosition-contains?
  (lambda (var depthPosition)
    (if (null? depthPosition)
      #f
      (let* ([current (car depthPosition)]
             [variable (car current)]
             [depthAndPosition (cdr current)]
             [next (cdr depthPosition)])
        (if [eqv? var variable]
          depthAndPosition
          (depthPosition-contains? var next)
        )
      )
    )
  )
)

(define lexical-address
  (lambda (lexical)
    (let notate ([lexical lexical]
                 [depth 0]
                 [depthPosition '()])
      (cond
        [(null? lexical) '()]
        [(symbol? lexical)
          (let ([depthAndPosition (depthPosition-contains? lexical depthPosition)])
            (if depthAndPosition
              (let* ([globalDepth (car depthAndPosition)]
                     [position (cadr depthAndPosition)]
                     [relativeDepth (- depth globalDepth)])
                (cons `: (list relativeDepth position))
              )
              (list `: `free lexical)
            )
          )
        ]
        [(eqv? (car lexical) 'lambda)
          (let* ([variables (cadr lexical)]
                 [body (caddr lexical)]
                 [nextDepth (add1 depth)]
                 [nextDepthPosition (make-depthPosition variables nextDepth 0 depthPosition)])
            (list 'lambda variables (notate body nextDepth nextDepthPosition))
          )
        ]
        [(eqv? (car lexical) 'if) ; (if expression expression expression)
          (let ([condition (cadr lexical)]
                [trueExp (caddr lexical)]
                [falseExp (cadddr lexical)]
                [next (cdr lexical)])
            (list 'if (notate condition depth depthPosition)
                      (notate trueExp depth depthPosition)
                      (notate falseExp depth depthPosition))
          )
        ]
        [(eqv? (car lexical) 'let) ; (let (variableExpressions) body)
          (let* ([variableExpressions (cadr lexical)]
                 [body (caddr lexical)]
                 [variables (car (apply map list variableExpressions))]
                 [nextDepth (add1 depth)]
                 [nextDepthPosition (make-depthPosition variables nextDepth 0 depthPosition)]
                 [expressions (map cadr variableExpressions)])
            (list 'let
              (map list variables
                (map (lambda (exp) (notate exp depth depthPosition)) expressions))
              (notate body nextDepth nextDepthPosition)
            )
          )
        ]
        [(eqv? (car lexical) 'set!) ; (set! variable expression)
          (let* ([variable (cadr lexical)]
                 [expression (caddr lexical)]
                 [nextDepth (add1 depth)]
                 [nextDepthPosition (make-depthPosition (list variable) nextDepth 0 depthPosition)])
            (list 'set! variable (notate expression depth nextDepthPosition))
          )
        ]
        [else
          (cons (notate (car lexical) depth depthPosition)
                (notate (cdr lexical) depth depthPosition))
        ]
      )
    )
  )
)
