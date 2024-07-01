#lang racket


(define (function-with-canary-stack x)
  (let ((canary #'identity))
    (begin
      (check-canary canary)
      (let ((result (x + 1)))
        (begin
          (check-canary canary)
          result
        )
      )
    )
  )
)

(define (check-canary canary)
  (if (canary #'identity)
    #f
    (error "Canary Stack corrompu")))
