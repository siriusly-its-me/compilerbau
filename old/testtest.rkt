#lang racket

(require rackunit)

(define (ma-fonction-a-tester x)
  (* x 2))

(test-suite
  "mes-tests"
  (test-case "Test de ma-fonction-a-tester"
    (check-equal? (ma-fonction-a-tester 2) 4)
    (check-equal? (ma-fonction-a-tester 5) 10)))

(define discarded-results (map run-test programs-outputs))

(run-test 'mes-tests)
