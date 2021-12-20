#lang racket
(require rackunit rackunit/text-ui)

;### Зад 1
; Дали `x` е автоморфно?
; Едно число е автоморфно, ако квадратът му завършва на него.
(define (square number) (* number number))
(define (last-digit n) (remainder n 10))
(define (remove-last n) (quotient n 10))


(define (cont num1 num2)
  (cond
         ((<= num2 0) #t)
         ((= (last-digit num1) (last-digit num2)) (cont (remove-last num1) (remove-last num2)))
         (else #f))
)
(define (automorphic? x)
  (cont (square x) x))

(run-tests
  (test-suite
    "automorphic? tests"
    (check-false (automorphic? 2))
    (check-false (automorphic? 3))
    (check-false (automorphic? 4))
    (check-false (automorphic? 7))
    (check-false (automorphic? 8))
    (check-false (automorphic? 9))
    (check-false (automorphic? 10))

    (check-true (automorphic? 1))
    (check-true (automorphic? 5))
    (check-true (automorphic? 6))
    (check-true (automorphic? 25))
    (check-true (automorphic? 76))
    (check-true (automorphic? 376))
    (check-true (automorphic? 625))
    (check-true (automorphic? 9376)))
  'verbose)