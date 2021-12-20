#lang racket
(require rackunit rackunit/text-ui)

(define head car)
(define tail cdr)

(define (minimum lst)
  (foldr min (head lst) (tail lst)))

(define (minimum-2 lst) (apply min lst))


(define (contains? x lst)
  (cond ((null? lst) #f)
        ((equal? (car lst) x) #t)
        (else (contains? x (cdr lst)))))


(define (findDuplicates lst)
  (cond ((null? lst) '())
        ((member (car lst) (cdr lst)) (cons (car lst) (findDuplicates (cdr lst))))
        (else (findDuplicates (cdr lst)))))


(define (min-duplicate l)
  (define duplicates (findDuplicates l))
  (minimum duplicates))

(run-tests
  (test-suite "min-duplicate tests"
    (check-eq? (min-duplicate '(-8 -8))
               -8)
    (check-eq? (min-duplicate '(1 2 3 4 4 5 6))
               4)
    (check-eq? (min-duplicate '(5 1 2 3 4 5 3 6 2 3 2 3 2 3))
               2)
    (check-eq? (min-duplicate '(3 2 2 2 1 1))
               1))
  'verbose)