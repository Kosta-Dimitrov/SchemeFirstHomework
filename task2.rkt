#lang racket
(require rackunit rackunit/text-ui)

;### Зад 2
; Броят на наредените двойки цели числа от интервала [`a`,`b`],
; които имат най-голям общ делител равен на `n`.
  (define (interval start end)
    (if(> start end) '() (cons start (interval (+ 1 start) end))))

(define (gcd-count lst n counter)
  (cond((null? lst) counter)
       ((equal? n (my-gcd (car (car lst)) (car(cdr (car lst))))) (gcd-count (cdr lst) n (+ 1 counter)))
       (else (gcd-count (cdr lst) n counter))))

(define (my-gcd a b)
  (cond ((equal? a b) a)
        ((< a b) (my-gcd a (- b a)))
        (else (my-gcd (- a b) b))))


(define (count-pairs-gcd n a b)
  
  (define a-to-b (interval a b))
  
 (define all-combinations (combinations a-to-b 2))

  (define counter (gcd-count all-combinations n 0))
  (if (and (>= n a) (<= n b)) (+ 1 (* 2 counter)) (* 2 counter))


  )
(run-tests
  (test-suite "count-pairs-gcd tests"
    (check-eq? (count-pairs-gcd 10 1 11)
               1)
    (check-eq? (count-pairs-gcd 8 1 11)
               1)
    (check-eq? (count-pairs-gcd 3 1 11)
               7)
    (check-eq? (count-pairs-gcd 16 1 11)
               0)
    (check-eq? (count-pairs-gcd 4 1 11)
               3))
  'verbose)