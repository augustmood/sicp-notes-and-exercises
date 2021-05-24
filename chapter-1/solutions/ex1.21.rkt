#lang racket

(define (smallest-divisor n)
    (define (find-divisor test-divisor)
        (cond ((> (square test-divisor) n) n)
              ((divideable? test-divisor n) test-divisor)
              (else (find-divisor (+ test-divisor 1)))))
    
    (define (divideable? a b)
        (= (remainder b a) 0))

    (define (square i)
        (* i i))

    (find-divisor 2))

(smallest-divisor 199) ; 199
(smallest-divisor 1999) ; 1999
(smallest-divisor 19999) ; 7