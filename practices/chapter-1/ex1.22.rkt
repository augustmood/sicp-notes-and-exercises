#lang racket

(define (runtime) (current-inexact-milliseconds))
(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (cond ((prime? n)
         (begin 
           (display n)
           (report-prime (- (runtime) start-time))
           ))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))


(define (search-for-primes start ends)
  (if (even? start)
      (search-for-primes (+ start 1) ends)
      (cond ((or (> start ends)) (display "end") (newline))
            (else (timed-prime-test start) 
                  (search-for-primes (+ 2 start) ends)))
      ))


(search-for-primes 1000 1100)
(search-for-primes 10000 10100)
(search-for-primes 100000 100100)
(search-for-primes 1000000 1000100)

;; roughtly sqrt(10)