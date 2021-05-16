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
  (cond ((fast-prime? n 100)
         (begin 
           (display n)
           (report-prime (- (runtime) start-time))
           ))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))



;; fast prime

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m)))) 

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)

(newline)

(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)

;; expect the time need by test primes around 1000000 is roughly twice as slow as testing primes 
;; around 1000.