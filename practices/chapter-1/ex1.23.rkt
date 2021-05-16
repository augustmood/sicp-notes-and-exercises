#lang racket

(define (runtime) (current-inexact-milliseconds))

(define (square n)
  (* n n))

(define (smallest-divisor n func)
  (find-divisor n 2 func))

(define (find-divisor n test-divisor func)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (func test-divisor) func))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (addone n)
  (+ n 1))

(define (prime? n func)
  (= n (smallest-divisor n func)))

(define (timed-prime-test n func)
  (start-prime-test n (runtime) func))

(define (start-prime-test n start-time func)
  (cond ((prime? n func)
      (begin 
        (display n)
        (report-prime (- (runtime) start-time))
        ))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))


(define (search-for-primes start ends func)
  (if (even? start)
      (search-for-primes (+ start 1) ends func)
      (cond ((or (> start ends)) (display "end") (newline))
            (else (timed-prime-test start func) 
                  (search-for-primes (+ 2 start) ends func)))
        ))

(timed-prime-test 1009 addone)
(timed-prime-test 1009 next)
(timed-prime-test 1013 addone)
(timed-prime-test 1013 next)
(timed-prime-test 1019 addone)
(timed-prime-test 1019 next)

(newline)

(timed-prime-test 10007 addone)
(timed-prime-test 10007 next)
(timed-prime-test 10009 addone)
(timed-prime-test 10009 next)
(timed-prime-test 10037 addone)
(timed-prime-test 10037 next)

(newline)

(timed-prime-test 100003 addone)
(timed-prime-test 100003 next)
(timed-prime-test 100019 addone)
(timed-prime-test 100019 next)
(timed-prime-test 100019 addone)
(timed-prime-test 100019 next)

;; the expectation is confirmed. the running time with func addone is roughly twice as long as with 
;; func next.