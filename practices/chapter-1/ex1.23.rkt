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

; (define (smallest-divisor n)
;   (find-divisor n 2))
; (define (find-divisor n test-divisor)
;   (cond ((> (square test-divisor) n))
;         ((divides? test-divisor n) test-divisor )
;         (else (find-divisor n (next test-divisor)))))
; (define (divides? a b)
;   (= (remainder b a) 0))

; (define (next n)
;     (if (= n 2)
;         3
;         (+ n 2)))

(smallest-divisor 1009)
(timed-prime-test 1009)
(smallest-divisor 1013)
(timed-prime-test 1013)
(smallest-divisor 1019)
(timed-prime-test 1019)

(newline)

(smallest-divisor 10007)
(timed-prime-test 10007)
(smallest-divisor 10009)
(timed-prime-test 10009)
(smallest-divisor 10037)
(timed-prime-test 10037)

(newline)

(smallest-divisor 100003)
(timed-prime-test 100003)
(smallest-divisor 100019)
(timed-prime-test 100019)
(smallest-divisor 100019)
(timed-prime-test 100019)