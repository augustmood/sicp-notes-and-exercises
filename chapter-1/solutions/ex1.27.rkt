#lang racket
(require test-engine/racket-tests)

; Numbers that fool the Fermat test are called Carmichael numbers, and little is known about them 
; other than that they are extremely rare. There are 255 Carmichael numbers below 100,000,000. The 
; smallest few are 561, 1105, 1729, 2465, 2821, and 6601. In testing primality of very large numbers 
; chosen at random, the chance of stumbling upon a value that fools the Fermat test is less than the 
; chance that cosmic radiation will cause the computer to make an error in carrying out a ``correct'' 
; algorithm. Considering an algorithm to be inadequate for the first reason but not for the second 
; illustrates the difference between mathematics and engineering.

;; Fermat test

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

(define (square n)
  (* n n))

; (fermat-test 1105)

(define (carmi-test n)
   (define (try-it a)
     (= (expmod a n n) a))
   (define (try-it-iter i)
     (if (= i n)
       #t
       (and (try-it i)
         (try-it-iter (+ i 1)))))
     (try-it-iter 1)
   )

(check-expect (carmi-test 561) #t)
(check-expect (carmi-test 1105) #t)
(check-expect (carmi-test 1729) #t)
(check-expect (carmi-test 2465) #t)
(check-expect (carmi-test 2821) #t)
(check-expect (carmi-test 6601) #t)
(test)