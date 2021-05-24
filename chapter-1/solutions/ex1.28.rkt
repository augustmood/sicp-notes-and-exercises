#lang racket
(require test-engine/racket-tests)
;; Miller-Rabin test

(define (square n)
    (* n n))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (define a (square (expmod base (/ exp 2) m)))
         (if (= a 1)
            0
            (remainder a m)))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
    (define (test-iter i)
        (if (= n i)
            #t
            (if (= (expmod i n n) i)
                (test-iter (+ i 1))
                #f)))
  (test-iter 1))

(check-expect (miller-rabin-test 561) #f)
(check-expect (miller-rabin-test 1105) #f)
(check-expect (miller-rabin-test 1729) #f)
(check-expect (miller-rabin-test 2465) #f)
(check-expect (miller-rabin-test 2821) #f)
(check-expect (miller-rabin-test 6601) #f)
(test)
