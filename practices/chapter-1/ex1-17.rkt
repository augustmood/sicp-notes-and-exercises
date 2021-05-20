#lang racket
(require racket/trace)
(require test-engine/racket-tests)

(define (double i)
    (* i 2))

(define (halve i)
    (/ i 2))
q
(define (fast-new-mul a b)
    (cond ((= b 0) 0)
          ((even? b) (double (fast-new-mul a (halve b))))
          (else (+ a (fast-new-mul a (- b 1))))))

;; test

(check-expect (fast-new-mul 3 4) 12)
(check-expect (fast-new-mul 2 5) 10)
(check-expect (fast-new-mul 2 0) 0)
(check-expect (fast-new-mul 2 1) 2)
(check-expect (fast-new-mul 15 25) 375)
(test)


(trace fast-new-mul)
(fast-new-mul 2 9)
(fast-new-mul 2 10)