#lang racket
(require test-engine/racket-tests)

;; T:
;; a <- a + b
;; b <- a

(define (origin-fib n)
  (origin-fib-iter 1 0 n))

(define (origin-fib-iter a b count)
  (if (= count 0)
      b
      (origin-fib-iter (+ a b) a (- count 1))))

;; T_pq:
;; a <- bq + aq + ap
;; b <- bp + aq


(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))     ; compute p'
                   (+ (* 2 p q) (* q q))      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


;; test
(check-expect (fib 0) (origin-fib 0))
(check-expect (fib 1) (origin-fib 1))
(check-expect (fib 5) (origin-fib 5))
(check-expect (fib 15) (origin-fib 15))
(check-expect (fib 100) (origin-fib 100))
(test)