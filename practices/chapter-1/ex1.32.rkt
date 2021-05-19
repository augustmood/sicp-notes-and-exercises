#lang racket
(require test-engine/racket-tests)

;; a
(define (accumulate-a combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner result (term a)))))
    (iter a null-value))

;; b
(define (accumulate-b combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a) (accumulate-b combiner null-value term (next a) next b))))


;; test

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))


(define (justify i)
    i)

(define (addone i)
    (+ i 1))

(check-expect (accumulate-a + 0 justify 1 addone 10) (sum justify 1 addone 10))
(check-expect (accumulate-a * 1 justify 1 addone 10) (product justify 1 addone 10))
(check-expect (accumulate-b + 0 justify 1 addone 10) (sum justify 1 addone 10))
(check-expect (accumulate-b * 1 justify 1 addone 10) (product justify 1 addone 10))
(check-expect (accumulate-a + 0 justify 1 addone 0) (sum justify 1 addone 0))
(check-expect (accumulate-a * 1 justify 1 addone 0) (product justify 1 addone 0))
(check-expect (accumulate-b + 0 justify 1 addone 0) (sum justify 1 addone 0))
(check-expect (accumulate-b * 1 justify 1 addone 0) (product justify 1 addone 0))
(test)