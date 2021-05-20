#lang racket
(require test-engine/racket-tests)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; phi^2 = phi + 1
;; phi = 1 + 1/ phi

(define (golden-ratio x)
    (define (average n m)
        (/ (+ m n) 2))
    (fixed-point (lambda (y) (average y (+ 1 (/ 1 y))))
    x))

;; test
(define (golden-calc x)
    (+ 1 (/ 1.0 x)))

(define (golden-check x)
    (< (abs (- (golden-ratio x)
               (golden-calc (golden-ratio x))))
        tolerance))

(check-expect (golden-check 1.0) #t)
(check-expect (golden-check 100.0) #t)
(check-expect (golden-check 0.5) #t)
(test)