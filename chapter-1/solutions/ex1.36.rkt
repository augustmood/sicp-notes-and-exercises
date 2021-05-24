#lang racket

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (log-related x)
    (define (average n m)
        (/ (+ m n) 2))
    (let ((x (if (= x 1)
                (+ x 1)
                x)))
    (fixed-point (lambda (y) (average y (/ (log 1000) (log y)))) x)))

(log-related 1)