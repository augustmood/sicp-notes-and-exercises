#lang sicp
(#%require "stream.rkt")

(define (sqrt-improve guess x)
  ((lambda (x y) (/ (+ x y) 2)) guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (stream-limit s tolerance)
  (let ([first (stream-ref s 0)]
        [second (stream-ref s 1)])
    (if (< (abs (- first second)) tolerance)
        second
        (stream-limit (stream-cdr s) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

; (sqrt 2 0.0000000000001)
