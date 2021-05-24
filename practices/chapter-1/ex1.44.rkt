#lang racket

(define (compose f g)
  (lambda (i) (f (g i))))

(define (repeated f n)
  (if (< n 2)
      f
      (compose f (repeated f (- n 1)))))

(define dx 0.00001)

(define (square i)
  (* i i))

(define (smooth f)
  (lambda (i) 
    (/ (+ (f (- i dx)) (f i) (f (+ i dx))) 
       3)))

(define (n-fold-smooth f n)
    ((repeated smooth n) f))