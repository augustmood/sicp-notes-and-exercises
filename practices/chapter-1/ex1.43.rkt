#lang racket

(define (compose f g)
  (lambda (i) (f (g i))))

(define (repeated f n)
    (if (< n 2)
        f
        (compose f (repeated f (- n 1)))))

(define (square i)
  (* i i))

((repeated square 2) 5)

