#lang racket

(define (compose f g)
    (lambda (i) (f (g i))))

(define (square i)
    (* i i))

(define (inc i)
    (+ i 1))

((compose square inc) 6)