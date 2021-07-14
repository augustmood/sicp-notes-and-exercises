#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(require "constraints.rkt")

(define (averager a b c)
  (let ([result (make-connector)]
        [quantity (make-connector)])
    (adder a b result)
    (multiplier c quantity result)
    (constant 2 quantity)
    (void)))

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))
(averager A B C)

(probe "A" A)
(probe "B" B)
(probe "C" C)
(set-value! A 2 'user)
(set-value! B 8 'user)
