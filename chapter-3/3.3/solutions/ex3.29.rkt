#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)

(define (or-gate a1 a2 output)
  (inverter 
   (and-gate 
    (inverter a1 output) 
    (inverter a2 output) output) output))
