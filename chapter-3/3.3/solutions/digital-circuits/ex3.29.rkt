#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(require "wire.rkt")
(require "inverter.rkt")
(require "and-gate.rkt")

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ([i1-output (make-wire)]
          [i2-output (make-wire)]
          [and-output (make-wire)])
      (inverter a1 i1-output)
      (inverter a2 i2-output)
      (and-gate i1-output i2-output and-output)
      (inverter and-output output)))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure))

;; or-gate delay = 2 inverter delay + and-gate-delay.

