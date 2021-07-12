#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(require "wire.rkt")
(require "inverter.rkt")
(require "and-gate.rkt")

(define (or-gate a1 a2 output)
    (let ([i1-output (make-wire)]
          [i2-output (make-wire)]
          [and-output (make-wire)])
      (inverter a1 i1-output)
      (inverter a2 i2-output)
      (and-gate i1-output i2-output and-output)
      (inverter and-output output)))

;; or-gate delay = 2 * inverter delay + 1 * and-gate delay.
