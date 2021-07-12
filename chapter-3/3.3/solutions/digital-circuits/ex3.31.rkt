#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(require "wire.rkt")
(require "basic-gates.rkt")

;; codes for testing:

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(probe 'sum sum)
(probe 'carry carry)
(half-adder input-1 input-2 sum carry)
(or-gate input-1 input-2 sum)
(set-signal! input-1 1)
(propagate)
