#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(require "wire.rkt")
(provide (all-defined-out))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond [(= (+ s1 s2) 2) 1]
        [(< (+ s1 s2) 2) 0]
        (else (error "Invalid signal" s1 s2))))
