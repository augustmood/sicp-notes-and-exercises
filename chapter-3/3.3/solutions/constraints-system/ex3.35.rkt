#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(require "constraints.rkt")

; Ben Bitdiddle tells Louis that one way to avoid the trouble in exercise 3.34
; is to define a squarer as a new primitive constraint. Fill in the missing
; portions in Ben's outline for a procedure to implement such a constraint:

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (set-value! b (* (get-value a) (get-value a)) me)))
  (define (process-forget-value) 
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond [(eq? request 'I-have-a-value)
           (process-new-value)]
          [(eq? request 'I-lost-my-value)
           (process-forget-value)]
          [else
           (error "Unknown request -- ADDER" request)]))
  (connect a me)
  (connect b me)
  me)

(define A (make-connector))
(define B (make-connector))
(squarer A B)
(probe "A" A)
(probe "B" B)

(set-value! B 9 'user)
