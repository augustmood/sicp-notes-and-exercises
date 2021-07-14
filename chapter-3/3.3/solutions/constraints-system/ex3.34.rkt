#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(require "constraints.rkt")

; Louis Reasoner wants to build a squarer, a constraint device with two 
; terminals such that the value of connector b on the second terminal will 
; always be the square of the value a on the first terminal. He proposes the 
; following simple device made from a multiplier:

(define (squarer a b)
  (multiplier a a b))

; There is a serious flaw in this idea. Explain.

(define A (make-connector))
(define B (make-connector))
(squarer A B)
(probe "A" A)
(probe "B" B)

(set-value! B 9 'user)

;; this will not produce the value of A.
;; since in the implementation of multiplier, we are always need to check if two
;; of the three arguments of the multiplier have value. and Louis's idea only 
;; gives one argument when it comes to only the B's value is given.
