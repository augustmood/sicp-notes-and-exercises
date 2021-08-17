#lang sicp

(define a (cons b c))

(define b (cons 1 '()))

(define c (cons b nil))

;; the laizer intepreter, we don't need to worry abt the order of the 
;; definitons.
