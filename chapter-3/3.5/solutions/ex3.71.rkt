#lang sicp
(#%require "stream.rkt")
(#%require "ex3.70.rkt")

(define weight-fn-3 (lambda (i) (+ (expt (car i) 3) (expt (cadr i) 3))))
(define ramanujan-stream (weighted-pairs integers integers weight-fn-3))

