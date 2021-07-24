#lang sicp
(#%require "stream.rkt")
(#%require "ex3.59.rkt")
(#%require "ex3.60.rkt")
(#%provide (all-defined))

(define ones (cons-stream 1 ones))

(define (invert-unit-series s)
  (cons-stream 1 
               (scale-stream 
                (mul-series (stream-cdr s) (invert-unit-series s)) 
                -1)))
