#lang sicp
(#%require "stream.rkt")
(#%require "ex3.60.rkt")
(#%require "ex3.61.rkt")
(#%provide (all-defined))

(define (div-series s1 s2)
  (if (= (stream-car s2) 0) 
      (error "Denominator cannot have zero constant term!"))
  (mul-series s1 (invert-unit-series s2)))


