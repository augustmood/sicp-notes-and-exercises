#lang sicp
(#%require "stream.rkt")
(#%require "ex3.59.rkt")
(#%provide (all-defined))

(define (add-streams s1 s2)
  (general-stream-map + s1 s2))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))
