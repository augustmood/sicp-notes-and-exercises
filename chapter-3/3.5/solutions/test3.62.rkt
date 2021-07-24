#lang sicp
(#%require "stream.rkt")
(#%require "ex3.59.rkt")
(#%require "ex3.62.rkt")

(define s-1 (div-series sine-series cosine-series))

(show-stream s-1 10) ;; tangent power series

(define s-2 (div-series sine-series (cons-stream 0 cosine-series)))

(show-stream s-2 10) ;; error
