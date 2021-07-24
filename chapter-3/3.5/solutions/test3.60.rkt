#lang sicp
(#%require "stream.rkt")
(#%require "ex3.59.rkt")
(#%require "ex3.60.rkt")

(define test (add-streams (mul-series sine-series sine-series)
                          (mul-series cosine-series cosine-series)))

(show-stream test 10)