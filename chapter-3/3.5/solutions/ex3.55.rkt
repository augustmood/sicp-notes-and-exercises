#lang sicp
(#%require "stream.rkt")

; Define a procedure partial-sums that takes as argument a stream S and returns 
; the stream whose elements are S0, S0 + S1, S0 + S1 + S2, .... For example, 
; (partial-sums integers) should be the stream 1, 3, 6, 10, 15, ....

(define (partial-sums s)
  (let ([sum 0])
    (stream-map (lambda (i) (set! sum (+ i sum)) sum) s)))

;; test part

(define example (partial-sums integers))

(stream-test example 10)
