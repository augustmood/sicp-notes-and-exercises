#lang sicp
(#%require "stream.rkt")
(#%require "acc.rkt")

(define (stream-limit s tolerance)
  (let ([first (stream-car s)]
        [second (stream-car (stream-cdr s))])
    (if (< (abs (- first second)) tolerance)
        second
        (stream-limit (stream-cdr s) tolerance))))

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream (partial-sums (ln2-summands 1)))

(show-stream ln2-stream 10)

(show-stream (euler-transform ln2-stream) 10)

(show-stream (accelerated-sequence euler-transform
                                      ln2-stream) 10)
