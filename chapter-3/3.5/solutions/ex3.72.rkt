#lang sicp
(#%require "stream.rkt")
(#%require "ex3.70.rkt")

(define weight-fn-4 (lambda (i) (+ (expt (car i) 2) (expt (cadr i) 2))))
(define sq-sum-stream (weighted-pairs integers integers weight-fn-4))

(define (find-sum-in-3-way seq weight)
  (define (iter seq prev-stored)
    (let ([scar (stream-car seq)]
          [scadr (stream-car (stream-cdr seq))]
          [scaddr (stream-car (stream-cdr (stream-cdr seq)))])
      (let ([weighted-scar (weight scar)]
            [weighted-scadr (weight scadr)]
            [weighted-scaddr (weight scaddr)])
        (if (and (= weighted-scar weighted-scadr)
                 (not (eq? weighted-scar prev-stored)))
            (cons-stream (list weighted-scar scar scaddr scaddr)
                         (iter 
                          (stream-cdr (stream-cdr (stream-cdr seq)))
                          weighted-scar))
            (iter (stream-cdr seq) prev-stored)))))
  (iter seq 0))

(define sq-sum (find-sum-in-3-way sq-sum-stream weight-fn-4))

;; test

(show-stream sq-sum 10)
