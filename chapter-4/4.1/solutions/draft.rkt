#lang sicp

(define vals '(1))

vals
(if (null? (cdr vals))
    (set! vals nil)
    (begin (set-car! vals (cadr vals))
           (set-cdr! vals (cddr vals))))
vals
