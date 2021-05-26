#lang racket

(define (make-interval a b)
    (cons (min a b) (max a b)))

(define (lower-bound itvl)
    (car itvl))

(define (upper-bound itvl)
  (cdr itvl))


