#lang racket

(define (make-product a b)
    (cons a b))

(define (a product)
    (car product))

(define (b product)
    (cdr product))

(define (result product)
    (* (pow 2 (a product))
    (pow 3 (b product))))

(define (pow x y)
    (if (= x 0)
        1
        (* y (pow (- x 1) y))))

(result (make-product 2 3))