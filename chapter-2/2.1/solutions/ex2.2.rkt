#lang racket

(define (make-segment start end)
    (cons start end))

(define (start-segment seg)
    (car seg))

(define (end-segment seg)
    (cdr seg))

(define (make-point x-point y-point)
    (cons x-point y-point))

(define (x-point point)
    (car point))

(define (y-point point)
    (cdr point))

(define (midpoint-segment seg)
    (let ((x (/ (+ (x-point (start-segment seg)) (x-point (end-segment seg))) 2.0))
            (y (/ (+ (y-point (start-segment seg)) (y-point (end-segment seg))) 2.0)))
        (make-point x y)))


(define seg1 (cons (cons 10 20) (cons 100 200)))

(midpoint-segment seg1)

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(print-point (midpoint-segment seg1))