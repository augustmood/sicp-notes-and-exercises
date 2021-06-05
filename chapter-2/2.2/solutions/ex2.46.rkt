#lang sicp

(define make-vect cons)

(define xcor-vect car)

(define ycor-vect cdr)

(define (add/sub-vect f vect-1 vect-2)
  (make-vect (f (xcor-vect vect-1) (xcor-vect vect-2))
             (f (xcor-vect vect-1) (xcor-vect vect-2))))

(define (add-vect vect-1 vect-2) (add/sub-vect + vect-1 vect-2))

(define (sub-vect vect-1 vect-2) (add/sub-vect - vect-1 vect-2))

(define (scale-vect scalar vect)
  (make-vect (* scalar (xcor-vect vect)) (* scalar (ycor-vect vect))))

(define a (make-vect 3 4))
(define b (make-vect 5 6))

(add-vect a b)
(sub-vect b a)
(scale-vect 2 a)