#lang racket
(provide (all-defined-out))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cddr frame))

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

(define (make-segment start-point end-point)
  (list start-point end-point))

(define start-segment car)

(define end-segment cadr)