#lang sicp
(#%require "stream.rkt")

(define (add-streams s1 s2)
  (general-stream-map + s1 s2))

(define (scale-stream stream factor)
  (general-stream-map (lambda (x) (* x factor)) stream))

(define (integral integrand initial-value dt)
  (cons-stream initial-value
               (let ([integrand (force integrand)])
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

