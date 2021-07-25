#lang sicp
(#%require "stream.rkt")

(define (add-streams s1 s2)
  (general-stream-map + s1 s2))

(define (scale-stream stream factor)
  (general-stream-map (lambda (x) (* x factor)) stream))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC r c dt)
    (lambda (s v0)
      (add-streams (scale-stream s r)
              (integral s v0 dt))))
