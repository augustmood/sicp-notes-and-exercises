#lang sicp
(#%require "polynomial.rkt")
(#%require "operators.rkt")
(#%require "arithmetic.rkt")

(install-arithmetic)
(install-polynomial-package)
(install-coercion)

(define p1 (make-polynomial 'x '(sparse (term 2 1) (term 1 -2) (term 0 1))))
(define p2 (make-polynomial 'x '(sparse (term 2 11) (term 0 7))))
(define p3 (make-polynomial 'x '(sparse (term 1 13) (term 0 5))))

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(mod-print q1)
(mod-print q2)
(mod-print (greatest-common-divisor q1 q2))
