#lang sicp
(#%require "operators.rkt")
(#%require "polynomial.rkt")
(#%require "arithmetic.rkt")

(install-arithmetic)
(install-polynomial-package)

(define p1 (make-polynomial 'x (list (list 100 1) (list 2 2) (list 0 1))))
(define p2 (make-polynomial 'x '((0 0) (0 0) (0 0))))
(define p3 (make-polynomial 'x '((2 1) (1 2))))

(=zero? p1)
(=zero? p2)
(=zero? p3)
