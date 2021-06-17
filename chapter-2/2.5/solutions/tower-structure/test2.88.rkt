#lang sicp
(#%require "interface.rkt")
(#%require "operators.rkt")
(#%require "polynomial.rkt")
(#%require "arithmetic.rkt")

(install-arithmetic)
(install-polynomial-package)

(define p1 (cons 'x (attach-tag 'sparse '((100 1) (2 2) (0 1)))))
(define p2 (cons 'x (attach-tag 'sparse '((100 2) (2 2) (0 1)))))
(define p3 (cons 'x '((2 1) (1 2))))
(define p4 (make-polynomial 'x '(dense 1 2 0)))

p1
(cdr p2)
p3
(cdr p3)
(add p1 p2)

; (sub p1 p2)
; (sub p1 p3)
; (sub p2 p1)
; (sub p2 p3)
; (sub p3 p1)
; (sub p3 p2)

; (=zero? (sub p1 p1))
; (=zero? (sub p3 p3))