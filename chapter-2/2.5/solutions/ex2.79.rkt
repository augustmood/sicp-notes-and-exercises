#lang sicp
(#%require "arithmetic.rkt")
(install-arithmetic-system)

(define sn-1 (make-scheme-number 0.333333))
(define sn-2 (make-scheme-number 0.666666))
(define rat-1 (make-rational 1 3))
(define rat-2 (make-rational 2 3))
(define rat-3 (make-rational 0 3))
(define complex-1 (make-complex-from-mag-ang 0 0))
(define complex-2 (make-complex-from-real-imag 3 4))
(define complex-3 (make-complex-from-mag-ang 5 (atan (/ 4 3))))

(equ? sn-1 rat-1)
(equ? sn-2 rat-2)
(equ? (mul sn-1 2) sn-2)
(equ? 0 rat-3)
(equ? rat-3 complex-1)
(equ? complex-2 complex-3)