#lang sicp
(#%require "interface.rkt")
(#%require "arithmetic.rkt") 
(install-arithmetic)

(define sn-1 (make-integer 10))
(define sn-2 9)
(define rtn-1 (make-rational 3 4))
(define rtn-2 (make-rational 20 4))
(define rln-1 (make-real 3.14))
(define rln-2 (make-real 2.5))
(define cn-1 (make-complex-from-real-imag 3 2))
(define cn-2 (make-complex-from-real-imag 15 7))

(define test-lst (list sn-1 sn-2 rtn-1 rtn-2 rln-1 rln-2 cn-1 cn-2))
(accumulate (lambda (x y) (add x y)) 0 test-lst)