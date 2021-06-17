#lang sicp
(#%require "operators.rkt")
(#%require "polynomial.rkt")
(#%require "arithmetic.rkt")

(install-arithmetic)
(install-polynomial-package)

(define poly-1 (make-polynomial 'x '(sparse (term 6 2) (term 5 1) (term 4 2) (term 3 3))))
(define poly-2 (make-polynomial 'x 
                                '(sparse (term 5 2) (term 4 3) (term 3 1) 
                                         (term 2 5) (term 1 9) (term 0 10))))
(define poly-3 (make-polynomial 'x '(dense 2 1 2 3 0 0 0)))
(define poly-4 (make-polynomial 'x '(dense 2 3 1 5 9 7 9)))
(define poly-5 (make-polynomial 'x '(dense 2 3 1 5 9 10)))


(add poly-1 poly-1)
(add poly-2 poly-2)
(add poly-3 poly-3)
(add poly-4 poly-4)
(add poly-5 poly-5)

(add poly-1 poly-2)
(add poly-2 poly-3)
(add poly-3 poly-4)
(add poly-4 poly-5)

(sub poly-1 poly-1)
(sub poly-2 poly-2)
(sub poly-3 poly-3)
(sub poly-4 poly-4)
(sub poly-5 poly-5)

(sub poly-1 poly-2)
(sub poly-2 poly-3)
(sub poly-3 poly-4)
(sub poly-4 poly-5)

(mul poly-1 poly-1)
(mul poly-2 poly-2)
(mul poly-3 poly-3)
(mul poly-4 poly-4)
(mul poly-5 poly-5)

(mul poly-1 poly-2)
(mul poly-2 poly-3)
(mul poly-3 poly-4)
(mul poly-4 poly-5)