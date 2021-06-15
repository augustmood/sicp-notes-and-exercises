#lang sicp
(#%require "interface.rkt")
(#%provide (all-defined))

(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))    
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (round (/ x y)))))
  
  ;; ex2.79 below:```
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  ;; ```ex2.79 above.
  
  ;; ex2.80 below:```
  (put '=zero? '(integer)
       (lambda (x) (= x 0)))
  ;; ```ex2.80 above.
  
  (put 'make 'integer
       (lambda (x) (tag (round x))))
  'done)