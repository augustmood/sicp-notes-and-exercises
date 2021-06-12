#lang sicp
(#%require "interface.rkt")
(#%provide (all-defined))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  
  ;; ex2.79 below:```
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (or (= x y) (< (- x y) 0.0000001))))
  ;; ```ex2.79 above.

  ;; ex2.80 below:```
    (put '=zero? '(scheme-number)
        (lambda (x) (= x 0)))
  ;; ```ex2.80 above.

  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)