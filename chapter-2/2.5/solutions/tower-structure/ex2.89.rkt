#lang sicp
(#%require "interface.rkt")
(#%require "operators.rkt")
(#%require "arithmetic.rkt")
(install-arithmetic)
(#%provide (all-defined))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (dense-representation lst)
  (let ((length (order (first-term lst))))
    (define (iter n lst result)
      (if (< n 0)
          result
          (if (= n (order (first-term lst)))
              (iter (- n 1) (rest-terms lst) (cons (coeff (first-term lst)) result))
              (iter (- n 1) lst (cons 0 result)))))
    (reverse (iter length lst nil))))

(define tld-a (dense-representation '((100 2) (4 9) (2 2) (0 1))))


(define (first-term-dense lst)
    (list (- (length lst) 1) (car lst)))

; (first-term-dense tld-a)

(define (adjoin-term-dense term term-list)
    (let ((diff (- (order term) (order (first-term-dense term-list)))))
        (define (iter n result)
            (if (= n 1)
                result
                (iter (- n 1) (cons 0 result))))
        (cons (coeff term) (iter diff term-list))))

; (adjoin-term-dense '(102 2) tld-a)
tld-a