#lang sicp
(#%require "interface.rkt")
(#%require "operators.rkt")
(#%require "arithmetic.rkt")
(#%require "polynomial.rkt")
(install-arithmetic)


(define (install-sparse-package)
  (define (adjoin-term term term-list)
    (if (=zero? (cadr term))
        term-list
        (cons term term-list)))
  (define (first-term term-list) (car term-list))
  (put 'adjoin-term 'sparse adjoin-term)
  (put 'first-term 'sparse first-term)
  'done)

(define (install-dense-package)
  (define (adjoin-term term term-list)
    (let ((diff (- (car term) (car (first-term term-list)))))
      (define (iter n result)
        (if (= n 1)
            result
            (iter (- n 1) (cons 0 result))))
      (cons (cadr term) (iter diff term-list))))
  (define (first-term lst)
    (list (- (length lst) 1) (car lst)))
  (put 'adjoin-term 'dense adjoin-term)
  (put 'first-term 'dense first-term)
  'done)

(define (adjoin-term term term-list)
  (if (null? term-list)
      (cons term term-list)
      ((get 'adjoin-term (if (pair? (car term-list)) 'sparse 'dense)) term term-list)))

(define (first-term term-list)
    ((get 'first-term (if (pair? (car term-list)) 'sparse 'dense)) term-list))

(install-sparse-package)
(install-dense-package)









(define lst-a '((3 1) (1 2)))
(define lst-b '(2 4 5 7 1 3 5 3 5 1))
lst-a
lst-b
(first-term lst-a)
(adjoin-term '(5 1) lst-a)
(first-term lst-b)
(adjoin-term '(12 5) lst-b)

(install-polynomial-package)
(define p1 (make-polynomial 'x '((4 1) (2 2) (0 1))))
(define p2 (make-polynomial 'x '(1 0 2 0 1)))
(define p3 (make-polynomial 'x '((2 1) (1 2))))