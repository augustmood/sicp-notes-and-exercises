#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)

(define x '((a b) c d))
(define y '(e f))
x
y
(begin (set-car! x y) x) ;; ((e f) c d)
(begin (set-cdr! x y) x) ;; ((e f) e f)

(define (get-new-pair)
  (cons '() '()))

(define (new-cons x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))

(define a (new-cons 1 1))
(define b (new-cons 1 1))
a ; (1 . 1)
(set-cdr! a 2)
a ; (1 . 2)
b ; (1 . 1)

(define new-x (list 'a 'b))
(define z1 (cons new-x new-x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))
z1
z2
(eq? (car z1) (cdr z1))
(eq? (car z2) (cdr z2))
