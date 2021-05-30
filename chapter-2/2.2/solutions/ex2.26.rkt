#lang racket

(define x (list 1 2 3))
(define y (list 4 5 6))

(display "original:")
(append x y)
(display "expected result:")
(list 1 2 3 4 5 6)
(newline)
(display "original:")
(cons x y)
(display "expected result:")
(list (list 1 2 3) 4 5 6)
(newline)
(display "original:")
(list x y)
(display "expected result:")
(list (list 1 2 3) (list 4 5 6))