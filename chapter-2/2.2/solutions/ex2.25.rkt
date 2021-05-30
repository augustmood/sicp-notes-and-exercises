#lang sicp

(define list-a (list 1 3 (list 5 7) 9))

(define list-b (list (list 7)))

(define list-c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;; Lists:
list-a
list-b
list-c

(newline)

(car (cdr (car (cdr (cdr list-a)))))
(car (car list-b))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list-c))))))))))))
