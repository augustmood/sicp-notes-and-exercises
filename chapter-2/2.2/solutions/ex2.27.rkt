#lang sicp

(define (deep-reverse lst)
  (cond ((null? lst) nil)
        ((not (pair? lst)) lst)
        (else (append (deep-reverse (cdr lst)) (cons (deep-reverse (car lst)) nil)))))

(define x (list (list 1 2) (list 3 4)))
(define list-a (list 1 3 (list 5 7) 9))
(define list-b (list (list 7)))
(define list-c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(deep-reverse x)
(deep-reverse list-a)
(deep-reverse list-b)
(deep-reverse list-c)