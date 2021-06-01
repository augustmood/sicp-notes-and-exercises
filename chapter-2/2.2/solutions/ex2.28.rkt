#lang sicp

(define (fringe lst)
  (cond ((null? lst) nil)
        ((not (pair? lst)) (list lst))
        (else (append (fringe (car lst)) 
                      (fringe (cdr lst)))
              )))

(define x (list (list 1 2) (list 3 4)))
(define list-a (list 1 3 (list 5 7) 9))
(define list-b (list (list 7)))
(define list-c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(fringe x)
(fringe (list x x))
(fringe list-a)
(fringe list-b)
(fringe list-c)