#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (fringe lst)
  (cond ((null? lst) nil)
        ((not (pair? lst)) (list lst))
        (else (append (fringe (car lst)) 
                      (fringe (cdr lst)))
              )))

; (define (count-leaves t)
;   (accumulate (lambda (x y) (+ 1 y)) 0 (map (lambda (i) i) (fringe t))))

(define (count-leaves t)
  (accumulate (lambda (x y) (if (pair? x) (+ (count-leaves x) y) (+ 1 y))) 0 t))

(define x (cons (list 1 2) (list 3 4)))

(list x x)

(fringe (list x x))

(count-leaves (list x x))