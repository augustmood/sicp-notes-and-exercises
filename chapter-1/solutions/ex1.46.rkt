#lang racket

(define (square i)
  (* i i))

(define (average x y)
  (/ (+ x y) 2))

;; iterative-improve:
(define (iterative-improve f g)
  (lambda (i) (if (f i)
                  i
                  ((iterative-improve f g) (g i)))))



;; rewrtie sqrt:
(define (sqrt x)
  (define (good-enough? x)
    (lambda (i) (< (abs (- (square i) x)) 0.001)))
  (define (improve x)
    (lambda (i) (average i (/ x i))))
  ((iterative-improve (good-enough? x) (improve x)) 1.0))



;; rewrite fixed-point:
(define (fixed-point f first-guess)
  (let ((tolerance 0.00001))
    (define (close-enough? f)
      (lambda (i) (< (abs (- i (f i))) tolerance)))
    ((iterative-improve (close-enough? f) f) first-guess)))