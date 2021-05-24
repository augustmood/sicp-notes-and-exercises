#lang racket

(define (average m n)
  (/ (+ m n) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point f first-guess)
  (let ((tolerance 0.00001))
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) tolerance))
    (define (try guess)
      (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess)))

(define (repeated f n)
  (define (compose f g)
    (lambda (i) (f (g i))))
  (if (< n 2)
      f
      (compose f (repeated f (- n 1)))))

;;;;;;;;;

(define (exp n)
  (lambda (i) (fast-expt i n)))

(define (fast-expt b n)
  (if (= n 0) 
      1
      (fast-expt-iter n 1 b)))

(define (fast-expt-iter counter product-a product-b)
  (define (square i)
    (* i i))
  (cond ((= counter 1) (* product-a product-b))
        ((even? counter) (fast-expt-iter (/ counter 2) product-a (square product-b)))
        (else (fast-expt-iter (- counter 1) (* product-a product-b) product-b))))

(define (nth-root x n)
  (fixed-point 
   ((repeated average-damp n)
    (lambda (y) 
      (/ x (fast-expt y (- n 1)))))
   1.0))

(nth-root 27 3)
(nth-root 35 4)

;; I've seen some guys use (floor (log 2 n)) as the parameter of the repeated n variable, I quitely 
;; didn't understand. I am gonna skip this part and headed back here later days...