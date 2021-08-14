#lang sicp

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)

;; 10! = 3628800

;; a

((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (fi k)
      (if (<= k 2)
          1
          (+ (fi fi (- k 2)) (fi fi (- k 1)))))))
 10)

;; (fib 10) = 55

;; b 
;; Consider the following procedure, which includes mutually recursive internal 
;; definitions:

; (define (f x)
;   (define (even? n)
;     (if (= n 0)
;         true
;         (odd? (- n 1))))
;   (define (odd? n)
;     (if (= n 0)
;         false
;         (even? (- n 1))))
;   (even? x))

;; Fill in the missing expressions to complete an alternative definition of f, 
;; which uses neither internal definitions nor letrec:

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(f 11) ;; #f
(f 0)  ;; #t
(f 10) ;; #t
