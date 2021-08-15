#lang sicp

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

; (define (factorial n)
;   (unless (= n 1)
;     (* n (factorial (- n 1)))
;     1))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 5)

;; the procedure will go into an infinite loop.

;; yes, our definitions will work in a normal-order language, since the 
;; nomral-order one will evalute argument only when the argument is required,
;; thus the (factorial 0) will not be calculated.
