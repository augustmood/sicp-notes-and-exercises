#lang racket
(require racket/trace)

; a
(define (product-re term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-re term (next a) next b))))

; b
(define (product-iter term a next b)
  (define (iter a b next term result)
    (if (> a b)
        result
        (iter (next a) b next term (* result (term a)))))
  (iter a b next term 1))

;; helpers

(define (addone i)
  (+ i 1))

(define (term-above i)
  (* 2 (+ (floor (/ i 2)) 1)))

(define (term-below i)
  (+ 3 
     (* 2 
        (- (ceiling (/ i 2)) 1))))

(define (approx-pi n func)
  (* 4 (/ (func term-above 1.0 addone n) (func term-below 1.0 addone n)))
  )

(approx-pi 150 product-iter)
(approx-pi 150 product-re)

;; imrpoved term
(define (imporved-term i)
  (/ (* 2 (+ (floor (/ i 2)) 1))
     (+ 3 (* 2 (- (ceiling (/ i 2)) 1)))))

(define (improved-approx-pi n func)
  (* 4 (func imporved-term 1.0 addone n)))

(improved-approx-pi 1000 product-iter)