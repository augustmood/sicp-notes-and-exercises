#lang racket
(require racket/trace)
(require test-engine/racket-tests)

(define (even? n)
  (= (remainder n 2) 0))

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

;; test

(check-expect (fast-expt 2 0) 1)
(check-expect (fast-expt 3 0) 1)
(check-expect (fast-expt 3 1) 3)
(check-expect (fast-expt 3 3) 27)
(check-expect (fast-expt 2 1) 2)
(check-expect (fast-expt 2 8) 256)
(check-expect (fast-expt 2 9) 512)
(check-expect (fast-expt 2 10) 1024)
(test)

;; thanks to racket's trace feature, the visualization of fast-expt-iter is avaliable.

; (trace fast-expt-iter)
; (fast-expt-iter 2 10 1 2)