#lang racket

(define (cont-frac n d k)
  (define (cont-frac-helper i result)
    (if (< i k)
        (cont-frac-helper (+ i 1) (/ (n i) (+ result (d (- k i)))))
        result))
  (cont-frac-helper 0 0))

(+ 2 (cont-frac (lambda (i) 1.0)
                (lambda (i) (if (= (remainder (- i 2) 3) 0)
                                (+ 2 (* 2.0 (/ (- i 2) 3)))
                                1.0)) 100))