#lang racket
(require test-engine/racket-tests)

; (define (tan-cf x k)
;     (define (tan-cf-helper i)
;         (cond ((= i 1) (/ x (- () (tan-cf-helper (+ i 1)))))
;                 ((= i k) (/ (* x x) () )))))

(define (cont-frac n d k)
  (define (cont-frac-helper i)
    (if (< i k)
        (/ (n i) (- (d i) (cont-frac-helper (+ 1 i))))
        (/ (n k) (d i))))
  (cont-frac-helper 1))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (* x x))) 
             (lambda (i) (- (* 2.0 i) 1))
             k))

(define (verifier n)
  (if (< n 1)
      true
      (and (< (abs (- (tan-cf n 100) (tan n))) 0.0001)
           (verifier (- n 1)))))

(check-expect (verifier 10) #t)
(test)