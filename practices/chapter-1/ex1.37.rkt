#lang racket
;; a

;; recursive process:

; (define (cont-frac n d k)
;   (define (cont-frac-helper i)
;     (if (< i k)
;         (/ (n i) (+ (d i) (cont-frac-helper (+ 1 i))))
;         (/ (n k) (d i))))
;   (cont-frac-helper 1))

;; interative process:

(define (cont-frac n d k)
  (define (cont-frac-helper i result)
    (if (< i k)
        (cont-frac-helper (+ i 1) (/ (n i) (+ result (d (- k i)))))
        result))
  (cont-frac-helper 0 0))



;; helpers
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; phi^2 = phi + 1
;; phi = 1 + 1/ phi

(define (golden-ratio x)
  (let ((x (* 1.0 x)))
    (define (average n m)
      (/ (+ m n) 2))
    (fixed-point (lambda (y) (average y (+ 1 (/ 1 y))))
                 x)))

;; for successive values of k

(define (golden-check x)
  (< (abs (- (golden-ratio x)
             (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) x))))
     tolerance))

(define (accurate-check i)
  (< (abs (- (golden-ratio i)
             (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) i))))
     tolerance))

(define (k-value i)
    (if (accurate-check i)
        i
        (k-value (+ i 1))))
        
(k-value 1)