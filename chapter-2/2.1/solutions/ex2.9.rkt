#lang racket

(define (make-interval a b)
  (cons a b))

(define (lower-bound itvl)
  (car itvl))

(define (upper-bound itvl)
  (cdr itvl))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (width intvl)
  (* 0.5 (- (upper-bound intvl) (lower-bound intvl))))

;; for addition x & y
;; add-x-y <- (, (xL + yL) (xH + yH))
;; then the width of add-x-y is
;; width <- (* 0.5 (- (+ xH yH) (+ xL yL)))
;;       <- (* 0.5 (+ (- xH xL) (- yH yL)))
;;       <- (+ (width x) (width y))

;; for subtraction x & y
;; sub-x-y <- (, (xL - yH) (xH - yL))
;; then the width of sub-x-y is
;; width <- (* 0.5 (- (- xH yL) (- xL yH)))
;;       <- (* 0.5 (+ (- xH xL) (- yH yL)))
;;       <- (+ (width x) (width y))

;; Therefore, the width of the sum of the add/sub of two intervals is function of the sum of width of 
;; two intervals.

;; This is not true, we could simply tell by giving two interval (x [0, 100]) (y [0, 10])
;; the result of (mul x y) is [0, 1000], thus the width of the result of mul should be 1000, 
;; meanwhile, the sum of widths of the two interval is 1100 instead of 1000.