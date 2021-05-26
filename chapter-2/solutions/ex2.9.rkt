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
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

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

;; The width of the interval which is the result of the mul or div, might be any two of 
;; pairs p1, p2, p3, p4 mentioned above