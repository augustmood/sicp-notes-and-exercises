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

;; or

; (define (sub-interval x y)
;   (add-interval x
;                 (make-interval (- 0 (upper-bound y))
;                                (- 0 (lower-bound y)))))
                    