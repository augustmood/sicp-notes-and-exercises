#lang sicp

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))



;; one

(add-1 zero)

(lambda (f) (lambda (x) (f ((zero f) x))))

(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))

(lambda (f) (lambda (x) (f ((lambda (x) x) x))))

(lambda (f) (lambda (x) (f x)))


;; Therefore, one shoule be:

(define one (lambda (f) (lambda (x) (f x))))



;; two

(add-1 one)

(lambda (f) (lambda (x) (f ((one f) x))))

(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))

(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))

(lambda (f) (lambda (x) (f (f x))))


;; Therefore, two should be:

(define two (lambda (f) (lambda (x) (f (f x)))))





;; Church numerals
;; 0 f x = x
;; 1 f x = f x
;; 2 f x = f f x
;; 3 f x = f f f x
;; ...
;; n f x = f^n x