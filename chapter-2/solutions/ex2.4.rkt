#lang racket

; Here is an alternative procedural representation of pairs. For this representation, 
; verify that (car (cons x y)) yields x for any objects x and y.

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

; (car (cons x y))

; ((cons x y) (lambda (p q) p))

; ((lambda (m) (m x y)) (lambda (p q) p))

; ((lambda (p q) p) x y)

; x

; What is the corresponding definition of cdr?

(define (cdr z)
    (z (lambda (p q) q)))

; (cdr (cons x y))

; ((cons x y) (lambda (p q) q))

; ((lambda (m) (m x y)) (lambda (p q) q))

; ((lambda (p q) q) x y)

; y