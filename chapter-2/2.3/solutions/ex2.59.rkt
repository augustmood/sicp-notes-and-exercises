#lang racket
(provide (all-defined-out))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; (define (union-set set1 set2)
;   (cond ((or (null? set1) (null? set2)) set2)
;         ((not (element-of-set? (car set1) set2))     
;          (cons (car set1)
;                (union-set (cdr set1) set2)))
;         (else (union-set (cdr set1) set2))))

(define (union-set set1 set2)
    (append set1 (filter (lambda (i) (not (element-of-set? i set1))) set2)))

;; both worked, and the work of both of them are O(n^2)

;; test

(define a '(1 2 3 4 5 6 10 12))
(define b '(1 2 3 4 7 8 9 0))
(union-set a b)