#lang racket

;; element-of-set? could still be the same. O(n)
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; adjoin-set could directly add element in the set without considering if there're duplicates. O(1)
(define (adjoin-set x set)
    (cons x set))

;; union-set O(1)
(define (union-set set1 set2)
    (append set1 set2))

;; intersection-set: 

; (define (intersection-set set1 set2)
;   (cond ((or (null? set1) (null? set2)) '())
;         ((element-of-set? (car set1) set2)        
;          (cons (car set1)
;                (intersection-set (cdr set1) set2)))
;         (else (intersection-set (cdr set1) set2))))

(define (eliminate-duplicates set)
    (define (iter set re-set)
        (if (null? set)
        '()
        (cons (car set) (iter (filter (lambda (i) (not (eq? (car set) i))) (cdr set)) re-set))))
    (iter set '()))

(define (intersection set1 set2)
    (append set1 (filter (lambda (i) (element-of-set? i (eliminate-duplicates set1))) set2)))

(intersection '(1 1 2 2 3 3 4 4) '(3 3 4 4 5 5 6 6))

;; still O(n^2), might not be an improvement though