#lang racket

(define (adjoin-set x set)
  (if (or (null? set) (< x (car set)))
      (cons x set)
      (if (= x (car set))
          set
          (cons (car set) (adjoin-set x (cdr set))))))

(define a '(1 2 3 4 6 7 8))
(adjoin-set '5 a)
(adjoin-set '3 a)
(adjoin-set '3 '())

;; It's a sort of simple way to adjoin, we just need to check if the rest set is null or if the to be 
;; added element is smaller than the first element of set, if so, we just need to add it in front of
;; the set, otherwise, we need to go through the set, if there's already the same element in the set,
;; we just need to return the original set, if not, we add it before the position of the first element
;; is bigger than it. and in the worst case, we need to go throught the whole set to compare, and thus
;; the required steps is $\O(n)$
