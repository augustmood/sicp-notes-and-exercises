#lang sicp

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (i) (cons (car s) i)) rest)))))

(subsets (list 1 2 3))

;; > (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

;; Why `(lambda (i) (cons (car s) i))` works?

;; The main idea of this program is to recurse the given set until the second element is the set of 
;; the empty list (that is, the last element of the initial list) and the first subset we get is the 
;; empty set, at which point all we have to do is recurse backwards, keeping the subset we got before,
;; while copying all the subsets we got, adding the first element of the pair of subsets we got by 
;; recursion to the result sef of subsets copy, and so on until the subset containing the first 
;; element of the initial set is added to our result set of subsets. And what we do of completing the 
;; given in-complete procedure is to implement the function that adds the first element of the most 
;; reachable pair we get by recursion back to a copy of the previous result set of subsets.

;; (I may modify my wording later...)