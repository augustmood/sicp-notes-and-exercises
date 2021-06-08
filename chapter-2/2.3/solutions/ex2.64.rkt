#lang racket
(require "tree.rkt")
(provide (all-defined-out))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))   ;; left-size
        (let ((left-result (partial-tree elts left-size))) ;; left-result
          (let ((left-tree (car left-result)) ;; left-tree
                (non-left-elts (cdr left-result))  ;;subtree stuff?
                (right-size (- n (+ left-size 1)))) ;; right-size
            (let ((this-entry (car non-left-elts))   
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; a

;; partial-tree will first call itself on the first half of the given list, using the the first 
;; element of the rest list as the node of the tree, and call itself on the rest of the list, when 
;; the given size of the length of the list that need to be evaluated equal to 0, it will return an
;; empty list, and finally it will call make-tree to combine the left subtree which is evaluated from 
;; the first half part of the list, the node which is the element right after the first half part of 
;; the given list, and the right subtree which is evaluated from the second half part of the list.



; (list->tree '(1 3 5 7 9 11))

;;          5
;;         / \
;;        1   9
;;        \   /\
;;         3 7  11

;; '((5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))) <-
;; (partial-tree '(1 3 5 7 9 11) 6) <-
;; (cons (make-tree (car '(5 7 9 11)) (make-tree 1 '() (make-tree 3 '() '()))
;;             (car (partial-tree '(7 9 11) 3))) '()) <-
;; <-   (cons (make-tree (car '(5 7 9 11)) (make-tree 1 '() (make-tree 3 '() '()))
;;                          (make-tree 9 '(7 '() '()) '(11 '() '())))'())
;; (partial-tree '(1 3 5 7 9 11) 2) <- (make-tree 1 '() (car (partial-tree '(3 5 7 9 11) 1)))
;; <- (cons (make-tree 1 '() (make-tree 3 '() '())) '(5 7 9 11))

;; b
;; T(n) = 2 * T(n/2) + O(1)
;; Thus required steps is around O(n)