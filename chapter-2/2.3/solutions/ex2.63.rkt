#lang racket
(require "tree.rkt")
(provide (all-defined-out))
;;  Each of the following two procedures converts a binary tree to a list.
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))



(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))



;; a

;; yes, both of two procedures produce the same result for every tree.

;; b

;; *`append` is O(n), while `cons` is O(1).*
;; Thus, for the first procedure:
;; T(n) = 2 * T(n/2) + O(n) = O(n * logn)
;; Also, for the second procedure:
;; T(n) = 2 * T(n/2) + O(1) = O(n)
;; Thus, the first one grows slower.



;; Examples:
(define tree-a 
  (make-tree 1 nil
             (make-tree 2 nil 
                        (make-tree 3 nil 
                                   (make-tree 4 nil 
                                              (make-tree 5 nil 
                                                         (make-tree 6 nil 
                                                                    (make-tree 7 
                                                                               nil 
                                                                               nil))))))))
(define tree-b
  (make-tree 5 (make-tree 3 (make-tree 2 nil nil) (make-tree 4 nil nil))
             (make-tree 8 (make-tree 6 nil nil) (make-tree 10 nil nil))))

(define tree-c
  (make-tree 10 
             (make-tree 9 
                        (make-tree 8 
                                   (make-tree 7 
                                              (make-tree 6 nil nil) nil) nil) nil) nil))

(define tree-d
  (make-tree 5 nil nil))

; (tree->list-1 tree-a)
; (tree->list-2 tree-a)
; (tree->list-1 tree-b)
; (tree->list-2 tree-b)
; (tree->list-1 tree-c)
; (tree->list-2 tree-c)
; (tree->list-1 tree-d)
; (tree->list-2 tree-d)