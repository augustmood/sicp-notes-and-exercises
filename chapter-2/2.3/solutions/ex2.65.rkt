#lang racket
(require (rename-in "ex2.62.rkt" [union-set union-list]))
(require "ex2.63.rkt")
(require "ex2.64.rkt")

(define (union-set set1 set2)
  (list->tree (union-list (tree->list-2 set1) (tree->list-2 set2))))
;; tree->list-2 takes O(n) union-list takes O(n) and list->tree takes O(n)
;; thus, the total works is around O(n)


(define/match (intersection-list list1 list2)
  [('() list2) '()]
  [(list1 '()) '()]
  [((cons x1 rest-list1) (cons x2 rest-list2))
   (cond ((< x1 x2) (intersection-list rest-list1 list2))
         ((> x1 x2) (intersection-list list1 rest-list2))
         (else (cons x1 (intersection-list rest-list1 rest-list2))))])

(define (intersection-set set1 set2)
  (list->tree (intersection-list (tree->list-2 set1) (tree->list-2 set2))))

;; tree->list-2 takes O(n) intersection-list takes O(n) and list->tree takes O(n)
;; thus, the total works is around O(n)