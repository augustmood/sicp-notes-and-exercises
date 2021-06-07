#lang racket

(define (adjoin-set x set)
  (if (or (null? set) (< x (car set)))
      (cons x set)
      (if (= x (car set))
          set
          (cons (car set) (adjoin-set x (cdr set))))))

; (define (union-set set1 set2)
;     (if (null? set1)
;         set2
;         (union-set (cdr set1) (adjoin-set (car set1) set2))))

;; uhmmmm, this seems work in \Theta(n^2) time.

; (define (union-set set1 set2)
;   (cond ((null? set1) set2)
;         ((null? set2) set1)
;         (else 
;          (let ((x1 (car set1)) (x2 (car set2)))
;            (cond ((= x1 x2)
;                   (cons x1 (union-set (cdr set1)
;                                       (cdr set2))))
;                  ((< x1 x2)
;                   (cons x1 (union-set (cdr set1) set2)))
;                  ((< x2 x1)
;                   (cons x2 (union-set set1 (cdr set2)))))))))

;; This one is similar to the given procedure `intersection-set` and the required step is $\O(n)$



;; There's a cool way to implement this by using racket match

(define/match (union-set set1 set2)
  [('() set2) set2]
  [(set1 '()) set1]
  [((cons x1 rest-set1) (cons x2 rest-set2))
   (cons (min x1 x2) (if (< x1 x2) (union-set rest-set1 set2) (union-set rest-set2 set1)))])

;; Almost the same as the preivous $\O(n)$ one, just looks more pretty.


(union-set '(1 2 3 4) '(3 4 5 6 7 8))
(union-set '(1 2 3 4 5 6 7) '(3 4 5 10))
(union-set '(1 2 3 4 5 6 7) '())
(union-set '() '(1 2 3 4 5))
(union-set '() '())