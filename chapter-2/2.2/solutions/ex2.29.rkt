#lang racket

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; a
(define (left-branch m)
  (car m))

(define (right-branch m)
  (car (cdr m)))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (car (cdr b)))

;; b

(define (total-weight m)
  (if (number? m)
      m
      (let ((left (branch-structure (left-branch m)))
            (right (branch-structure (right-branch m))))
        (+ (total-weight left)
           (total-weight right)))))

; (define (total-weight m)
;   (define (weight n)
;     (if (number? n)
;         n
;         (total-weight n)))
;   (let ((left (branch-structure (left-branch m)))
;         (right (branch-structure (right-branch m))))
;     (+ (weight left)
;        (weight right))))

(define example-1 (list (list 10 100) (list 20 (list (list 10 20) (list 20 40)))))
(total-weight example-1)



;; c
(define (branch-weight b)
  (let ((branch-s (branch-structure b)))
    (if (number? branch-s)
        branch-s
        (total-weight branch-s))))

(define (torque b)
  (* (branch-weight b)
     (branch-length b)))

(define (balanced? m)
  (let ((left (left-branch m))
        (right (right-branch m))
        (left-structure (branch-structure (left-branch m)))
        (right-structure (branch-structure (right-branch m))))
    (and (= (torque left) (torque right))
         (if (pair? left-structure)
             (balanced? left-structure)
             true)
         (if (pair? right-structure)
             (balanced? right-structure)
             true))))

(define ma (list (list 30 100) 
                (list 50 (list (list 20 20) (list 10 40)))))
(define mb (list (list 10 20) (list 40 5)))
(define mc (list (list 100 (list (list 20 60) (list 30 40))) (list 50 200)))
(define md (list (list 10 20) (list 40 20)))
(define me (list (list 100 (list (list 20 50) (list 30 40))) (list 50 200)))

(balanced? ma)
(balanced? mb)
(balanced? mc)
(balanced? md)
(balanced? me)


;; d
; Suppose we change the representation of mobiles so that the constructors are

; (define (make-mobile left right)
;   (cons left right))
; (define (make-branch length structure)
;   (cons length structure))

; How much do you need to change your programs to convert to the new representation?


;; I only need to change the selectors:

; (define (left-branch m)
;   (car m))

; (define (right-branch m)
;   (cdr m))

; (define (branch-length b)
;   (car b))

; (define (branch-structure b)
;   (cdr b))