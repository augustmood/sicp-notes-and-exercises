#lang sicp
(#%require "interface.rkt")
(#%require "operators.rkt")
(#%require "polynomial.rkt")
(#%require "arithmetic.rkt")
(#%require "tower-system.rkt")

(install-arithmetic)
(install-polynomial-package)

(define (make-poly variable term-list)
  (cons variable term-list))
(define (variable p) (car p))
(define (term-list p) (cdr p))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (first-term term-list)
  (apply-generic 'first-term term-list))

(define (rest-terms term-list)
  (apply-generic 'rest-terms term-list))

(define (empty-termlist? term-list) 
  (= (length term-list) 1))

(define (make-term order coeff) 
  (attach-tag 'term (list order coeff)))
(define (order term) (car (contents term)))
(define (coeff term) (cadr (contents term)))

(define (adjoin-term term term-list)
  (apply-generic 'adjoin-term term term-list))

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) (map (lambda (i) (if (and (pair? i) (eq? (type-tag i) 'sparse)) (contents i) i)) L2))
        ((empty-termlist? L2) (map (lambda (i) (if (and (pair? i) (eq? (type-tag i) 'sparse)) (contents i) i)) L1))
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      '(sparse)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      '(sparse)
      (let ((t2 (first-term L)))
        ; (display "t1: ")
        ; (display t1)
        ; (newline)
        ; (display "t2: ")
        ; (display t2)
        ; (newline)
        ; (display "rest-terms: ")
        ; (display (rest-terms L))
        ; (newline)
        (adjoin-term
         (make-term (add (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

(define p1 (make-polynomial 'x (attach-tag 'sparse 
                                           (list (make-term 100 1) (make-term 2 2) (make-term 0 1)))))
(define p2 (make-polynomial 'x (attach-tag 'sparse 
                                           (list (make-term 100 2) (make-term 2 2) (make-term 0 1)))))
(define p3 (make-polynomial 'x (attach-tag 'sparse (list (make-term 2 1) (make-term 1 2)))))

(define p4 (make-polynomial 'x '(dense 1 2 0 0 0 0 0 0 0 0)))

(define pp2 (make-poly 'x (attach-tag 'sparse 
                                           (list (make-term 100 2) (make-term 2 2) (make-term 0 1)))))

(define (rec lst)
  (if (empty-termlist? lst)
      nil
      (cons (make-term (order (first-term lst)) (sub 0 (coeff (first-term lst)))) (rec (rest-terms lst)))))

(define (neg p)
  (define (rec lst)
    (if (empty-termlist? lst)
        nil
        (let ((first-t (first-term lst))
              (rest-t (rest-terms lst)))
          (cons (make-term (order first-t) (sub 0 (coeff first-t))) (rec rest-t)))))
  (make-poly (variable p) (attach-tag 'sparse (rec (term-list p)))))

; p4
; (term-list p4)
; (first-term (term-list p4))
; (neg p4)
; p1
; p2
; (get 'add (list (type-tag p1) (type-tag p2)))
; (contents p1)
; (contents p2)
; (variable (contents p1))
; (variable (contents p2))
; (term-list (contents p1))
; (term-list (contents p2))

; (type-tag '(sparse))
; (get 'adjoin-term (type-tag '(sparse)))

; (get 'adjoin-term (type-tag '(sparse)))

(empty-termlist? '(sparse))

; (add p1 p2)

; (sub p1 p2)
; (sub p1 p3)

; (sub p1 p4)

; (mul p1 p2)

(define l1 (term-list (contents p1)))
(define l2 (term-list (contents p2)))
(first-term l1)
; (add-terms (mul-term-by-all-terms (first-term l1) l2)
; (add-terms (mul-term-by-all-terms (first-term (rest-terms l1)) l2)
; (add-terms (mul-term-by-all-terms (first-term (rest-terms (rest-terms l1))) l2) '(sparse)) ))
(add-terms (mul-term-by-all-terms (first-term (rest-terms (rest-terms l1))) l2) '(sparse))
(mul-term-by-all-terms (first-term (rest-terms l1)) l2)
; (add-terms (mul-term-by-all-terms (first-term (rest-terms l1)) l2)
; (add-terms (mul-term-by-all-terms (first-term (rest-terms (rest-terms l1))) l2) '(sparse)))
(add-terms '(sparse (100 2) (2 2) (0 1))
           '(sparse (102 4) (4 4) (2 2)))
; (mul-term-by-all-terms (first-term (rest-terms (rest-terms (rest-terms l1)))) l2)

; (sub p2 p1)
; (sub p2 p3)
; (sub p2 p4)
; (sub p3 p1)
; (sub p3 p2)
; (sub p3 p4)

; (=zero? (sub p1 p1))
; (=zero? (sub p3 p3))

; (order (make-term 0 1))
; (coeff (make-term 0 1))
