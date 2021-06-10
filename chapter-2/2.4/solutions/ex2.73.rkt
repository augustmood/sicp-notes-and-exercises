#lang sicp
(#%require "interface.rkt")

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (install-deriv-package)
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  
  (define (make-exponentiation b n)
    (cond ((=number? n 0) 1)
          ((=number? n 1) b)    
          ((and (number? b) (number? n)) (expt b n))
          (else (list '** b n))))
  
  (define (addend arg) (car arg))
  (define (augend arg) (cadr arg))
  
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  
  (define (base e) (car e))
  (define (exponent e) (cadr e))
  
  
  (define (deriv-sum arg var)
    (make-sum (deriv (addend arg) var)
              (deriv (augend arg) var)))
  
  (define (deriv-product arg var)
    (make-sum
     (make-product (multiplier arg)
                   (deriv (multiplicand arg) var))
     (make-product (deriv (multiplier arg) var)
                   (multiplicand arg))))
  
  (define (deriv-expt arg var)
    (make-product 
     (make-product (exponent arg)
                   (make-exponentiation (base arg)
                                        (make-sum (exponent arg) -1)))
     (deriv (base arg) var)))
  
  ;; interface to the rest of the system  
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-expt)
  'done)
  
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(install-deriv-package)
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(deriv '(* x y) 'x)
(deriv '(+ x (+ x x)) 'x)
(deriv '(** x 3) 'x)

;; a

;; It just deletes the operator-diciding steps and funtion-diciding part and instead looking for the 
;; corresponding function and ways of transformation of the operands in the already built table.

;; They are just predicates, so there's no need to dispatch.


;; d

;; We need to edit the `put` part, reverse the order of the first two variables in those `pur` 
;; operations.