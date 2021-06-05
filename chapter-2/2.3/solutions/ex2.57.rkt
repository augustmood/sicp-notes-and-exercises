#lang racket
(provide (all-defined-out))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

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

(define (make-sub a1 a2)
  (cond ((=number? a1 0) (- a2))
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (- a1 a2))
        (else (list '- a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

; (define (augend s) 
;   (let ((new-s (cddr s)))
;     (if (= (length new-s) 1)
;         (car new-s)
;         (append '(+) new-s))))

(define (augend s) 
  (accumulate make-sum 0 (cddr s)))

(define (sub? x)
  (and (pair? x) (eq? (car x) '-)))

(define (minuend s)
  (cadr s))

(define (subtrahend s)
  (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

; (define (multiplicand p) 
;   (let ((new-p (cddr p)))
;     (if (= (length new-p) 1)
;         (car new-p)
;         (append '(*) new-p))))

(define (multiplicand p) 
    (accumulate make-product 1 (cddr p)))

;; (** u n)
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

(define (make-exponentiation b n)
  (cond ((=number? n 0) 1)
        ((=number? n 1) b)    
        ((and (number? b) (number? n)) (expt b n))
        (else (list '** b n))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product 
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (make-sub (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))


(deriv '(* x y (+ x 3)) 'x)

;; Once there are many variables, we may also simplify the formula first: calculate the sum/product of 
;; all numeric items, and then combine this result with the non-numeric items.

; (define (simplify exp)
;   (let ((p (if (sum? exp) + *))
;         (init (if (sum? exp) 0 1)))
;     (let
;         ((non-num (filter (lambda (i) (not (number? i))) exp))
;          (num-sum (accumulate p init (filter (lambda (i) (number? i)) exp))))
;       (append non-num (list num-sum)))))

; (sort-num '(+ 1 2 3 4 5 x y z))
; (sort-num '(* 1 2 3 4 5 x y z))