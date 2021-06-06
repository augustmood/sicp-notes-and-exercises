#lang racket
(provide (all-defined-out))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (variable? x) (symbol? x))

(define nil empty)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (operand? op)
  (or (eq? op '+)
      (eq? op '*)
      (eq? op '-)
      (eq? op '**)
      (eq? op '/)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (make-sub a1 a2)
  (cond ((=number? a1 0) (- a2))
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (- a1 a2))
        (else (list a1 '- a2))))

(define (sum? x)
  (and (pair? x) (accumulate (lambda (x y) (or (eq? x '+) y)) #f x)))

(define (addend s)
  (define (rec s)
    (if (eq? (car s) '+)
        nil
        (cons (car s) (rec (cdr s)))))
  (if (= (length (rec s)) 1) (car (rec s)) (rec s)))

(define (augend s)
  (define (rec s)
    (if (eq? (car s) '+)
        (cdr s)
        (rec (cdr s))))
  (if (= (length (rec s)) 1) (car (rec s)) (rec s)))

(define (sub? x)
  (and (pair? x) (memq '- x) #t))

(define (minuend s)
  (cadr s))

(define (subtrahend s)
  (caddr s))

(define (product? x)
  (and (pair? x) (accumulate (lambda (x y) (and (eq? x '*) y)) #t (filter operand? x))))

(define (multiplier p) 
  (define (rec p)
    (if (eq? (car p) '*)
        nil
        (cons (car p) (rec (cdr p)))))
  (car (rec p)))

(define z (make-product '(a + b) '(3 + 6)))
(multiplier z)
(define s (make-product 'a 'e))
(multiplier s)

(define (multiplicand p)
  (define (rec p)
    (if (eq? (car p) '*)
        (cdr p)
        (rec (cdr p))))
  (if (= (length (rec p)) 1)
      (car (rec p))
      (rec p)))

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

(deriv '(x * y * (x + 3)) 'x)
(deriv '((x * y) * (x + 3)) 'x)
(deriv '(x * (y * (x + 3))) 'x)