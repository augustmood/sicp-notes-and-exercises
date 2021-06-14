#lang sicp
(#%require "interface.rkt")
(#%require "generic.rkt")
(#%require "complex-number.rkt")

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (or (= x y) (< (- x y) 0.0000001))))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))  
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  ;; following added to Scheme-number package
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y)))) ; using primitive expt
  'done)

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)

(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

(install-scheme-number-package)
(install-complex-package)

(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (exp x y) 
  (apply-generic 'exp x y))

;; a
;; `exp` goes into an infinite loop when we call `exp` with two complex numbers as arguments.

;; b
;; `apply-generic` works correctly as it is, Louis's implementation doesn't.
;; we may not need a same-type coercion, we just need to check if the type are the same if it is 
;; necessary to do a coercion.

;; c

; (define (apply-generic op . args)
;   (let ((type-tags (map type-tag args)))
;     (let ((proc (get op type-tags)))
;       (define (error-generator) 
;         (error "No method for these types" (list op type-tags)))
;       (if proc
;           (apply proc (map contents args))
;           (if (= (length args) 2)
;               (let ((type1 (car type-tags))
;                     (type2 (cadr type-tags))
;                     (a1 (car args))
;                     (a2 (cadr args)))
;                 (if (eq? type1 type2)
;                     (error-generator)
;                     (let ((t1->t2 (get-coercion type1 type2))
;                           (t2->t1 (get-coercion type2 type1)))
;                       (cond (t1->t2
;                              (apply-generic op (t1->t2 a1) a2))
;                             (t2->t1
;                              (apply-generic op a1 (t2->t1 a2)))
;                             (else
;                              (error-generator))))))
;               (error-generator))))))