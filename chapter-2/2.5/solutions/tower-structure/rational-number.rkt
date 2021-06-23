#lang sicp
(#%require "interface.rkt")
(#%require "operators.rkt")
(#%provide (all-defined))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cadr x))
  (define (make-rat n d)
    (if (=zero? d)
        (error "Denominator cannot be equal to 0")
        (let ([args (reduce n d)])
          (list (car args) (cadr args)))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  
  ;; ex2.79 below:```
  (define (equ?-rat x y)
    (and (equ? (numer x) (numer y))
         (equ? (denom x) (denom y))))
  
  (define (rat->ordinary x)
    ; (numer x))
    (exact->inexact (/ (numer x) (denom x))))
  ;; ```ex2.79 above.
  
  ;; ex2.80 below:```
  (define (=zero?-rat x)
    (= (numer x) 0))
  ;; ```ex2.80 above.
  
  (define (print-rat x)
    (append (list (mod-print (numer x))) (list '/) (list (mod-print (denom x)))))
  
  ;   (define (gcf-rational args)
  ;     (apply gcd (map rat->ordinary args)))
  
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  
  ;; ex2.79 below:```
  (put 'equ? '(rational rational)
       (lambda (x y) (equ?-rat x y)))
  
  (put 'convert-to-ordinary '(rational)
       (lambda (x) (rat->ordinary x)))
  ;; ```ex2.79 above.
  
  ;; ex2.80 below:```
  (put '=zero? '(rational)
       (lambda (x) (=zero?-rat x)))
  ;; ```ex2.80 above.
  
  (put 'mod-print '(rational)
       (lambda (p) (print-rat p)))
  
  (put 'sine '(rational)
       (lambda (x) (sin (/ (numer x) (denom x)))))
  
  (put 'cosine '(rational)
       (lambda (x) (cos (/ (numer x) (denom x)))))
  
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
