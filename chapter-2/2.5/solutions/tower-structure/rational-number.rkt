#lang sicp
(#%require "interface.rkt")
(#%provide (all-defined))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (if (= d 0)
        (error "Denominator cannot be equal to 0")
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g)))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  
  ;; ex2.79 below:```
  (define (equ?-rat x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  
  (define (rat->ordinary x)
    ; (numer x))
    (exact->inexact (/ (numer x) (denom x))))
  ;; ```ex2.79 above.
  
  ;; ex2.80 below:```
  (define (=zero? x)
    (= (numer x) 0))
  ;; ```ex2.80 above.
  
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
       (lambda (x) (=zero? x)))
  ;; ```ex2.80 above.
  
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)