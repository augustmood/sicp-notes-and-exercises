#lang sicp
(#%require "interface.rkt")
(#%require "complex-number.rkt")
(#%require "arithmetic.rkt")
(#%require "ex2.82.rkt")

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(install-arithmetic-system)

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(define (rational-number->complex n)
  (let ((temp (contents n)))
    (make-complex-from-real-imag (exact->inexact (/ (car temp) (cdr temp))) 0)))

(define (complex->complex n)
  n)

(define (complex->scheme-number n)
  (make-scheme-number (real-part n)))

(define (rational-number->scheme-number n)
  (let ((temp (contents n)))
    (make-scheme-number (exact->inexact (/ (car temp) (cdr temp))))))

(define (scheme-number->scheme-number n)
  n)

(put-coercion 'scheme-number 'complex scheme-number->complex)
(put-coercion 'rational 'complex rational-number->complex)
; (put-coercion 'complex 'complex complex->complex)

; (put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'rational 'scheme-number rational-number->scheme-number)
(put-coercion 'complex 'scheme-number complex->scheme-number)



(define cn1 (make-complex-from-real-imag 3 0))
(define sn1 (make-scheme-number 10))
(define rn1 (make-rational 20 10))

(define test-lst (list sn1 cn1 rn1))

(define coerced (coerce '(10 (complex rectangular 3 . 0) (rational 2 . 1) (complex rectangular 3 . 0))))

(define (add . x)
    (if (null? (cdr x))
        (car x)
        (apply-generic 'add x)))


(add sn1 cn1)
(add sn1 cn1 rn1 cn1)
(add cn1 sn1 rn1 cn1)
(add cn1 rn1 sn1 cn1)
(add cn1 rn1 cn1 sn1)
; (add cn1 rn1 cn1 cn1)
(add rn1 cn1)
cn1
(add rn1)