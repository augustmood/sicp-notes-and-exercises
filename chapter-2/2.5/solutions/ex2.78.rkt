#lang sicp
(#%require "interface.rkt")

; (define (attach-tag type-tag contents)
;   (if (and (eq? type-tag 'scheme-number) (number? contents))
;       contents
;       (cons type-tag contents)))

; (define (type-tag datum)
;   (cond 
;     ((number? datum) 'scheme-number)
;     ((pair? datum) (car datum))
;     (else (error "Bad tagged datum -- TYPE-TAG" datum))))

; (define (contents datum)
;   (cond 
;     ((number? datum) datum)
;     ((pair? datum) (cdr datum))
;     (else (error "Bad tagged datum -- CONTENTS" datum))))

;; I have integrated the above procedures to "interface.rkt" file to make them work better with the 
;; newly revised scheme-number package.

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
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(install-scheme-number-package)
(define (add x y)
  (apply-generic 'add x y))
(define (sub x y)
  (apply-generic 'sub x y))
(define (mul x y)
  (apply-generic 'mul x y))
(define (div x y)
  (apply-generic 'div x y))
(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))