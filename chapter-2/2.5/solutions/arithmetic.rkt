#lang sicp
(#%provide (all-defined))
(#%require "interface.rkt")
(#%require "scheme-number.rkt")
(#%require "rational-number.rkt")
(#%require "complex-number.rkt")

(define (install-arithmetic-system)
  (install-scheme-number-package)
  (install-rational-package)
  (install-complex-package))

(define (add x y)
  (apply-generic 'add x y))
(define (sub x y)
  (apply-generic 'sub x y))
(define (mul x y)
  (apply-generic 'mul x y))
(define (div x y)
  (apply-generic 'div x y))

;; ex2.79 below:```
(define (convert-to-ordinary x)
  (cond ((eq? (type-tag x) 'scheme-number) x)
        ((eq? (type-tag x) 'complex) (if (= (imag-part x) 0)
                                         (real-part x)
                                         #f))
        (else (apply-generic 'convert-to-ordinary x))))
;   ((get 'rat->ordinary 'rational) x))


(define (equ? x y)
  ;   (apply-generic 'equ? x y))
  (if (eq? (type-tag x) (type-tag y))
      (apply-generic 'equ? x y)
      (let ((converted-x (convert-to-ordinary x))
            (converted-y (convert-to-ordinary y)))
        (and (and converted-x converted-y)
             (equ? converted-y converted-y)))))
;; ```ex2.79 above.

;; ex2.80 below:```
(define (=zero? x)
  (apply-generic '=zero? x))
;; ```ex2.80 above.

(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))