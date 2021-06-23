#lang sicp

; (define (pseudoremainder-terms a b)
;   (let ([o1 (order (first-term a))]
;         [o2 (order (first-term b))]
;         [c (coeff (first-term b))])
;     (let ([new-a (mul-term-by-all-terms (make-term 0 (expt c (+ (- o1 o2) 1))) a)])
;       (cadr (div-terms new-a b)))))

; (define (simplify-gcd a)
;   (car (div-terms a (list 'sparse (make-term 0 (apply gcd (coeff-list a)))))))

; (define (gcd-terms a b)
;   (if (empty-termlist? b)
;       (simplify-gcd a)
;       (gcd-terms b (pseudoremainder-terms a b))))

; (define (gcd-poly a b)
;   (if (same-variable? (variable a) (variable b))
;       (make-poly (variable a) (gcd-terms (term-list a) (term-list b)))
;       (error "Polys not in same var -- GCD-POLY" (list a b))))
