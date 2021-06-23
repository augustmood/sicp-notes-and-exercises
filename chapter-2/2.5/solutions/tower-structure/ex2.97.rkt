#lang sicp
(#%require "polynomial.rkt")
(#%require "operators.rkt")
(#%require "arithmetic.rkt")

(install-arithmetic)
(install-polynomial-package)
(install-coercion)

; (define (reduce-terms n d)
;   (let ([rf (gcd-terms n d)]) 
;     (map car (list (div-terms n rf) (div-terms d rf)))))

; (define (reduce-poly p1 p2)
;   (if (same-variable? (variable p1) (variable p2))
;       (let ([n (term-list p1)]
;             [d (term-list p2)])
;         (map (lambda (i) (attach-tag (variable p1) i)) (reduce-terms n d)))
;       (error "Polys not in same var -- REDUCE-POLY"
;              (list p1 p2))))

(define p1 (make-polynomial 'x '(sparse (term 1 1)(term 0 1))))
(define p2 (make-polynomial 'x '(sparse (term 3 1)(term 0 -1))))
(define p3 (make-polynomial 'x '(sparse (term 1 1))))
(define p4 (make-polynomial 'x '(sparse (term 2 1)(term 0 -1))))

(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

(mod-print p1)
(mod-print p2)
(mod-print (make-rational p1 p2))

(mod-print p3)
(mod-print p4)
(mod-print (make-rational p3 p4))

(mod-print rf1)
(mod-print rf2)
(mod-print (add rf1 rf2))
