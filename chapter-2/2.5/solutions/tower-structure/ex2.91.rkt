#lang sicp
(#%require "operators.rkt")
(#%require "polynomial.rkt")
(#%require "arithmetic.rkt")

(install-arithmetic)
(install-polynomial-package)

(define poly-1 (make-polynomial 'x '(sparse (term 6 2) (term 5 1) (term 4 2) (term 3 3))))
(define poly-2 (make-polynomial 'x 
                                '(sparse (term 5 2) (term 4 3) (term 3 1) 
                                         (term 2 5) (term 1 9) (term 0 10))))
(define poly-3 (make-polynomial 'x '(dense 2 1 2 3 0 0 0)))
(define poly-4 (make-polynomial 'x '(dense 2 3 1 5 9 7 9)))
(define poly-5 (make-polynomial 'x '(dense 2 3 1 5 9 10)))

;; the two polynomials in the example:
(define poly-6 (make-polynomial 'x '(sparse (term 5 1) (term 0 -1))))
(define poly-7 (make-polynomial 'x '(sparse (term 2 1) (term 0 -1))))

;; I modified `sub-poly` as well as `neg` so that they are analogous to the given `add-poly` and 
;; `mul-poly`. Our original `sub-poly` just called the negation function directly on the polynomial 
;; expression and then used it to call `add-poly`, but I found it seemed more helpful to separate the 
;; make-poly from the negation term-list in the `neg` function, since we could then use it on long 
;; division.

; (define (sub-poly p1 p2)
;   (if (same-variable? (variable p1) (variable p2))
;       (make-poly (variable p1)
;                  (sub-terms (term-list p1)
;                             (term-list p2)))
;       (error "Polys not in same var -- ADD-POLY"
;              (list p1 p2))))

; (define (neg-terms L)
;   (attach-tag 'sparse (map (lambda (term)
;                              (make-term (order term) (sub 0 (coeff term))))
;                            (contents (dense->sparse L)))))

; (define (sub-terms L1 L2)
;   (add-terms (dense->sparse L1) (neg-terms L2)))

; (define (div-terms L1 L2)
;   (if (empty-termlist? L1)
;       (list (the-empty-termlist) (the-empty-termlist))
;       (let ((t1 (first-term L1))
;             (t2 (first-term L2)))
;         (if (> (order t2) (order t1))
;             (list (the-empty-termlist) L1)
;             (let ((new-c (div (coeff t1) (coeff t2)))
;                   (new-o (- (order t1) (order t2))))
;               (let ((rest-of-result
;                      (div-terms (sub-terms L1 
;                                            (mul-term-by-all-terms 
;                                             (make-term new-o new-c) L2)) L2)))
;                 (append-div-result (list (make-term new-o new-c) '()) rest-of-result)))))))

; (define (append-div-result rl1 rl2)
;   (list (adjoin-term (car rl1) (car rl2)) (append (cadr rl1) (cadr rl2))))

; (define (div-poly p1 p2)
;   (if (same-variable? (variable p1) (variable p2))
;       (let ((results (div-terms (term-list p1)
;                                 (term-list p2))))
;         (list (make-poly (variable p1)
;                          (car results))
;               (make-poly (variable p1)
;                          (cadr results))))
;       (error "Polys not in same var -- MUL-POLY"
;              (list p1 p2))))

; (put 'make 'polynomial
;      (lambda (var terms) (tag (make-poly var terms))))

(div poly-1 poly-1)
(div poly-2 poly-2)
(div poly-3 poly-3)
(div poly-4 poly-4)
(div poly-5 poly-5)

(div poly-1 poly-2)
(div poly-2 poly-3)
(div poly-3 poly-4)
(div poly-4 poly-5)

(div poly-6 poly-7)

(mul poly-1 2)
