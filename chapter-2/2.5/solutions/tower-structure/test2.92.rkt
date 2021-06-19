#lang sicp
(#%require "interface.rkt")
(#%require "integer.rkt")
(#%require "rational-number.rkt")
(#%require "real.rkt")
(#%require "complex-number.rkt")
(#%require "polynomial.rkt")
(#%require "operators.rkt")
(#%require "arithmetic.rkt")
(#%require "tower-system.rkt")

(install-arithmetic)
(install-polynomial-package)
(install-coercion)
(define poly-1 (make-polynomial 'x '(sparse (term 0 2))))
(define poly-2 (make-polynomial 'x '(sparse (term 1 1) (term 0 2))))
(define poly-3 (make-polynomial 'x '(sparse (term 0 3))))
(define poly-4 (make-polynomial 'x '(sparse (term 6 2) (term 5 1) (term 4 2) (term 3 3))))
(define poly-5 (make-polynomial 'y '(sparse (term 1 1) (term 0 2))))
(define poly-6 (make-polynomial 'y '(sparse (term 0 5))))
; (define complex-1 (make-complex-from-real-imag 5 1))
; ((get-coercion 'polynomial 'complex) poly-1)
; (mul ((get-coercion 'polynomial 'complex) poly-2) ((get-coercion 'polynomial 'complex) poly-2))

; poly-1
; poly-2
; (project poly-2)
; (drop poly-2)
; (mul poly-2 poly-3)
; (mul poly-2 (raise complex-1))
; (raise (raise (raise (make-rational 6 2))))
; (project (make-complex-from-real-imag 10.0 0))
; (general-eq? (make-complex-from-real-imag 10.0 0) 
;              (raise (project (make-complex-from-real-imag 10.0 0))))
; (raise (project (make-complex-from-real-imag 10.0 0)))

; (add poly-2 poly-6)
; (add poly-2 poly-5)
poly-4
(add poly-2 poly-5)
(add poly-4 (add poly-2 poly-5))
(add (add poly-2 poly-5) (add poly-4 (add poly-2 poly-5)))
; ((get-coercion 'complex 'polynomial) (raise (raise (raise 0))))
; (add poly-1 poly-2)
; (add poly-1 poly-5)

; (get-coercion 'real 'complex)

;; integer/real/rational -> polynomial: ok
;; complex -> polynomial: ?
;;           (if (= (imag-part complex) 0)
;;               -> polynomial: ok
;;               -> polynomial: work, but not in good looking.
;; In fact, this is diffcult to justify, uhmmm, since our original raise/drop/coercion only takes
;; one argument, and even from the understanding of coercion and the number-systems, We'd better
;; not to coerce these numbers to polynomials.
;; And we may need to modifiy the mul or create a new `mul` that can multiply polynomial by complex
;; or other sort of number, it may need to drop the number to as simple as possible, and then coerce
;; to the polynomial with the variable of the existed poly's variable.
;; 
;; polynomial -> complex: only if the polynomial only has the 0-order term.

;;
;; No complex number can have a real-part or imag-part of a polynomial, if so, it is a polynomial
;; and not a complex number.

;; 