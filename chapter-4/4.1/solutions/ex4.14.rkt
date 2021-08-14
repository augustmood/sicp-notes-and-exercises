#lang sicp
(#%require "global-env.rkt")

(driver-loop)

; (eval* 
;  '(map car '((1 2 3) (2 3 4) (3 4 5) (4 5 6) (5 6 7))) 
;  the-global-environment)

;; the `list-of-values` will call eval* on some primitive procedures, which will
;; produce like (list 'car car) and cannot be used in the underlying lisp 
;; system.
