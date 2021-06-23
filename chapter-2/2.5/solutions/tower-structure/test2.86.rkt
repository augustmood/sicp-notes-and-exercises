#lang sicp
(#%require "arithmetic.rkt") 
(#%require "tower-system.rkt")
(#%require "operators.rkt")
(install-arithmetic)
(install-coercion)

(drop (make-rational 10 2))
(drop (make-real 2.5))
(make-complex-from-real-imag (make-rational 10 2) (make-real 2.5))

;; We make all operators independent from the data type packages, only define the specific operations 
;; in sepcific data type packages, and then modify the complex number package by modifying all 
;; constructors to accept all other number types args, and drop its type if possible. The complex 
;; operations are also modified so that they use add/sub/mul/div instead of just +/-*/`/`, since now 
;; we accept various arg types instead of just generic numbers.

(sine 10)
