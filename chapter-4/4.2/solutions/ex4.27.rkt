#lang sicp
(#%require "lazy-interpreter.rkt")

(driver-loop)

(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)

(define w (id (id 10)))

;;; L-Eval input:
count

;;; L-Eval value:
;; 1 ; define w, needed to evaluate the (id 10)

;;; L-Eval input:
w

;;; L-Eval value:
;; 10 ; call w, need to evaluate (id (id 10)), since we've already know 
;; `(id 10)`, so we only need to evaluate (id '(evaluated-thunk 10)), and thus 
;; the count increments to 2.

;;; L-Eval input:
count

;;; L-Eval value:
2
