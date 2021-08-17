#lang sicp
(#%require "lazy-interpreter.rkt")

; (define count 0)
; (define (id x)
;   (set! count (+ count 1))
;   x)

; (define (square x)
;   (* x x))

;;; L-Eval input:
; (square (id 10))

;;; L-Eval value:
; with memoizes: 100, w/o memoizes: 100

;;; L-Eval input:
; count

;;; L-Eval value:
; with memoizes: 1, w/o memoizes: 2

(driver-loop)
