#lang sicp
(#%require "lazy-interpreter.rkt")

;; a

; since `display` is primitive function, it will be forced to produce the actual
; value, which means in this case, `eval` functions as `actual-value`, and thus
; Ben is right.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; b

; (define (p1 x)
;   (set! x (cons x '(2)))
;   x)
  
; (p1 1)

; (define (p2 x)
;   (define (p e)
;     e
;     x)
;   (p (set! x (cons x '(2)))))
; (p2 1)

(driver-loop)

; original:
; (p1 1): '(1 2)
; (p2 1): 1 ; the `e` in function p will be delayed, thus,  it has not effects
; x.

; Cy's version:
; (p1 1): '(1 2)
; (p2 1): '(1 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; c

; Because the example in part a calls `display`, which is a primitive procedure,
; so it will just call `actual-value` instead of delaying.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; d

; Followed the concept of the normal-order evaluation, I prefer the original
; eval-sequence version, I do think a thunk should be forced only when it is
; needed.
