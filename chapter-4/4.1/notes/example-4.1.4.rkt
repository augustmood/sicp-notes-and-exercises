#lang sicp
(#%require "global-env.rkt")

(driver-loop)

;;; M-Eval input:
; (define (append x y)
;   (if (null? x)
;       y
;       (cons (car x)
;             (append (cdr x) y))))

;;; M-Eval value:
; ok

;;; M-Eval input:
; (append '(a b c) '(d e f))

;;; M-Eval value:
; (a b c d e f)

