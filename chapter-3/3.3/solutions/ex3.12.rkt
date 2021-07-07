#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; Consider the interaction

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z ; (a b c d)
(cdr x)
; <response>
(define w (append! x y))
w ; (a b c d)
(cdr x)
; <response>

; What are the missing <response>s? Draw box-and-pointer diagrams to explain 
; your answer.

;; line 27: '(b)
;; line 31: '(b c d)
