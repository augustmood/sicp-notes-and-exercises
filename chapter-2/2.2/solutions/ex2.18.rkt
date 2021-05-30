#lang racket

; Define a procedure reverse that takes a list as argument and returns a list of the same elements 
; in reverse order:

; (reverse (list 1 4 9 16 25))
; (25 16 9 4 1)

(define (reverse list)
  (if (null? list)
      empty
      (append (reverse (cdr list)) (cons (car list) empty))))

(reverse (list 1 4 9 16 25))