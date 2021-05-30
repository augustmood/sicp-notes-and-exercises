#lang sicp

; Define a procedure last-pair that returns the list that contains only the last element of a 
; given (nonempty) list:

; (last-pair (list 23 72 149 34))
; (34)

(define (last-pair list)
    (if (null? (cdr list))
        (car list)
        (last-pair (cdr list))))