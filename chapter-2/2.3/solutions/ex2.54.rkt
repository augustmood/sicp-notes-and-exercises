#lang sicp

(define (equal? lst-1 lst-2)
  (let ((first (car lst-1))
        (second (car lst-2)))
  (cond ((not (= (length lst-1) (length lst-2))) #false)
        ((null? (cdr lst-1)) (eq? first second))
        (else (and (eq? first second)
                   (equal? (cdr lst-1) (cdr lst-2)))))))

(equal? '(this is a list) '(this is a list))

(equal? '(this is a list) '(this (is a) list))

(equal? '(this is a list) '(this (is a) nested list))