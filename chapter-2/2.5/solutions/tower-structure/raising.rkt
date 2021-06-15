#lang sicp
(#%require "interface.rkt")

;; ex 2.83

(define tower '((1 integer) (2 rational) (3 real) (4 complex)))

(define (tower-level id sets)
  (let ((check (if (number? id) car cadr))
        (lookup (if (number? id) cadr car)))
    (define (iter sets)
      (if (null? sets) (error "No such item") 
          (if (eq? id (check (car sets)))
              (lookup (car sets))
              (iter (cdr sets)))))
    (iter sets)))

(define (raise arg)
  (let ((arg-type (type-tag arg)))
    (define (coerce arg type)
      ((get-coercion arg-type type) arg))
    (define (next-type lst)
      (cond ((eq? arg-type (car lst))
             (if (null? (cdr lst))
                 arg
                 (coerce arg (cadr lst))))
            ((null? (cdr lst)) (error "No such type exists" arg-type))
            (else (next-type (cdr lst)))))
    (next-type tower)))