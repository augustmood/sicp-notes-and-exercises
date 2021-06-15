#lang sicp
(#%require "interface.rkt")
(#%provide (all-defined))

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
  (let ((arg-type (type-tag arg))
        (type-level (tower-level (type-tag arg) tower)))
    (define (coerce arg type)
      ((get-coercion arg-type type) arg))
    (if (= type-level (length tower))
        arg
        (let ((next-type (tower-level (+ 1 type-level) tower)))
          (coerce arg next-type)))))

(define (highest type-tags)
  (tower-level (apply max (map (lambda (i) (tower-level i tower)) type-tags)) tower))

(define (all-type-same? type-tags type)
  (accumulate (lambda (x y) (and (eq? x type) y)) #t type-tags))

(define (coercion args highest-type)
  (define (iter args)
    (if (all-type-same? (map type-tag args) highest-type)
        args
        (iter (map 
               (lambda (i) 
                 (if (eq? (type-tag i) highest-type) 
                     i 
                     (raise i))) args))))
  (iter args))

(define (error-generator op type-tags)
  (error "No method for these types" (list op type-tags)))

(define (apply-generic op . args)
  (define (apply-general op args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc 
            (apply proc (map contents args))
            (let ((highest-type (highest (map type-tag args))))
              (if (all-type-same? type-tags highest-type)
                  (error-generator op type-tags)
                  (apply-general op (coercion args highest-type))))))))
  (apply-general 'add args))