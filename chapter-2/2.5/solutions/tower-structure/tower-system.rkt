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

(define (project arg)
  (let ((arg-type (type-tag arg))
        (type-level (tower-level (type-tag arg) tower)))
    (define (coerce arg type)
      ((get-coercion arg-type type) arg))
    (if (= type-level 1)
        arg
        (let ((next-type (tower-level (- type-level 1) tower)))
          (coerce arg next-type)))))

(define (general-eq? x y)
(define (iter p result)
  (if (pair? p)
      (iter (cdr p) (cons (cdr p) (cons (car p) result)))
      (map (lambda (i) (if (number? i) (exact->inexact i))) (cons p result))))
  (equal? (iter x nil)
          (iter y nil)))


(define (drop arg)
  (let ((fixed-arg (if (and (eq? (type-tag arg) 'complex) 
                            (eq? (type-tag (contents arg)) 'polar))
                       ((get-coercion 'polar 'rect) arg)
                       arg)))
    (if (general-eq? fixed-arg (raise (project fixed-arg)))
        (drop (project fixed-arg))
        fixed-arg)))

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
  (define (apply-general op sets)
    (let ((type-tags (map type-tag sets)))
      (let ((proc (get op type-tags)))
        (if proc 
            (apply proc (map contents sets))
            (let ((dropped (map drop args)))
              (let ((new-type-tags (map type-tag dropped)))
                (let ((new-proc (get op new-type-tags)))
                  (let ((highest-type (highest new-type-tags)))
                    (if (all-type-same? new-type-tags highest-type)
                        (error-generator op type-tags)
                        (apply-general op (coercion dropped highest-type)))))))))))
  (apply-general op args))