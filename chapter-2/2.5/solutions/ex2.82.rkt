#lang sicp
(#%require "interface.rkt")
(#%provide (all-defined))
; Show how to generalize apply-generic to handle coercion in the general case of multiple arguments. 
; One strategy is to attempt to coerce all the arguments to the type of the first argument, then to 
; the type of the second argument, and so on. Give an example of a situation where this strategy (and 
; likewise the two-argument version given above) is not sufficiently general. (Hint: Consider the case 
; where there are some suitable mixed-type operations present in the table that will not be tried.)

(define (coerce args)
  (let ((result-list (let ((type-tags (map type-tag args)))
                       (map (lambda (x) 
                              (map (lambda (i) 
                                     (let ((coerce-proc (get-coercion (type-tag i) x)))
                                       (if coerce-proc
                                           (coerce-proc i)
                                           (if (eq? (type-tag i) x) i #f)))) args))
                            type-tags))))
    (filter (lambda (results) (accumulate (lambda (x y) (and x y)) #t results)) result-list)))

(define (error-generator op type-tags) 
  (error "No method for these types:" (map (lambda (i) (list op i)) type-tags)))

(define (apply-coerced op lst)
  (let ((type-tags (map (lambda (i) (map type-tag i)) lst)))
    (define (iter tags lst)
      (if (null? tags)
          (error-generator op type-tags)
          (if (get op (car tags))
              (apply (get op (car tags)) (car lst))
              (iter (cdr tags) (cdr lst)))))
    (iter type-tags lst)
    ))

(define (apply-generic op args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((coerced (coerce args)))
            (if (null? coerced) (error-generator) (apply-coerced op coerced)))))))