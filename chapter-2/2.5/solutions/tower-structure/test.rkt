#lang sicp
(#%require "interface.rkt")
(#%require "operators.rkt")
(#%provide (all-defined))

(define (install-sparse-package)
  (define (adjoin-term term term-list)
    (if (=zero? (cadr term))
        term-list
        (cons term term-list)))
  (define (first-term term-list) (car term-list))
  (put 'adjoin-term 'sparse adjoin-term)
  (put 'first-term 'sparse first-term)
  'done)


(define (install-dense-package)
  (define (adjoin-term term term-list)
    (let ((diff (- (car term) (car (first-term term-list)))))
      (define (iter n result)
        (if (= n 1)
            result
            (iter (- n 1) (cons 0 result))))
      (cons (cadr term) (iter diff term-list))))
  (define (first-term lst)
    (list (- (length lst) 1) (car lst)))
  (put 'adjoin-term 'dense adjoin-term)
  (put 'first-term 'dense first-term)
  'done)


  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  
  ;; representation of terms and term lists
  (install-dense-package)
  (install-sparse-package)
  (define (adjoin-term term term-list)
    (if (null? term-list)
        (cons term term-list)
        ((get 'adjoin-term (if (pair? (car term-list)) 'sparse 'dense)) term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list)
    ((get 'first-term (if (pair? (car term-list)) 'sparse 'dense)) term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  
  (define (neg p)
    (let ((adjust-p (dense->sparse p)))
      (make-poly (variable p) 
                 (map (lambda (i) 
                        (make-term (order i) (sub 0 (coeff i)))) 
                      (term-list p)))))
  
  (define (dense->sparse p)
    (define (iter lst)
      (if (null? lst)
          nil
          (let ((first (first-term lst)))
            (let ((coef (coeff first))
                  (order (coeff first)))
              (if (= coef 0)
                  (iter (cdr lst))
                  (cons (list order coef) (iter (cdr lst))))))))
    (make-poly (variable p) (iter (term-list p))))
  
  (define (sub-poly p1 p2)
    (add-poly p1 (neg p2)))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  (define (=zero?-poly p)
    (or (empty-termlist? (term-list p))
        (= (accumulate + 0 (map coeff (term-list p))) 0)
        ))
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put '=zero? '(polynomial)
       (lambda (p) (=zero?-poly p)))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))

(make-polynomial)