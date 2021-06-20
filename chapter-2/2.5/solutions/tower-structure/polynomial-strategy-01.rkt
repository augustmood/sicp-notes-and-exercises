#lang sicp
(#%require "interface.rkt")
(#%require "operators.rkt")
(#%require "term-list.rkt")
(#%require "tower-system.rkt")
(#%provide (all-defined))

(define (install-polynomial-package)
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
  (install-term-package)
  
  (define (adjoin-term term term-list)
    (apply-generic 'adjoin-term term term-list))
  
  (define (first-term term-list)
    (apply-generic 'first-term term-list))
  
  (define (rest-terms term-list)
    (apply-generic 'rest-terms term-list))
  
  (define (empty-termlist? term-list)
    (null? (contents term-list)))
  
  (define (the-empty-termlist)
    (attach-tag 'sparse '()))
  
  (define (order term) (apply-generic 'order term))
  (define (coeff term) (apply-generic 'coeff term))
  (define (make-term order coeff) ((get 'make-term 'term) order coeff))
  
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
  
  (define (dense->sparse term-list)
    (define (rec lst)
      (if (empty-termlist? lst)
          '(sparse)
          (let ((first-t (first-term lst))
                (rest-t (rest-terms lst)))
            (adjoin-term (make-term (order first-t) (coeff first-t)) (rec rest-t)))))
    (if (eq? (type-tag term-list) 'dense)
        (rec term-list)
        term-list))
  
  (define (neg-terms L)
    (attach-tag 'sparse (map (lambda (term)
                               (make-term (order term) (sub 0 (coeff term))))
                             (contents (dense->sparse L)))))
  
  (define (sub-terms L1 L2)
    (add-terms (dense->sparse L1) (neg-terms L2)))
  
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
           (make-term (add (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (div-terms (sub-terms L1 
                                             (mul-term-by-all-terms 
                                              (make-term new-o new-c) L2)) L2)))
                  (append-div-result (list (make-term new-o new-c) '()) rest-of-result)))))))
  
  (define (append-div-result rl1 rl2)
    (list (adjoin-term (car rl1) (car rl2)) (append (cadr rl1) (cadr rl2))))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        ; (error "Polys not in same var -- ADD-POLY"
        ;        (list p1 p2))
        (let ((first-p1 (first-term (term-list p1)))
              (first-p2 (first-term (term-list p2))))
          (if (> (order first-p1) (order first-p2))
              (make-poly (variable p1)
                         (adjoin-term (make-term 0 p2)
                                      (term-list p1)))
              (make-poly (variable p2)
                         (adjoin-term (make-term 0 p1)
                                      (term-list p2)))))))
  
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((results (div-terms (term-list p1)
                                  (term-list p2))))
          (list (make-poly (variable p1)
                           (car results))
                (make-poly (variable p1)
                           (cadr results))))
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
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (map tag (div-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)