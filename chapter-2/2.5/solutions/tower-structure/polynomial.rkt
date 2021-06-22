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
          (the-empty-termlist)
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
  
  (define (order-zero p)
    (let ((reversed (reverse (contents (dense->sparse (term-list p))))))
      (let ((first (car reversed)))
        (if (= (order first) 0)
            (list (coeff first) (make-poly (variable p) (cons 'sparse (reverse (cdr reversed)))))
            (list 0 p)))))
  
  (define (polynomial? p)
    (and (pair? p) (eq? (type-tag p) 'polynomial)))
  
  (define (add-poly p1 p2)
    (let ((first-p1 (first-term (term-list p1)))
          (first-p2 (first-term (term-list p2))))
      (cond 
        ((or (same-variable? (variable p1) (variable p2))
             (and (= (order first-p1) 0) (= (order first-p2) 0)))
         (make-poly (variable p1)
                    (add-terms (term-list p1)
                               (term-list p2))) )
        ((= (order first-p1) 0)
         (add-poly (make-poly (variable p2) (term-list p1)) p2))
        
        ((= (order first-p2) 0)
         (add-poly p1 (make-poly (variable p1) (term-list p2))))
        
        (else (let ((zero-p1 (car (order-zero p1)))
                    (zero-p2 (car (order-zero p2)))
                    (no-zero-p1 (cadr (order-zero p1)))
                    (no-zero-p2 (cadr (order-zero p2))))
                (cond ((and (polynomial? zero-p1) (polynomial? zero-p2))
                       (add-poly (add-poly (contents zero-p1) no-zero-p2)
                                 (add-poly (contents zero-p2) no-zero-p1)))
                      ((polynomial? zero-p1)
                       (add-poly (add-poly (contents zero-p1) p2) no-zero-p1))
                      ((polynomial? zero-p2)
                       (add-poly (add-poly (contents zero-p2) p1) no-zero-p2))
                      (else 
                       (let ((args (if (> (order first-p1) (order first-p2))
                                       (list p1 p2)
                                       (list p2 p1))))
                         (add-poly (car args)
                                   (make-poly (variable (car args))
                                              (list 'sparse 
                                                    (make-term 0 (tag (cadr args))))))))))))))
  
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  
  (define (coeff-list p)
    (map coeff (contents (term-list p))))
  
  (define (get-co p)
    (let ((base (contents (car (coeff-list p)))))
      (let ((co-arg (apply gcd (coeff-list base))))
        (div-poly base (make-poly (variable base) (list 'sparse (make-term 0 co-arg)))))))
  
  (define (gcd-poly args)
    (let ((base (contents (car args))))
      (let ((co-arg (apply gcd (coeff-list base))))
        (div-poly base (make-poly (variable base) (list 'sparse (make-term 0 co-arg)))))))
  
  (define (poly-list? termlist)
    (accumulate (lambda (x y) (or x y)) #f (map polynomial? termlist)))
  
  (define (mod-gcd args)
    (cond ((poly-list? args) (gcd-poly args)) ;; <- polynomials
          ((null? args) (make-poly 'temp-var (list 'sparse (make-term 0 1)))) ;; <- null, return 1
          (else (make-poly 'temp-var (list 'sparse (make-term 0 (apply gcd args))))))) ;; <- numbers

  (define (mul-poly p1 p2)
    (let ((of-p1 (order (first-term (term-list p1))))
          (of-p2 (order (first-term (term-list p2)))))
      (let ((list-args (if (> of-p1 of-p2)
                           (list p1 p2)
                           (list p2 p1))))
        (let ((arg1 (car list-args))
              (arg2 (cadr list-args)))
          (if (or (same-variable? (variable arg1) (variable arg2))
                  (= (mul of-p1 of-p2) 0))
              (make-poly (variable arg1)
                         (mul-terms (term-list arg1)
                                    (term-list arg2)))
              (let ((coeff-list-p1 (coeff-list arg1)) 
                    (coeff-list-p2 (coeff-list arg2)))
                (let ((multi-t1 (mod-gcd coeff-list-p1))
                      (multi-t2 (mod-gcd coeff-list-p2)))
                  (let ((arg-p1 (div-poly arg1 
                                          (make-poly (variable arg1) 
                                                     (list 'sparse (make-term 0 (tag multi-t1))))))
                        (arg-p2 (div-poly arg2
                                          (make-poly (variable arg2) 
                                                     (list 'sparse (make-term 0 (tag multi-t2)))))))
                    (cond ((and (same-variable? (variable multi-t1) (variable arg-p2))
                                (same-variable? (variable multi-t2) (variable arg-p1)))
                           (mul-poly (mul-poly multi-t1 arg-p2)
                                     (mul-poly multi-t2 arg-p1)))
                          ((same-variable? (variable multi-t1) (variable arg-p2))
                           (mul-poly arg-p1 (mul-poly arg2 multi-t1)))
                          ((same-variable? (variable multi-t2) (variable arg-p1))
                           (mul-poly (mul-poly arg1 multi-t2) arg-p2))
                          (else   (mul-poly arg1 
                                            (make-poly (variable arg1)
                                                       (list 'sparse 
                                                             (make-term 0 (tag arg2)))))))))))))))

  (define (div-poly p1 p2)
    (let ((div-result
           (cond ((= (order (first-term (term-list p2))) 0)
                  (let ((coeff-p2 (coeff (first-term (term-list p2)))))
                    (list (make-poly (variable p1)
                                     (cons 'sparse (map (lambda (i) 
                                                          (make-term (order i) 
                                                                     (div (coeff i) coeff-p2)))
                                                        (contents (term-list p1)))))
                          (make-poly (variable p1)
                                     (the-empty-termlist)))))
                 ((same-variable? (variable p1) (variable p2))
                  (let ((results (div-terms (term-list p1)
                                            (term-list p2))))
                    (list (make-poly (variable p1)
                                     (car results))
                          (make-poly (variable p1)
                                     (cadr results)))))
                 (else (error "Polys not in same var -- DIV-POLY" (list p1 p2))))))
      (if (empty-termlist? (term-list (cadr div-result)))
          (let ((result (car div-result)))
            (if (= (order (first-term (term-list result))) 0)
                (coeff (first-term (term-list result)))
                result))
          div-result)))
  
  (define (=zero?-poly p)
    (or (empty-termlist? (term-list p))
        (= (accumulate + 0 (map coeff (term-list p))) 0)
        ))
  
  (define (print-poly p)
    (let ((arg-p (make-poly (variable p) (dense->sparse (term-list p)))))
      (let ((var (variable arg-p))
            (termlists (term-list arg-p)))
        (define (iter lst)
          (if (empty-termlist? lst)
              nil
              (let ((first-t (first-term lst))
                    (rest-t (rest-terms lst)))
                (let ((coeff-first (coeff first-t))
                      (order-first (order first-t)))
                  (append (list '+) (let ([revised-coeff (if (polynomial? coeff-first)
                                                             (print-poly (contents coeff-first))
                                                             coeff-first)])
                                      (cond [(and (number? revised-coeff)
                                                  (= revised-coeff 1) 
                                                  (= order-first 0)) (list revised-coeff)]
                                            [(and (number? revised-coeff) 
                                                  (= revised-coeff 1) 
                                                  (= order-first 1)) (list var)]
                                            [(and (number? revised-coeff) 
                                                  (= revised-coeff 1)) 
                                             (list var '^ order-first)]
                                            [(= order-first 0) (list revised-coeff)]
                                            [(= order-first 1) (list revised-coeff '* var)]
                                            [else (list revised-coeff '* var '^ order-first)]))
                          (iter rest-t))))))
        (cdr (iter termlists))
        )))
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put '=zero? '(polynomial)
       (lambda (p) (=zero?-poly p)))
  (put 'mod-print '(polynomial)
       (lambda (p) (print-poly p)))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))  
  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (let ((div-result (div-poly p1 p2)))
                         (if (pair? div-result)
                             (if (= (length div-result) 2)
                                 (map tag div-result)
                                 (tag div-result))
                             div-result))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)