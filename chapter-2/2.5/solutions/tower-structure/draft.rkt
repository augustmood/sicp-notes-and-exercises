#lang sicp
; (require (except-in sicp
;                     angle
;                     magnitude
;                     real-part
;                     imag-part))
(#%require "interface.rkt")
(#%require "operators.rkt")
(#%require "arithmetic.rkt")
(#%require "tower-system.rkt")
(#%require "polynomial.rkt")
(#%provide (all-defined))
; (print-as-expression #f)
; (print-mpair-curly-braces #f)

(define (install-term-package)
  (define (make-term order coeff) (list order coeff))
  (define (tag x) (attach-tag 'term x))
  (define (tag-exist? term) (and (= (length term) 3) (eq? (car term) 'term)))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (put 'make-term 'term (lambda (order coeff) (tag (make-term order coeff))))
  (put 'order '(term) order)
  (put 'coeff '(term) coeff))

(define (install-sparse-package)
  (install-term-package)
  (define (order term) (apply-generic 'order term))
  (define (coeff term) (apply-generic 'coeff term))
  (define (make-term order coeff) ((get 'make-term 'term) order coeff))
  
  (define (adjoin-term term term-list)
    (let ((revised-term (make-term (car term) (cadr term))))
      (if (and (not (eq? (type-tag (coeff revised-term)) 'polynomial))
               (=zero? (coeff revised-term)))
          term-list
          (cons revised-term term-list))))
  (define (first-term term-list)
    (if (null? term-list)
        (make-term 0 0)
        (make-term (order (car term-list)) (coeff (car term-list)))))
  (define (rest-terms term-list) 
    (if (null? term-list)
        '()
        (cdr term-list)))
  (define (tag x) (attach-tag 'sparse x))
  (put 'adjoin-term '(term sparse)
       (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'first-term '(sparse) (lambda (term-list) (first-term term-list))) ;; ?
  (put 'rest-terms '(sparse) (lambda (term-list) (tag (rest-terms term-list))))
  'done)

(define (install-dense-package)
  (install-term-package)
  (define (order term) (apply-generic 'order term))
  (define (coeff term) (apply-generic 'coeff term))
  (define (make-term order coeff) ((get 'make-term 'term) order coeff))
  
  (define (iter n result)
    (if (= n 1)
        result
        (iter (- n 1) (cons 0 result))))
  (define (adjoin-term term term-list)
    (let ((revised-term (make-term (car term) (cadr term))))
      (if (and (not (eq? (type-tag (coeff revised-term)) 'polynomial))
               (=zero? (coeff revised-term)))
          term-list
          (if (null? term-list)
              (cons (coeff revised-term) (iter (add 1 (order revised-term)) term-list))
              (let ((diff (- (order revised-term) (order (first-term term-list)))))
                (cons (coeff revised-term) (iter diff term-list)))))))
  
  (define (first-term term-list)
    (if (null? term-list)
        (make-term 0 0)
        (make-term (- (length term-list) 1) (car term-list))))
  (define (rest-terms term-list) 
    (if (null? term-list)
        '()
        (cdr term-list)))
  (define (tag x) (attach-tag 'dense x))
  (define (term-tag x) (attach-tag 'term x))
  (put 'adjoin-term '(term dense)
       (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'first-term '(dense) (lambda (term-list) (first-term term-list)))
  (put 'rest-terms '(dense) (lambda (term-list) (tag (rest-terms term-list))))
  'done)


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

(define (poly-conversion x)
  (if (and (pair? x) (symbol? (car x)) (or (eq? (cadr x) 'sparse) (eq? (cadr x) 'dense)))
      (tag x)
      x))

(define (coeff-list p)
  (map coeff (contents (term-list p))))

(define (poly-list? termlist)
  (accumulate (lambda (x y) (or x y)) #f (map polynomial? termlist)))

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

; (define (add-poly p1 p2)
;   (if (same-variable? (variable p1) (variable p2))
;       (make-poly (variable p1)
;                  (add-terms (term-list p1)
;                             (term-list p2)))
;       (error "Polys not in same var -- ADD-POLY"
;              (list p1 p2))))

;; serve for add-poly:

; (define (order-zero-term p)
;   (coeff (car (filter (lambda (i) (= (order i) 0)) 
;                       (contents (dense->sparse (term-list p))) ))))

(define (reverse lst)
  (accumulate (lambda (x y) (append y (list x))) nil lst))

(define (order-zero p)
  (let ((reversed (reverse (contents (dense->sparse (term-list p))))))
    (let ((first (car reversed)))
      (if (= (order first) 0)
          (list (coeff first) (make-poly (variable p) (cons 'sparse (reverse (cdr reversed)))))
          (list 0 p)))))

(define (polynomial? p)
  (and (pair? p) (eq? (type-tag p) 'polynomial)))


(define (simplify-addition p var)
  (let ((curr-var (variable p))
        (terms (term-list p)))
    (define (iter terms)
      (if (empty-termlist? terms)
          (make-poly var (list 'sparse))
          (let 
              ((first (first-term terms))
               (rests (rest-terms terms)))
            (add-poly 
             (contents 
              (mul (coeff first)
                   (tag (make-poly var (list 'sparse 
                                             (make-term 0 
                                                        (tag (make-poly 
                                                              curr-var (list 
                                                                        'sparse 
                                                                        (make-term 
                                                                         (order first) 1))))))))))
             (iter rests)))))
    (iter terms)))

(define (add-poly p1 p2)
  (let ((first-p1 (first-term (term-list p1)))
        (first-p2 (first-term (term-list p2))))
    (cond 
      ((or (same-variable? (variable p1) (variable p2))
           (and (= (order first-p1) 0) (= (order first-p2) 0)))
       (make-poly (variable p1)
                  (add-terms (term-list p1)
                             (term-list p2))))
      ((= (order first-p1) 0)
       (add-poly (make-poly (variable p2) (term-list p1)) p2))
      
      ((= (order first-p2) 0)
       (add-poly p1 (make-poly (variable p1) (term-list p2))))
      
      (else (let ((zero-p1 (car (order-zero p1)))
                  (zero-p2 (car (order-zero p2)))
                  (no-zero-p1 (cadr (order-zero p1)))
                  (no-zero-p2 (cadr (order-zero p2))))
              ; (display "zero-p1: ")
              ; (display zero-p1)
              ; (newline)
              ; (display "zero-p2: ")
              ; (display zero-p2)
              ; (newline)
              ; (display "no-zero-p1: ")
              ; (display no-zero-p1)
              ; (newline)
              ; (display "no-zero-p2: ")
              ; (display no-zero-p2)
              ; (newline)
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
                       ;  (display "p1: ")
                       ;  (display p1)
                       ;  (newline)   
                       ;  (display "car-args: ")
                       ;  (display (car args))
                       ;  (newline)                                  
                       ;  (display "p2: ")
                       ;  (display p2)
                       ;  (newline)
                       ;  (display "cadr-args: ")
                       ;  (display (tag (cadr args)))
                       ;  (newline)     
                       ;  (display "args: ")
                       ;  (display args)
                       ;  (newline)
                       (add-poly (car args)
                                 (make-poly (variable (car args))
                                            (list 'sparse (make-term 0 (tag (cadr args))))))))))))))

(define (add-poly-pro p1 p2)
  (let ((result (add-poly p1 p2)))
    (let ((zero-p (car (order-zero result)))
          (no-zero-p (cadr (order-zero result))))
      (if (pair? zero-p)
          (add-poly (simplify-addition (contents zero-p) (variable result)) no-zero-p)
          result))))




(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (sub-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

(define (get-co p)
  (let ((base (contents (car (coeff-list p)))))
    (let ((co-arg (apply gcd (coeff-list base))))
      (div-poly base (make-poly (variable base) (list 'sparse (make-term 0 co-arg)))))))

(define (gcd-poly args)
  (let ((base (contents (car args))))
    (let ((co-arg (apply gcd (coeff-list base))))
      (div-poly base (make-poly (variable base) (list 'sparse (make-term 0 co-arg)))))))

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
            ; (mul-poly arg1
            ;           (make-poly (variable arg1)
            ;                      (list 'sparse 
            ;                            (make-term 0 (tag arg2)))))
            
            (let ((coeff-list-p1 (coeff-list arg1)) 
                  (coeff-list-p2 (coeff-list arg2)))
              ; (display "coeff-list-p1:\n")
              ; (display coeff-list-p1)
              ; (newline)
              ; (display "coeff-list-p2:\n")
              ; (display coeff-list-p2)
              ; (newline)
              (let ((multi-t1 (mod-gcd coeff-list-p1))
                    (multi-t2 (mod-gcd coeff-list-p2)))
                ; (display "multi-t1:\n")
                ; (display multi-t1)
                ; (newline)
                ; (display "multi-t2:\n")
                ; (display multi-t2)
                ; (newline)
                (let ((arg-p1 (div-poly arg1 
                                        (make-poly (variable arg1) 
                                                   (list 'sparse (make-term 0 (tag multi-t1))))))
                      (arg-p2 (div-poly arg2
                                        (make-poly (variable arg2) 
                                                   (list 'sparse (make-term 0 (tag multi-t2)))))))
                  ; (display "arg-p1:\n")
                  ; (display arg-p1)
                  ; (newline)
                  ; (display "arg-p2:\n")
                  ; (display arg-p2)
                  ; (newline)
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
    ;            (display "div-result: ")
    ;            (display div-result)
    ;            (newline)
    ; (display (empty-termlist? (term-list (cadr div-result))))
    ; (newline)
    ;            (display "real-result: \n")
    (if (empty-termlist? (term-list (cadr div-result)))
        (let ((result (car div-result)))
          (if (= (order (first-term (term-list result))) 0)
              (coeff (first-term (term-list result)))
              result))
        div-result)))



(define (=zero?-poly p)
  (or (empty-termlist? (term-list p))
      (= (accumulate add 0 (map coeff (term-list p))) 0)
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
     (lambda (p1 p2) (let ((div-result (div-poly p1 p2)))
                       (if (pair? div-result)
                           (if (= (length div-result) 2)
                               (map tag div-result)
                               (tag div-result))
                           div-result))))
(put 'make 'polynomial
     (lambda (var terms) (tag (make-poly var terms))))

;; the result is such a mess
;; I'd like to print the result in a better way:
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
                                    
                                    (cond [(and (number? revised-coeff)(= revised-coeff 1) (= order-first 0)) (list revised-coeff)]
                                          [(and (number? revised-coeff) (= revised-coeff 1) (= order-first 1)) (list var)]
                                          [(and (number? revised-coeff) (= revised-coeff 1)) (list var '^ order-first)]
                                          [(= order-first 0) (list revised-coeff)]
                                          [(= order-first 1) (list revised-coeff '* var)]
                                          [else (list revised-coeff '* var '^ order-first)]))
                        (iter rest-t))))))
      ; (iter termlists)
      (cdr (iter termlists))
      )))

(install-term-package)
(install-sparse-package)
(install-dense-package)
(install-arithmetic)
(install-coercion)
(install-polynomial-package)


(define (poly-sonar p)
  (accumulate 
   (lambda (x y) (begin (display (mod-print x)) (display ":\n") (display x) (newline) y))
   (newline)
   (map coeff (reverse (cdr (term-list p))))))

(define poly-1 (make-poly 'x '(sparse (term 0 2))))
(define poly-2 (make-poly 'x '(sparse (term 1 4) (term 0 2))))
(define poly-3 (make-poly 'x '(sparse (term 0 3))))
(define poly-4 (make-poly 'x '(sparse (term 6 2) (term 5 1) (term 4 2) (term 3 3))))
(define poly-5 (make-poly 'y '(sparse (term 1 1) (term 0 2))))
(define poly-6 (make-poly 'y '(sparse (term 0 5))))
(define poly-7 (mul-poly poly-2 poly-5))
(define poly-8 (mul-poly poly-4 poly-7))
(define poly-9 (mul-poly poly-7 poly-8))
(define poly-10 (make-poly 'x '(sparse (term 6 8) (term 5 7) (term 4 6) (term 3 3))))
(define poly-11 (make-poly 'y '(sparse (term 1 3) (term 0 2))))
(define poly-12 (mul-poly poly-10 poly-11))
(define poly-13 (make-poly 'y '(sparse (term 10 1))))
(define poly-14 (make-poly 'x '(sparse (term 0 (polynomial y sparse (term 6 1))))))
(define poly-15 (make-poly 'x '(sparse (term 1 1))))
(define poly-17 (make-poly 'x '(sparse (term 0 (polynomial x sparse (term 6 1))))))
(mul-poly poly-14 poly-15)

(print-poly poly-8)
(add-poly poly-8 poly-13)
(print-poly (add-poly poly-8 poly-13))
(car (order-zero (add-poly poly-8 poly-13)))
(mod-print (car (order-zero (add-poly poly-8 poly-13))))

(contents (car (order-zero (add-poly poly-8 poly-13))))

(simplify-addition (contents (car (order-zero (add-poly poly-8 poly-13)))) 'y)
(simplify-addition poly-17 'x)

(simplify-addition '(x sparse (term 7 (polynomial y sparse (term 1 8) (term 0 16))) (term 6 (polynomial y sparse (term 1 8) (term 0 16))) (term 5 (polynomial y sparse (term 1 10) (term 0 20))) (term 4 (polynomial y sparse (term 1 16) (term 0 32))) (term 3 (polynomial y sparse (term 1 6) (term 0 12)))) 'y)
(add-poly-pro poly-8 poly-13)
