#lang sicp
(#%require "interface.rkt")
(#%require "operators.rkt")
(#%require "arithmetic.rkt")
(#%require "tower-system.rkt")
(#%provide (all-defined))

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
                             (term-list p2))))
      ((= (order first-p1) 0)
       (add-poly (make-poly (variable p2) (term-list p1)) p2))
      
      ((= (order first-p2) 0)
       (add-poly p1 (make-poly (variable p1) (term-list p2))))
      
      (else (let ((zero-p1 (car (order-zero p1)))
                  (zero-p2 (car (order-zero p2)))
                  (no-zero-p1 (cadr (order-zero p1)))
                  (no-zero-p2 (cadr (order-zero p2))))
              (display "zero-p1: ")
              (display zero-p1)
              (newline)
              (display "zero-p2: ")
              (display zero-p2)
              (newline)
              (display "no-zero-p1: ")
              (display no-zero-p1)
              (newline)
              (display "no-zero-p2: ")
              (display no-zero-p2)
              (newline)
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
                       (display "p1: ")
                       (display p1)
                       (newline)   
                       (display "car-args: ")
                       (display (car args))
                       (newline)                                  
                       (display "p2: ")
                       (display p2)
                       (newline)
                       (display "cadr-args: ")
                       (display (tag (cadr args)))
                       (newline)     
                       (display "args: ")
                       (display args)
                       (newline)
                       (add-poly (car args)
                                 (make-poly (variable (car args))
                                            (list 'sparse (make-term 0 (tag (cadr args))))))))))))))



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
     (lambda (p1 p2) (map tag (div-poly p1 p2))))
(put 'make 'polynomial
     (lambda (var terms) (tag (make-poly var terms))))


(install-term-package)
(install-sparse-package)
(install-dense-package)
(install-arithmetic)
(install-coercion)

(define poly-1 (make-polynomial 'x '(sparse (term 6 2) (term 5 1) (term 4 2) (term 3 3))))
(define poly-2 (make-polynomial 'x 
                                '(sparse (term 5 2) (term 4 3) (term 3 1) 
                                         (term 2 5) (term 1 9) (term 0 10))))
(define poly-3 (make-polynomial 'x '(dense 2 1 2 3 0 0 0)))
(define poly-4 (make-polynomial 'x '(dense 2 3 1 5 9 7 9)))
(define poly-5 (make-polynomial 'x '(dense 2 3 1 5 9 10)))

(define pl-1 (make-poly 'x '(sparse (term 1 1) (term 0 2))))
(define pl-2 (make-poly 'y '(sparse (term 1 1) (term 0 2))))
(define pl-3 (make-poly 'x '(sparse (term 6 2) (term 5 1) (term 4 2) (term 3 3))))
(define pl-4 (make-poly 'y '(sparse (term 2 1) (term 1 1) (term 0 2))))
; (variable poly-1)
; poly-1
; (make-term 0 poly-1)
; (adjoin-term (make-term 0 poly-1) (the-empty-termlist))


; (adjoin-term '(term 0 poly-1) '(sparse))
; (add-poly (contents poly-1) (contents poly-2))
; (add-poly (contents poly-3) (contents poly-4))
; (add-poly (contents poly-1) (contents poly-4))


(add-poly pl-1 pl-2)
; '(y sparse (term 1 1) (term 0 (polynomial x sparse (term 1 1) (term 0 4))))

(add-poly (add-poly pl-1 pl-2) pl-3)

; (order-zero pl-2)
; (order-zero pl-4)
; (term-list poly-8)
; (first-term (term-list poly-8))
; (add-poly poly-8 poly-7)
; (add-poly (contents poly-5) poly-9)

; (define se-1 (make-poly 'y (list 'sparse (make-term 0 poly-6))))
; (coeff (first-term (term-list se-1)))
; (poly-conversion (coeff (first-term (term-list se-1))))
; (add 3 (poly-conversion (coeff (first-term (term-list se-1)))))


; (variable poly-7)
; poly-7
; (order-zero poly-7)

; zero-p1: 4
; zero-p2: 0
; no-zero-p1: (x sparse (term 6 2) (term 5 1) (term 4 2) (term 3 3) (term 1 1))
; no-zero-p2: (y sparse (term 1 1))
; p1: (x sparse (term 6 2) (term 5 1) (term 4 2) (term 3 3) (term 1 1) (term 0 4))
; car-args: (x sparse (term 6 2) (term 5 1) (term 4 2) (term 3 3) (term 1 1) (term 0 4))
; p2: (y sparse (term 1 1))
; cadr-args: (polynomial y sparse (term 1 1))
; args: ((x sparse (term 6 2) (term 5 1) (term 4 2) (term 3 3) (term 1 1) (term 0 4)) (y sparse (term 1 1)))
; (x sparse (term 6 2) (term 5 1) (term 4 2) (term 3 3) (term 1 1) (term 0 5))

; (define a '(x sparse (term 6 2) (term 5 1) (term 4 2) (term 3 3) (term 1 1) (term 0 4)))

; (variable '(x sparse (term 6 2) (term 5 1) (term 4 2) (term 3 3) (term 1 1) (term 0 4)))

; (make-poly (variable a)
;            (list 'sparse (make-term 0 (tag (polynomial y sparse (term 1 1))))))

; (term-list a)
; (term-list (make-poly (variable a) (adjoin-term (make-term 0 '(polynomial y sparse (term 1 1))) (the-empty-termlist))))

; (add-poly a (make-poly (variable a) (adjoin-term (make-term 0 '(polynomial y sparse (term 1 1))) (the-empty-termlist))))
; (add 4 '(polynomial y sparse (term 1 1)) )