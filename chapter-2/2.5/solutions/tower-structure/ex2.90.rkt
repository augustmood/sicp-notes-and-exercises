#lang sicp
(#%require "interface.rkt")
(#%require "operators.rkt")
(#%require "arithmetic.rkt")
(#%require "tower-system.rkt")
(install-arithmetic)

(define (install-term-package)
  (define (make-term order coeff) (list order coeff))
  (define (tag x) (attach-tag 'term x))
  (define (tag-exist? term) (and (= (length term) 3) (eq? (car term) 'term)))
  ; (define (order term) (if (tag-exist? term) (cadr term) (car term)))
  ; (define (coeff term) (if (tag-exist? term) (caddr term) (cadr term)))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (put 'make-term 'term (lambda (order coeff) (tag (make-term order coeff))))
  (put 'order '(term) order)
  (put 'coeff '(term) coeff))

(define (install-sparse-package)
  (define (adjoin-term term term-list)
    (let ((revised-term (make-term (car term) (cadr term))))
      (if (=zero? (coeff revised-term))
          term-list
          (cons revised-term term-list))))
  (define (first-term term-list)
    (make-term (order (car term-list)) (coeff (car term-list))))
  (define (rest-terms term-list) (cdr term-list))
  (define (tag x) (attach-tag 'sparse x))
  (put 'adjoin-term '(term sparse)
       (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'first-term '(sparse) (lambda (term-list) (first-term term-list))) ;; ?
  (put 'rest-terms '(sparse) (lambda (term-list) (tag (rest-terms term-list))))
  'done)


(define (install-dense-package)
  (define (iter n result)
    (if (= n 1)
        result
        (iter (- n 1) (cons 0 result))))
        
  (define (adjoin-term term term-list)
    (let ((revised-term (make-term (car term) (cadr term))))
      (if (=zero? (coeff revised-term))
          term-list
          (if (null? term-list)
              (cons (coeff revised-term) (iter (add 1 (order revised-term)) term-list))
              (let ((diff (- (order revised-term) (order (first-term term-list)))))
                (cons (coeff revised-term) (iter diff term-list)))))))
  
  (define (first-term term-list)
    (make-term (- (length term-list) 1) (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  
  (define (tag x) (attach-tag 'dense x))
  (define (term-tag x) (attach-tag 'term x))
  (put 'adjoin-term '(term dense)
       (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'first-term '(dense) (lambda (term-list) (first-term term-list)))
  (put 'rest-terms '(dense) (lambda (term-list) (tag (rest-terms term-list))))
  'done)

(install-sparse-package)
(install-dense-package)
(install-term-package)

(define (make-poly variable term-list)
  (cons variable term-list))
(define (variable p) (car p))
(define (term-list p) (cdr p))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (order term) (apply-generic 'order term))
(define (coeff term) (apply-generic 'coeff term))
(define (make-term order coeff) ((get 'make-term 'term) order coeff))


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
; (define (neg p)
;   (define (rec lst)
;     (if (empty-termlist? lst)
;         nil
;         (let ((first-t (first-term lst))
;               (rest-t (rest-terms lst)))
;           (cons (make-term (order first-t) (sub 0 (coeff first-t))) (rec rest-t)))))
;   (make-poly (variable p) (attach-tag 'sparse (rec (term-list p)))))

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

(define (neg p)
  (let ((var (variable p))
        (term-sets (term-list p)))
    (make-poly var 
               (attach-tag 'sparse (map (lambda (term)
                                          (make-term (order term) (sub 0 (coeff term))))
                                        (contents (dense->sparse term-sets)))))))

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

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
         (make-term (add (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

(define (sub-poly p1 p2)
  (add-poly p1 (neg p2)))

(define p1 (make-poly 'x '(sparse (term 6 2) (term 5 1) (term 4 2) (term 3 3))))
(define p2 
  (make-poly 'x '(sparse (term 5 2) (term 4 3) (term 3 1) (term 2 5) (term 1 9) (term 0 10))))
(define p3 (make-poly 'x '(dense 2 1 2 3 0 0 0)))
(define p4 (make-poly 'x '(dense 2 3 1 5 9 7 9)))
(define p5 (make-poly 'x '(dense 2 3 1 5 9 10)))

(define tl-1 (term-list p1))
(define tl-2 (term-list p2))
(define tl-3 (term-list p3))
(define tl-4 (term-list p4))
(define tl-5 (term-list p5))

; (adjoin-term (first-term (term-list p1)) (term-list p2))
(define tt-1 (first-term (term-list p1)))
(define tt-3 (first-term (term-list p3)))

tl-1
tl-2
tl-3
tl-4
tl-5
tt-1
tt-3

; (adjoin-term tt-1 tl-2)
; (adjoin-term tt-3 tl-5)
; (null? (contents '(dense)))
; (make-term 3 0)
; (coeff (make-term 3 0))
; (=zero? (coeff (make-term 3 0)))
; (adjoin-term (make-term 3 1) '(dense))

; (neg p1)
; (neg p2)
; (neg p3)
; (neg p4)

; tl-3
; (dense->sparse tl-3)

; (add-terms tl-1 tl-2)
; (add-terms tl-1 tl-4)

; (display "tl-2: ")
; (display tl-2)
; (newline)

; (display "tl-3: ")
; (display tl-3)
; (newline)

; (display "sparse-tl-3: ")
; (display (dense->sparse tl-3))
; (newline)

; (add-terms tl-3 tl-2)
; (add-terms tl-2 tl-3)
; (add-terms tl-3 tl-4)

; (add-poly p1 p2)
; (add-poly p1 p3)
; (add-poly p1 p4)
; (add-poly p2 p3)
; (add-poly p2 p4)
; (add-poly p3 p4)

; (sub-poly p1 p1)
; (sub-poly p1 p2)
; (sub-poly p1 p3)
; (sub-poly p1 p4)
; (sub-poly p2 p3)
; (sub-poly p2 p4)
; (sub-poly p3 p4)
