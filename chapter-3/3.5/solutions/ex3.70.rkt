#lang sicp
(#%require "stream.rkt")
(#%provide (all-defined))

;; *We will require that the weighting function be such that the weight of a 
;; *pair increases as we move out along a row or down along a column of the 
;; *array of pairs.

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream s1car 
                               (merge-weighted (stream-cdr s1) s2 weight)))
                 ((> (weight s1car) (weight s2car))
                  (cons-stream s2car 
                               (merge-weighted 
                                s1 
                                (stream-cdr s2) 
                                weight)))
                 (else
                  (cons-stream s1car 
                               (merge-weighted 
                                (stream-cdr s1) 
                                s2 
                                weight))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define (show-stream-w-weight seq weight n)
  (define counter 0)
  (stream-for-each
   (lambda (i) 
     (let ([pair (stream-ref seq i)])
       (display pair)
       (display " counter: ") 
       (display counter)
       (display " weight: ")
       (display (weight pair))
       (set! counter (+ counter 1))
       (newline)))
   (stream-enumerate-interval 0 (- n 1))))

;; a

(define weight-fn-1 (lambda (x) (+ (car x) (cadr x))))
(define stream-a (weighted-pairs integers integers weight-fn-1))

;; b
(define weight-fn-2 
  (lambda (x) (let ([i (car x)] [j (cadr x)]) (+ (* 2 i) (* 3 j) (* 5 i j)))))
(define (not-divisible? x y) (not (= (remainder x y) 0)))

; (define and-general 
;   (lambda 
;       (seq) 
;     (accumulate (lambda (x y) (and x y)) #t seq)))

; (define cond-b (lambda (p)
;                  (let ([i (car p)]
;                        [j (cadr p)])
;                    (and-general
;                     (map and-general
;                          (map (lambda (i) 
;                                 (map (lambda (f) (f i))
;                                      (map (lambda (n) 
;                                             (lambda (x) (not-divisible? x n))) 
;                                           '(2 5 3)))) (list i j)))))))

;; the above `cond-b` also works.

(define cond-b (lambda (p)
                 (let ([i (car p)]
                       [j (cadr p)])
                   (and (not-divisible? i 2) (not-divisible? i 3)
                        (not-divisible? i 5) (not-divisible? j 2)
                        (not-divisible? j 3) (not-divisible? j 5)))))

(define stream-b 
  (stream-filter 
   cond-b 
   (weighted-pairs 
    integers 
    integers 
    weight-fn-2)))

;; test

; (show-stream-w-weight stream-a weight-fn-1 20)
; (show-stream-w-weight stream-b weight-fn-2 20)
