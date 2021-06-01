#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


;; Asssume it's ascending order i < j < k:

(define (ordered-triples n)
  (flatmap (lambda (k) (flatmap (lambda (j) 
                                  (map (lambda (i) (list i j k)) 
                                       (enumerate-interval 1 (- j 1))))
                                (enumerate-interval 1 (- k 1))))
           (enumerate-interval 1 n)))

;; It just came to me that we could start by implmenting of generating ordered-pairs, and then 
;; implmenting of generating ordered-triples.

; (define (ordered-pairs n)
;   (flatmap (lambda (b) (map (lambda (a) (list a b)) 
;                                         ;; we could use (cons a b) here and replaced 
;                                         ;; (enumerate-interval 1 n) by (map (lambda (i) (list i)) 
;                                         ;;                                (enumerate-interval 1 n))
;                             (enumerate-interval 1 (- b 1)))) 
;            (enumerate-interval 1 n)))

; (define (ordered-triples n)
;   (flatmap (lambda (b) (map (lambda (a) (cons a b))
;                             (enumerate-interval 1 (- (car b) 1))))
;            (ordered-pairs n)))

; (define (ordered-quadruples n)
;   (flatmap (lambda (b) (map (lambda (a) (cons a b))
;                             (enumerate-interval 1 (- (car b) 1))))
;            (ordered-triples n)))

(define (ordered-sequences length n)
  (if (= length 1)
      (map (lambda (i) (list i)) (enumerate-interval 1 n))
      (flatmap (lambda (b) (map (lambda (a) (cons a b))
                                (enumerate-interval 1 (- (car b) 1))))
               (ordered-sequences (- length 1) n))))

(define (triple-sum-filter s seq)
  (filter (lambda (se) (= (+ (car se) (cadr se) (caddr se)) s)) seq))

(define (unique-triples n s)
  (triple-sum-filter s (ordered-sequences 3 n)))

(unique-triples 6 10)