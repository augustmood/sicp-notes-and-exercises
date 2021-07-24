#lang sicp
(#%require "stream.rkt")

; Write a procedure triples that takes three infinite streams, S, T, and U, and 
; produces the stream of triples (Si,Tj,Uk) such that i < j < k. Use triples to 
; generate the stream of all Pythagorean triples of positive integers, i.e., the
; triples (i,j,k) such that i < j and i^2 + j^2 = k^2.

(define (filter predicate sequence)
  (cond [(null? sequence) nil]
        [(predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence)))]
        [else (filter predicate (cdr sequence))]))

(define (interleave . args)
  (let ([args (filter (lambda (s) (not (stream-null? s))) args)])
    (cond [(null? args) (the-empty-stream)]
          [(null? (cdr args)) (car args)]
          [else (cons-stream 
                 (stream-car (car args))
                 (apply interleave 
                        (append (cdr args) 
                                (list (stream-cdr (car args))))))])))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (show-stream seq n)
  (define counter 0)
  (stream-for-each
   (lambda (i) 
     (let ([pair (stream-ref seq i)])
       (display pair)
       (display " ") 
       (display counter)
       (display " ")
       (set! counter (+ counter 1))
       (newline)))
   (stream-enumerate-interval 0 (- n 1))))

(define pythagorean-triples 
  (stream-filter 
   (lambda (i) (let ([x (car i)]
                     [y (cadr i)]
                     [z (caddr i)])
                 (= (+ (expt x 2) (expt y 2)) (expt z 2))))
   (triples integers integers integers)))

;; test

(show-stream (triples integers integers integers) 50)
(show-stream pythagorean-triples 5)
