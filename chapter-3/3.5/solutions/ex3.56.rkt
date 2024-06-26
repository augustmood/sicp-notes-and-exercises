#lang sicp
(#%require "stream.rkt")

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

; The required stream may be constructed with merge, as follows:

; (define S (cons-stream 1 (merge <??> <??>)))

; Fill in the missing expressions in the places marked <??> above.

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define S (cons-stream 1 (merge (scale-stream S 2) 
                                (merge (scale-stream S 3) 
                                       (scale-stream S 5)))))

(show-stream S 10)
