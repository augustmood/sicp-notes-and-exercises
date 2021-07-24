#lang sicp
(#%require "stream.rkt")
(#%provide (all-defined))

(define (square x)
    (* x x))

(define (scale-stream stream factor)
  (general-stream-map (lambda (x) (* x factor)) stream))

(define (partial-sums s)
  (let ([sum 0])
    (stream-map (lambda (i) (set! sum (+ i sum)) sum) s)))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))
