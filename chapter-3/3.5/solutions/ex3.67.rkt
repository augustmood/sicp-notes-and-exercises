#lang sicp
(#%require "stream.rkt")

; Modify the pairs procedure so that (pairs integers integers) will produce the 
; stream of all pairs of integers (i,j) (without the condition i < j). Hint: You
; will need to mix in an additional stream.

(define (filter predicate sequence)
  (cond [(null? sequence) nil]
        [(predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence)))]
        [else (filter predicate (cdr sequence))]))

;; version - 1

; (define (interleave s1 s2)
;   (if (stream-null? s1)
;       s2
;       (cons-stream (stream-car s1)
;                    (interleave s2 (stream-cdr s1)))))

; (define (pairs s t)
;   (cons-stream
;    (list (stream-car s) (stream-car t))
;    (interleave
;     (stream-map (lambda (x) (list (stream-car s) x))
;                 (stream-cdr t))
;     (pairs (stream-cdr s) t))))

;; version - 2

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
    (stream-map (lambda (x) (list x (stream-car t)))
                (stream-cdr s))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; test

(define (show-stream seq n)
  (define counter 0)
  (stream-for-each
   (lambda (i) 
     (let ([pair (stream-ref seq i)])
       (let ([x (car pair)]
             [y (cadr pair)])
         (display pair)
         (display " ") 
         (display counter)
         (display " ")
         (set! counter (+ counter 1))
         (newline))))
   (stream-enumerate-interval 0 (- n 1))))

(show-stream (pairs integers integers) 198)
