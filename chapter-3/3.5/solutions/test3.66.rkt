#lang sicp
(#%require "stream.rkt")

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define eq (lambda (x y)
             (let ([diff (- y x)])
               (+ (- (expt 2 x) 2)
                  (* (ceiling (/ (expt diff 2) (+ 1 (expt diff 2))))
                     (+ (expt 2 (- x 1)) (* (- y x 1) (expt 2 x))))))))

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
         (display (= counter (eq x y)))
         (set! counter (+ counter 1))
         (newline))))
   (stream-enumerate-interval 0 (- n 1))))

; (show-stream (pairs integers integers) 198)

(define (test)
  (let ([result #f]
        [counter 0]
        [seq (pairs integers integers)])
    (stream-for-each
     (lambda (i) 
       (let ([pair (stream-ref seq i)])
         (let ([x (car pair)]
               [y (cadr pair)])
           (set! result (= counter (eq x y)))
           (if (not result) (begin (display "Test Failed!")
                                   (display (eq x y))
                                   (display " is not equal to the counter: ")
                                   (display counter)))
           (set! counter (+ counter 1)))))
     (stream-enumerate-interval 0 10000))
    (display "Test Passed!\n")))

(test)
