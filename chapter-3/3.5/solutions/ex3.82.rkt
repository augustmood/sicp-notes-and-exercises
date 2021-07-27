#lang sicp
(#%require "stream.rkt")

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define random-init 3958)
(define rand-update (lambda (i) (random (+ i random-init))))
(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

(define (test x1 x2 y1 y2)
  (let ([curr-x (random-in-range x1 x2)]
        [curr-y (random-in-range y1 y2)]
        [pos-x (/ (+ x1 x2) 2)]
        [pos-y (/ (+ y1 y2) 2)]
        [radius (abs (/ (- x1 x2) 2))])
    (< (sqrt (+ (expt (- curr-x pos-x) 2) (expt (- curr-y pos-y) 2))) radius)))

(define (test-stream pred x1 x2 y1 y2)
  (map-successive-pairs (lambda (x y) (pred x1 x2 y1 y2))
                        random-numbers))

(define (estimate-integral pred x1 x2 y1 y2)
  (monte-carlo (test-stream pred x1 x2 y1 y2) 0 0))

(define (estimate-pi)
  (let ([x1 (random 100000)]
        [y1 (random 100000)]
        [radius (random 100000)])
    (let ([x2 (+ x1 radius)]
          [y2 (+ y1 radius)])
      (stream-map (lambda (i) (* i 4.0)) 
                  (estimate-integral test x1 x2 y1 y2)))))

(stream-ref (estimate-pi) 10000)
