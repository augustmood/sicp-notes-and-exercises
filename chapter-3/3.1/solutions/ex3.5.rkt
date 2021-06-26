#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (test x1 x2 y1 y2)
  (let ([curr-x (random-in-range x1 x2)]
        [curr-y (random-in-range y1 y2)]
        [pos-x (/ (+ x1 x2) 2)]
        [pos-y (/ (+ y1 y2) 2)]
        [radius (abs (/ (- x1 x2) 2))])
    ;     (display radius)
    ;     (newline)
    ; (display (expt (- curr-x pos-x) 2))
    ;     (newline)
    ; (display (expt (- curr-y pos-y) 2))
    ;     (newline)
    (< (sqrt (+ (expt (- curr-x pos-x) 2) (expt (- curr-y pos-y) 2))) radius)))

(define (estimate-integral pred x1 x2 y1 y2 trials)
  (define (experiment)
    (test x1 x2 y1 y2))
  (monte-carlo trials experiment))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-pi)
  (let ([x1 (random 100000)]
        [y1 (random 100000)]
        [radius (random 100000)])
    (let ([x2 (+ x1 radius)]
          [y2 (+ y1 radius)])
      (* 4.0 (estimate-integral test x1 x2 y1 y2 100000)))))

(estimate-pi)
