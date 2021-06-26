#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)

(define random-init 0)
(define (rand-update i)
  (+ i 1)) ;; using this for test

(define rand
  (let ([x random-init])
    (define (dispatch m)
      (cond [(eq? m 'generate) (begin (set! x (rand-update x))
                                      x)]
            [(eq? m 'reset) (lambda (i) (set! x i))]
            [else (error "RAND--UNKNOWN REQUESTS")]))
    dispatch))

(rand 'generate) ; 1
(rand 'generate) ; 2
(rand 'generate) ; 3
(rand 'generate) ; 4
(rand 'generate) ; 5
((rand 'reset) 100) ; set x/random-init to 100
(rand 'generate) ; 101
(rand 'generate) ; 102
