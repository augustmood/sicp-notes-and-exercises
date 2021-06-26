#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)

; (define (make-accumulator sum)
;     (lambda (i)
;       (begin (set! sum (+ sum i))
;              sum)))

(define make-accumulator
  (lambda (i)
    (let ((sum i))
      (lambda (arg)
        (begin (set! sum (+ sum arg))
               sum)))))

(define A (make-accumulator 5))
(A 10)
(A 10)
