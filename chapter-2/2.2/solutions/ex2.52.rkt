#lang racket 
(require sicp-pict)
(require "ex2.49.rkt")

;; a

(define new-wave (append wave (list (make-segment (make-vect 0.0 1.0) (make-vect 0.15 1.0))
                                    (make-segment (make-vect 0.15 1.0) (make-vect 0.15 0.85))
                                    (make-segment (make-vect 0.15 0.85) (make-vect 0.0 0.85))
                                    (make-segment (make-vect 0.0 0.85) (make-vect 0.0 1.0)))))

(paint (segments->painter new-wave))

;; b

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right corner))))))

;; c

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four identity  flip-horiz
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))