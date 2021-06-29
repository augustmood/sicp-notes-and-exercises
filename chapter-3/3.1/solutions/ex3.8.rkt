#lang racket
(require sicp)

(define f
  (let ((num 0)
        (prev 0))
    (lambda (i)
      (begin
        (set! prev num)
        (set! num i)
        prev))))

; (+ (f 0) (f 1)) ;; 0
; (+ (f 1) (f 0)) ;; 1
