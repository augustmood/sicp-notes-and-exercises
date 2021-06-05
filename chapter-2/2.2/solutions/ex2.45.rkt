#lang sicp
(#%require sicp-pict)

;; Right-split and up-split can be expressed as instances of a general splitting operation. Define a 
;; procedure split with the property that evaluating

; (define right-split (split beside below))
; (define up-split (split below beside))

; (define (split f1 f2)
;   (lambda (n)
;     (lambda (painter)
;       (if (= n 0)
;           painter
;           (f1 painter (f2 painter painter))))))

(define (split f1 f2)
  (define (split-helper painter n)
    (if (= n 0)
        painter
        (let ((small (split-helper painter (- n 1))))
          (f1 painter (f2 small small)))))
  (lambda (painter n)
    (split-helper painter n)))

(define right-split (split beside below))
(define up-split (split below beside))

(paint (right-split einstein 10))
(paint (up-split einstein 10))