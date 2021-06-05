#lang racket 
(require sicp-pict)
(provide (all-defined-out))

(define des-frame (list (make-segment (make-vect 0 0) (make-vect 1 0))
                        (make-segment (make-vect 1 0) (make-vect 1 1))
                        (make-segment (make-vect 1 1) (make-vect 0 1))
                        (make-segment (make-vect 0 1) (make-vect 0 0))))

(paint (segments->painter des-frame))

(define cross (list (make-segment (make-vect 0 0) (make-vect 1 1))
                    (make-segment (make-vect 1 0) (make-vect 0 1))))

(paint (segments->painter cross))

(define diamond (list (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
                      (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
                      (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
                      (make-segment (make-vect 0 0.5) (make-vect 0.5 0))))

(paint (segments->painter diamond))

(define wave (list (make-segment (make-vect 0 0.85) (make-vect 0.2 0.6))
                   (make-segment (make-vect 0.2 0.6) (make-vect 0.3 0.65))
                   (make-segment (make-vect 0.3 0.65) (make-vect 0.35 0.65))
                   (make-segment (make-vect 0.35 0.65) (make-vect 0.45 0.65))
                   (make-segment (make-vect 0.45 0.65) (make-vect 0.4 0.8))
                   (make-segment (make-vect 0.4 0.8) (make-vect 0.45 1))
                   (make-segment (make-vect 0.45 1) (make-vect 0.65 1))
                   (make-segment (make-vect 0.65 1) (make-vect 0.7 0.8))
                   (make-segment (make-vect 0.7 0.8) (make-vect 0.65 0.65))
                   (make-segment (make-vect 0.65 0.65) (make-vect 0.7 0.65))
                   (make-segment (make-vect 0.7 0.65) (make-vect 1 0.3))
                   (make-segment (make-vect 1 0.3) (make-vect 1 0.15))
                   (make-segment (make-vect 1 0.15) (make-vect 0.6 0.45))
                   (make-segment (make-vect 0.6 0.45) (make-vect 0.75 0))
                   (make-segment (make-vect 0.75 0) (make-vect 0.65 0))
                   (make-segment (make-vect 0.65 0) (make-vect 0.55 0.2))
                   (make-segment (make-vect 0.55 0.2) (make-vect 0.45 0))
                   (make-segment (make-vect 0.45 0) (make-vect 0.35 0))
                   (make-segment (make-vect 0.35 0) (make-vect 0.4 0.5))
                   (make-segment (make-vect 0.4 0.5) (make-vect 0.35 0.55))
                   (make-segment (make-vect 0.35 0.55) (make-vect 0.2 0.4))
                   (make-segment (make-vect 0.2 0.4) (make-vect 0 0.65))
                   (make-segment (make-vect 0 0.65) (make-vect 0 0.85))))

(paint (segments->painter wave))