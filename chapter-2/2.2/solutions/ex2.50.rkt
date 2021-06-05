#lang sicp
(#%require sicp-pict)
(#%require "ex2.49.rkt")



;
;           ^
;           |
;   edge2   |        
;           |
;           +--------->     
;             edge 1
;           



(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (rotate90 (rotate90 painter)))

(define (rotate270 painter)
  (rotate180 (rotate90 painter)))

(paint (flip-horiz (segments->painter wave)))
(paint (rotate180 (segments->painter wave)))
(paint (rotate270 (segments->painter wave)))