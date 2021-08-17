#lang sicp

(define (f y) (* y y))
(define (g f y) (f (f y) (f y)))

;; if we do not use `actual-value`, the f that passed in the foo, will be the
;; thunks, which can not used as functions.
