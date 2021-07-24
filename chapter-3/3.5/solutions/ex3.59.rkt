#lang sicp
(#%require "stream.rkt")
(#%provide (all-defined))

;; a

(define (integrate-series s)
  (general-stream-map / s integers))

;; b

(define (scale-stream stream factor)
  (general-stream-map (lambda (x) (* x factor)) stream))

(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
