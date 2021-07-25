#lang sicp
(#%require "stream.rkt")

(define sense-data (list-stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))

(define (sign-change-detector curr-val prev-val)
  (cond [(and (< curr-val 0) (> prev-val 0)) -1]
        [(and (> curr-val 0) (< prev-val 0)) 1]
        [else 0]))

(define (make-zero-crossings input-stream last-value) 
  (cons-stream
   (sign-change-detector
    (stream-car input-stream)
    last-value)
   (make-zero-crossings
    (stream-cdr input-stream)
    (stream-car input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0))

(define zero-crossings-2
  (general-stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))

;; test
; (show-stream zero-crossings 13)
; (newline)
; (show-stream zero-crossings-2 13)
