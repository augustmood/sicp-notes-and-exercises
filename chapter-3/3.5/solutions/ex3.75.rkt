#lang sicp
(#%require "stream.rkt")

(define sense-data (list-stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))

(define (sign-change-detector curr-val prev-val)
  (cond [(and (< curr-val 0) (> prev-val 0)) -1]
        [(and (> curr-val 0) (< prev-val 0)) 1]
        [else 0]))

; (define (make-zero-crossings 
;          input-stream last-value last-avp)
;       (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
;         (cons-stream
;          (sign-change-detector avpt last-avp)
;          (make-zero-crossings 
;           (stream-cdr input-stream) (stream-car input-stream) avpt))))

;; the original one has another problem: once we reached the end of the
;; input-stream, it will still try to call `stream-car` on it instead of
;; returning an empty stream.

(define (make-zero-crossings 
         input-stream last-value last-avp)
  (if (stream-null? input-stream)
      the-empty-stream
      (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
        (cons-stream
         (sign-change-detector avpt last-avp)
         (make-zero-crossings 
          (stream-cdr input-stream) (stream-car input-stream) avpt)))))

(define zero-crossings (make-zero-crossings sense-data 0 0))

(display-stream zero-crossings)
