#lang sicp
(#%require "stream.rkt")

(define sense-data (list-stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))

(define (sign-change-detector curr-val prev-val)
  (cond [(and (< curr-val 0) (> prev-val 0)) -1]
        [(and (> curr-val 0) (< prev-val 0)) 1]
        [else 0]))

(define (smooth input-stream) 
  (general-stream-map (lambda (x y) (/ (+ x y) 2.0)) 
                      (stream-cdr input-stream)
                      input-stream))

; (define (smooth-zero-crossings input-stream)
;   (let ([smoothed (smooth input-stream)])
;     (general-stream-map sign-change-detector
;                         (stream-cdr smoothed)
;                         smoothed)))

;; or

(define (make-zero-crossings input-stream)
    (general-stream-map sign-change-detector
                        (stream-cdr input-stream)
                        input-stream))

(define (smooth-zero-crossings input-stream)
    (make-zero-crossings (smooth input-stream)))

;; test

(define zero-crossings (make-zero-crossings sense-data))
(display-stream zero-crossings)
