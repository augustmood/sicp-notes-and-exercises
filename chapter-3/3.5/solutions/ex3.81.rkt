#lang sicp
(#%require "stream.rkt")

(define rand-update inc)

(define (dispatch req val)
  (cond [(eq? req 'generate) (rand-update val)]
        [(and (pair? req) (eq? (car req) 'reset))
         (cdr req)]
        [else (error "RAND--UNKNOWN REQUESTS" req)]))

(define (random-generator requests rand-init)
  (define random-stream
    (cons-stream rand-init
                 (general-stream-map
                  dispatch
                  requests
                  random-stream)))
  random-stream)

;; test

(define reqs-1 (list-stream 'generate 'generate (cons 'reset 10) 
                            (cons 'reset 200) 'generate))
(define reqs-2 (list-stream 'generate 'generate (cons 'reset 10) 
                            (cons 'generate 200) 'generate))

(display-stream (random-generator reqs-1 0))
(display-stream (random-generator reqs-2 0))
