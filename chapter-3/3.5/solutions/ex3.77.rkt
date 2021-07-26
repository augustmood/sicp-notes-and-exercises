#lang sicp
(#%require "stream.rkt")

; The integral procedure used above was analogous to the "implicit" definition 
; of the infinite stream of integers in section 3.5.2. Alternatively, we can 
; give a definition of integral that is more like integers-starting-from 
; (also in section 3.5.2):

; (define (integral integrand initial-value dt)
;   (cons-stream initial-value
;                (if (stream-null? integrand)
;                    the-empty-stream
;                    (integral (stream-cdr integrand)
;                              (+ (* dt (stream-car integrand))
;                                 initial-value)
;                              dt))))

; When used in systems with loops, this procedure has the same problem as does 
; our original version of integral. Modify the procedure so that it expects the 
; integrand as a delayed argument and hence can be used in the solve procedure 
; shown above.

(define (integral integrand initial-value dt)
  (cons-stream initial-value
               (let ([integrand (force integrand)])
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

;; test

(define (solve f y0 dt)
  (let ([y 'null]
        [dy 'null])
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))

(stream-ref (solve (lambda (y) y) 1 0.001) 1000) ;; 2.716923932235896
