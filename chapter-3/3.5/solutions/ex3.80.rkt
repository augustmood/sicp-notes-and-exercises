#lang sicp
(#%require "stream.rkt")

(define (add-streams s1 s2)
  (general-stream-map + s1 s2))

(define (scale-stream stream factor)
  (general-stream-map (lambda (x) (* x factor)) stream))

(define (integral integrand initial-value dt)
  (cons-stream initial-value
               (let ([integrand (force integrand)])
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

;; original one:

; (define (RLC R L C dt)
;   (lambda (v-c0 i-l0)
;     (define v-c (integral (delay d-vc) v-c0 dt))
;     (define i-l (integral (delay d-il) i-l0 dt))
;     (define d-vc (scale-stream i-l (/ -1 C)))
;     (define d-il (add-streams (scale-stream v-c (/ 1 L))
;                               (scale-stream i-l (/ (- R) L))))
;     (cons v-c i-l)))

;; the above codes cannot work in racket, so I had to write it in such a way:

(define (RLC R L C dt)
  (lambda (v-c0 i-l0)
    (let ([v-c nil]
          [i-l nil]
          [d-vc nil]
          [d-il nil])
      (set! v-c (integral (delay d-vc) v-c0 dt))
      (set! i-l (integral (delay d-il) i-l0 dt))
      (set! d-vc (scale-stream i-l (/ -1 C)))
      (set! d-il (add-streams (scale-stream v-c (/ 1 L))
                                (scale-stream i-l (/ (- R) L))))
      (general-stream-map cons v-c i-l))))

(define rlc-stream ((RLC 1 1 0.2 0.1) 10 0))
(show-stream rlc-stream 10)
