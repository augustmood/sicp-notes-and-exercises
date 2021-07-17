#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; a

(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell)
  (set-car! cell false))
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (make-semaphore n)
  (let ([cell 0]
        [mutex (make-mutex)])
    (define (the-semaphore m)
      (cond [(eq? m 'acquire)
             (mutex 'acquire)
             (if (< cell n)
                 (begin (set! cell (+ cell 1))
                        (mutex 'release))
                 (begin (mutex 'realease) 
                        (the-semaphore 'acquire)))] ; retry
            [(eq? m 'release)
             (mutex 'acquire)
             (if (>= cell 1) 
                 (set! cell (- cell 1)))
             (mutex 'release)]))
    the-semaphore))

;; b

; (define (make-semaphore n)
;   (let ([counter 0]
;         [cell (list false)])
;     (define (the-semaphore m)
;       (cond [(eq? m 'acquire)
;              (if (and (set! counter (+ counter 1)) 
;                       (>= counter n)
;                       (test-and-set! cell))
;                  (begin (set! counter n) 
;                         (the-semaphore 'acquire)))] ; retry
;             [(eq? m 'release) (clear! cell) (set! counter (- counter 1))])
;     the-semaphore))

; (define (clear! cell)
;   (set-car! cell false))
; (define (test-and-set! cell)
;   (if (car cell)
;       true
;       (begin (set-car! cell true)
;              false)))
