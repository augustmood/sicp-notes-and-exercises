#lang racket

(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-point x-point y-point)
  (cons x-point y-point))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment seg)
  (let ((x (/ (+ (x-point (start-segment seg)) (x-point (end-segment seg))) 2.0))
        (y (/ (+ (y-point (start-segment seg)) (y-point (end-segment seg))) 2.0)))
    (make-point x y)))

;; version 1:

; (define (make-rectangle width length)
;     (cons width length))

; (define (width rec)
;     (car rec))

; (define (length rec)
;     (cdr rec))


;; version 2:

(define (make-rectangle seg-diagonal)
    (let ((start (car seg-diagonal))
        (end (cdr seg-diagonal)))
        (cons (abs (- (x-point start) (x-point end)))
            (abs (- (y-point start) (y-point end))))))

(define (width rec)
    (car rec))

(define (length rec)
    (cdr rec))

;;;;;;;

(define (perimeter rec)
    (* 2 (+ (width rec) (length rec))))

(define (area rec)
    (* (width rec) (length rec)))

; (define rec-1 (make-rectangle 20 30))
; (perimeter rec-1)
; (area rec-1)

(define rec-2 (make-rectangle (make-segment (make-point 0 0) (make-point 100 100))))

(perimeter rec-2)
(area rec-2)
