#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)
; (require "queue.rkt")

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (front-pointer) front-ptr)
    (define (rear-pointer) rear-ptr)
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue" (front-pointer))
          (car (front-pointer))))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               (front-pointer))
              (else
               (set-cdr! (rear-pointer) new-pair)
               (set-rear-ptr! new-pair)
               (front-pointer)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue" (front-pointer)))
            (else
             (set-front-ptr! (cdr (front-pointer)))
             (front-pointer))))
    (define (dispatch m)
      (cond [(eq? m 'front-ptr) (front-pointer)]
            [(eq? m 'rear-ptr) (rear-pointer)]
            [(eq? m 'set-front-ptr!) (lambda (item) (set-front-ptr! item))]
            [(eq? m 'set-rear-ptr!) (lambda (item) (set-rear-ptr! item))]
            [(eq? m 'empty-queue?) (empty-queue?)]
            [(eq? m 'front-queue) (front-queue)]
            [(eq? m 'insert-queue!) (lambda (item) (insert-queue! item))]
            [(eq? m 'delete-queue!) (delete-queue!)]
            [else (error "Undefined operation: MAKE-QUEUE" m)]))
    dispatch))

(define q1 (make-queue))
((q1 'insert-queue!) 'a) ;; (a)
((q1 'insert-queue!) 'b) ;; (a b)
(q1 'empty-queue?)       ;; #f
(q1 'delete-queue!)      ;; (b)
(q1 'delete-queue!)      ;; ()
(q1 'empty-queue?)       ;; #t
