#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)

; A deque ("double-ended queue") is a sequence in which items can be inserted and deleted at either 
; the front or the rear. Operations on deques are the constructor make-deque, the predicate 
; empty-deque?, selectors front-deque and rear-deque, and mutators front-insert-deque!, 
; rear-insert-deque!, front-delete-deque!, and rear-delete-deque!. Show how to represent deques using 
; pairs, and give implementations of the operations.All operations should be accomplished in O(1) 
; steps.

; (define (make-deque) (cons '() '()))
; (define (empty-deque? deque) ...)
; (define (front-deque deque) ...)
; (define (rear-deque deque) ...)
; (define (front-insert-deque! deque) ...)
; (define (rear-insert-deque! deque) ...)
; (define (front-delete-deque! deque) ...)
; (define (rear-delete-deque! deque) ...)

(define a
    (cons (cons '() 'a) '()))

(define b
  (cons (cons '() 'b) '()))
(set-car! (car b) a)
(set-cdr! a b)

(define c
  (cons (cons '() 'c) '()))
(set-car! (car c) b)
(set-cdr! b c)

(define (make-node value)
    (cons (cons '() value) '()))

(define (next node)
    (cdr node))

(define (prev node)
    (caar node))

(define (val node)
    (cdar node))

(val a)
(val (next a))
(val (prev b))
(val (next b))
(val (prev c))


(define (make-deque) (cons '() '()))
(define (empty-deque? deque) (null? (front-deque deque)))
(define (front-deque deque) (car deque))
(define (rear-deque deque) (cdr deque))

