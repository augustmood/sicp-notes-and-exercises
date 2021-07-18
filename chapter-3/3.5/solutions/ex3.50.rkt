#lang racket
(require sicp)

; Complete the following definition, which generalizes stream-map to allow 
; procedures that take multiple arguments, analogous to map in section 2.2.3, 
; footnote 12.

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))
