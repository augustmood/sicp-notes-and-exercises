#lang sicp
(#%provide (all-defined))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (list-stream . args)
    (if (null? args)
        the-empty-stream
        (cons-stream (car args)
            (apply list-stream (cdr args)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (display x)
  (newline))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (show-stream seq n)
  (stream-for-each 
   (lambda (i) (display (stream-ref seq i)) (newline))
   (stream-enumerate-interval 0 (- n 1))))

(define (general-stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply general-stream-map
              (cons proc (map stream-cdr argstreams))))))
