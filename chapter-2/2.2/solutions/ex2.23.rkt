#lang sicp

(define (for-each f l)
  (define (iter l)
    (if (null? (cdr l))
        (f (car l))
        (begin (f (car l))
               (iter (cdr l)))))
  (iter l))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))