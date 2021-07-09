#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)

(define (make-table)
  (list '*table*))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (set-cdr! table (cons (cons key value) (cdr table))))
;; revise the insert! as deleting the redundant assoc function call

(define (memoize f)
  ((lambda (table)
     (lambda (x)
       ((lambda (previously-computed-result)
          (or previously-computed-result
              ((lambda (result)
                 (insert! x result table)
                 result) (f x)))))
       (lookup x table)))
   (make-table)))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(memo-fib 3)
