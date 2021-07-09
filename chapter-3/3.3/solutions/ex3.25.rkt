#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)

(provide (all-defined-out))

; Generalizing one- and two-dimensional tables, show how to implement a table in
; which values are stored under an arbitrary number of keys and different values
; may be stored under different numbers of keys. The `lookup` and `insert!`
; procedures should take as input a list of keys used to access the table.

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))

    (define (assoc key records)
      (cond [(or (null? records) (not (pair? records))) false]
            [(same-key? key (caar records)) (car records)]
            [else (assoc key (cdr records))]))
    
    (define (lookup . args)
      (let ([arg-table local-table])
        (define (lookup-iter args)
          (if (null? args)
              (cdr arg-table)
              (begin (set! arg-table (assoc (car args) (cdr arg-table)))
                      (and arg-table (lookup-iter (cdr args))))))
        (lookup-iter args)))
    
    (define (insert! . args)
      (let ([arg-table local-table]
            [prev-table '()]
            [arg-value (car (reverse args))]
            [arg-key (cadr (reverse args))]
            [table-keys (reverse (cddr (reverse args)))])
        (define (insert!-iter args)
          (display arg-table)
          (display " <- arg-table\n")
          (if (null? args)
              (let ([record (assoc arg-key (cdr arg-table))])
                (if record
                    (set-cdr! record arg-value)
                    (set-cdr! arg-table
                              (cons (cons arg-key arg-value)
                                    (cdr arg-table)))))
              (let ([temp (assoc (car args) (cdr arg-table))])
                (if temp
                    (and (set! arg-table temp) 
                         (insert!-iter (cdr args)))
                    (begin
                      (set-cdr! arg-table 
                                (append 
                                 (append 
                                  (accumulate (lambda (x y) (list (cons x y)))
                                              (list (cons arg-key arg-value))
                                              args))
                                 (cdr arg-table))))))))
        (insert!-iter table-keys)))
    
    (define (dispatch m)
      (cond [(eq? m 'lookup-proc) lookup]
            [(eq? m 'insert-proc!) insert!]
            [else (error "Unknown operation -- TABLE" m)]))
    dispatch))
