#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(provide (all-defined-out))

; Generalizing one- and two-dimensional tables, show how to implement a table in
; which values are stored under an arbitrary number of keys and different values
; may be stored under different numbers of keys. The `lookup` and `insert!`
; procedures should take as input a list of keys used to access the table.

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond [(null? records) false]
            [(same-key? key (caar records)) (car records)]
            [else (assoc key (cdr records))]))
    
    (define (lookup . args)
      (define (lookup-tables args)
        (cond [(= (length args) 1)
               (cons local-table (car args))]
              [(= (length args) 2)
               (cons (assoc (car args) (cdr local-table)) (cadr args))]
              [else (error "Arity mismatch -- TABLE-LOOKUP")] ))
      (let ([subtable (car (lookup-tables args))]
            [record-key (cdr (lookup-tables args))])
        (if subtable
            (let ((record (assoc record-key (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    
    (define (insert! . args)
      (let ([arg-table '()]
            [record '()]
            [value '()]
            [key-1 '()]
            [key-2 '()])
        (cond [(= (length args) 2)
               (begin (set! key-2 (car args))
                      (set! arg-table local-table)
                      (set! value (cadr args)))]
              [(= (length args) 3)
               (begin (set! key-1 (car args))
                      (set! key-2 (cadr args))
                      (set! arg-table (assoc key-1 (cdr local-table)))
                      (set! value (caddr args)))]
              [else (error "Arity mismatch -- TABLE-INSERT!")])
        (if arg-table
            (begin (set! record (assoc key-2 (cdr arg-table)))
                   (if record
                       (set-cdr! record value)
                       (set-cdr! arg-table
                                 (cons (cons key-2 value)
                                       (cdr arg-table)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table))))))
    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    
    dispatch))
