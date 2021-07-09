#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(provide (all-defined-out))

; To search a table as implemented above, one needs to scan through the list of
; records. This is basically the unordered list representation of section 2.3.3.
; For large tables, it may be more efficient to structure the table in a
; different manner. Describe a table implementation where the (key, value)
; records are organized using a binary tree, assuming that keys can be ordered
; in some way (e.g., numerically or alphabetically). (Compare exercise 2.66 of
; chapter 2.)

;; assume the key can be ordered in numerically;
;; each node on the tree will be like:
;;     +---+----+
;;     |key|data|
;;     +---+----+

(define (make-node key val) (cons key val))
(define (node-key node) (car node))
(define (node-val node) (cdr node))
(define (empty-node) (make-node null null))

(define (make-tree entry left right)
  (define (set-entry! v) (set! entry v))
  (define (set-left! v) (set! left v))
  (define (set-right! v) (set! right v))
  (define (dispatch m)
    (cond [(eq? m 'entry) entry]
          [(eq? m 'left-branch) left]
          [(eq? m 'right-branch) right]
          [(eq? m 'set-entry!) set-entry!]
          [(eq? m 'set-left!) set-left!]
          [(eq? m 'set-right!) set-right!]
          (else (error "Undefined operation -- MAKE-TREE" m))))
  dispatch)

(define (entry tree) (tree 'entry))
(define (left-branch tree) (tree 'left-branch))
(define (right-branch tree) (tree 'right-branch))

(define (set-entry! tree node)
  ((tree 'set-entry!) node))
(define (set-left! tree node)
  ((tree 'set-left!) node))
(define (set-right! tree node)
  ((tree 'set-right!) node))
(define (empty-tree) (make-tree (empty-node) null null))

(define (adjoin-tree x tree)
  (if (null? tree)
      (make-tree x '() '())
      (let ([x-key (node-key x)]
            [entry-key (node-key (entry tree))])
        (cond [(null? entry-key) (set-entry! tree x)]
              [(= x-key entry-key) (set-entry! tree x)]
              [(< x-key entry-key)
               (set-left! tree (adjoin-tree x (left-branch tree)))]
              [(> x-key entry-key)
               (set-right! tree (adjoin-tree x (right-branch tree)))])
               tree)))

(define (element-of-tree? x tree)
  (if (null? tree)
      false
      (let ([x-key (node-key x)]
            [entry-key (node-key (entry tree))])
        (cond [(null? entry-key) false]
              [(= x-key entry-key) true]
              [(< x-key entry-key)
               (element-of-tree? x (left-branch tree))]
              [(> x-key entry-key)
               (element-of-tree? x (right-branch tree))]))))

(define (lookup-tree key tree)
  (if (null? tree)
      (cons false null)
      (let ([entry-key (node-key (entry tree))])
        (cond [(null? entry-key) (cons false null)]
              [(= key entry-key) (cons true (entry tree))]
              [(< key entry-key)
               (lookup-tree key (left-branch tree))]
              [(> key entry-key)
               (lookup-tree key (right-branch tree))]))))

(define (make-table)
  (let ((local-table (cons '*table (empty-tree))))
    (let ((local-tree (cdr local-table)))
      
      (define (lookup key)
        (let ((record (lookup-tree key local-tree)))
          (and (car record) (node-val (cdr record)))))
      
      (define (insert! key value)
        (adjoin-tree (make-node key value) local-tree)
        (void))
      
      (define (dispatch m)
        (cond ((eq? m 'lookup-proc) lookup)
              ((eq? m 'insert-proc!) insert!)
              (else (error "Unknown operation -- TABLE" m))))
      dispatch)))

(define (insert-table! table key value)
  ((table 'insert-proc!) key value))

(define (lookup-table table key)
    ((table 'lookup-proc) key))
