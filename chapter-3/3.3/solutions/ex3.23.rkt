#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(provide (all-defined-out))

; A deque ("double-ended queue") is a sequence in which items can be inserted 
; and deleted at either the front or the rear. Operations on deques are the
; constructor make-deque, the predicate empty-deque?, selectors front-deque and
; rear-deque, and mutators front-insert-deque!, rear-insert-deque!,
; front-delete-deque!, and rear-delete-deque!. Show how to represent deques 
; using pairs, and give implementations of the operations.All operations should 
; be accomplished in O(1) steps.

(define (make-deque)
  (let ([front-ptr '()]
        [rear-ptr '()])
    
    (define (make-node value)
      (cons (cons '() value) '()))
    (define (next node)
      (cdr node))
    (define (prev node)
      (caar node))
    (define (val node)
      (cdar node))
    (define (set-prev! node item)
      (set-car! (car node) item))
    (define (set-next! node item)
      (set-cdr! node item))
    
    (define (front-deque) front-ptr)
    (define (rear-deque) rear-ptr)
    (define (set-front-deque! item) (set! front-ptr item))
    (define (set-rear-deque! item) (set! rear-ptr item))
    (define (empty-deque?) (null? (front-deque)))
    
    (define (front-insert-deque! item)
      (let ((new-pair (make-node item)))
        (cond [(empty-deque?)
               (set-front-deque! new-pair)
               (set-rear-deque! new-pair)]
              [else
               (let ((old-front (front-deque)))
                 (set-prev! old-front new-pair)
                 (set-next! new-pair old-front)
                 (set-front-deque! new-pair))])))
    
    (define (rear-insert-deque! item)
      (let ((new-pair (make-node item)))
        (cond [(empty-deque?)
               (set-front-deque! new-pair)
               (set-rear-deque! new-pair)]
              [else
               (let ((old-rear (rear-deque)))
                 (set-next! old-rear new-pair)
                 (set-prev! new-pair old-rear)
                 (set-rear-deque! new-pair))])))
    
    (define (front-delete-deque!)
      (cond [(empty-deque?)
             (error "DELETE! called with an empty queue")]
            [else
             (let ([old-front (front-deque)])
               (let ([new-front (next old-front)])
                 (if (null? new-front)
                     (begin (set-rear-deque! '())
                            (set-front-deque! '()))
                     (begin (set-prev! new-front null)
                            (set-next! old-front null)
                            (set-front-deque! new-front)))))]))
    
    (define (rear-delete-deque!)
      (cond [(empty-deque?)
             (error "DELETE! called with an empty queue")]
            [else
             (let ([old-rear (rear-deque)])
               (let ([new-rear (prev old-rear)])
                 (if (null? new-rear)
                     (begin (set-front-deque! '())
                            (set-rear-deque! '()))
                     (begin (set-prev! old-rear null)
                            (set-next! new-rear null)
                            (set-rear-deque! new-rear)))))]))
    
    (define (print-deque)           ;; takes O(n)
      (let ([data (front-deque)])
        (define (rec i)
          (if (null? i)
              null
              (cons (val i) (rec (next i)))))
        (rec data)))
    
    (define (dispatch m)
      (cond [(eq? m 'front-deque) (front-deque)]
            [(eq? m 'rear-deque) (rear-deque)]
            [(eq? m 'set-front-deque!) 
            (lambda (item) (set-front-deque! item))]
            [(eq? m 'set-rear-deque!) 
            (lambda (item) (set-rear-deque! item))]
            [(eq? m 'empty-queue?) (empty-deque?)]
            [(eq? m 'front-insert-deque!) 
            (lambda (item) (front-insert-deque! item))]
            [(eq? m 'rear-insert-deque!) 
            (lambda (item) (rear-insert-deque! item))]
            [(eq? m 'front-delete-deque!) (front-delete-deque!)]
            [(eq? m 'rear-delete-deque!) (rear-delete-deque!)]
            [(eq? m 'print-deque) (print-deque)]
            [else (error "Undefined operation: MAKE-QUEUE" m)]))
    dispatch))
    