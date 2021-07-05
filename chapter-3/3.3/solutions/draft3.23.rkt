#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)

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


(define (make-deque) (cons '() '()))
(define (front-deque deque) (car deque))
(define (rear-deque deque) (cdr deque))
(define (set-front-deque! deque item) (set-car! deque item))
(define (set-rear-deque! deque item) (set-cdr! deque item))
(define (empty-deque? deque) (null? (front-deque deque)))

(define (front-insert-deque! deque item)
  (let ((new-pair (make-node item)))
    (cond [(empty-deque? deque)
           (set-front-deque! deque new-pair)
           (set-rear-deque! deque new-pair)]
          [else
           (let ((old-front (front-deque deque)))
             (set-prev! old-front new-pair)
             (set-next! new-pair old-front)
             (set-front-deque! deque new-pair))])))

(define (rear-insert-deque! deque item)
  (let ((new-pair (make-node item)))
    (cond [(empty-deque? deque)
           (set-front-deque! deque new-pair)
           (set-rear-deque! deque new-pair)]
          [else
           (let ((old-rear (rear-deque deque)))
             (set-next! old-rear new-pair)
             (set-prev! new-pair old-rear)
             (set-rear-deque! deque new-pair))])))

(define (front-delete-deque! deque)
  (cond [(empty-deque? deque)
         (error "DELETE! called with an empty queue" deque)]
        [else
         (let ([old-front (front-deque deque)])
           (let ([new-front (next old-front)])
             (if (null? new-front)
                 (begin (set-rear-deque! deque '())
                        (set-front-deque! deque '()))
                 (begin (set-prev! new-front null)
                        (set-next! old-front null)
                        (set-front-deque! deque new-front)))))]))

(define (rear-delete-deque! deque)
  (cond [(empty-deque? deque)
         (error "DELETE! called with an empty queue" deque)]
        [else
         (let ([old-rear (rear-deque deque)])
           (let ([new-rear (prev old-rear)])
             (if (null? new-rear)
                 (begin (set-front-deque! deque '())
                        (set-rear-deque! deque '()))
                 (begin (set-prev! old-rear null)
                        (set-next! new-rear null)
                        (set-rear-deque! deque new-rear)))))]))

(define (print-deque deque)             ;; takes O(n)
  (let ([data (front-deque deque)])
    (define (rec i)
      (if (null? i)
          null
          (cons (val i) (rec (next i)))))
    (rec data)))
