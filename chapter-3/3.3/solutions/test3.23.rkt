#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(require "ex3.23.rkt")

(define x (make-deque))
((x 'front-insert-deque!) 1)
(x 'print-deque)                ;; (1)
((x 'front-insert-deque!) 2)
(x 'print-deque)                ;; (2 1)
((x 'front-insert-deque!) 3)
(x 'print-deque)                ;; (3 2 1)
(x 'front-delete-deque!)
(x 'print-deque)                ;; (2 1)
(x 'rear-delete-deque!)
(x 'print-deque)                ;; (2)
(x 'front-delete-deque!)
(x 'print-deque)                ;; ()
(x 'empty-queue?)               ;; #t

(newline)

(define q1 (make-deque))
((q1 'front-insert-deque!) 'a)  
(q1 'print-deque)               ;; (a)
((q1 'front-insert-deque!) 'b) 
(q1 'print-deque)               ;; (b a)
((q1 'rear-insert-deque!) 'c)   
(q1 'print-deque)               ;; (b a c)
(q1 'empty-queue?)              ;; #f
(q1 'rear-delete-deque!)        
(q1 'print-deque)               ;; (b a)
(q1 'rear-delete-deque!)        
(q1 'print-deque)               ;; (b)
(q1 'front-delete-deque!)
(q1 'print-deque)               ;; ()
(q1 'empty-queue?)              ;; #t
