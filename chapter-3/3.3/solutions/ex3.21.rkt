#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(require "queue.rkt")

; Ben Bitdiddle decides to test the queue implementation described above. He types in the procedures
; to the Lisp interpreter and proceeds to try them out:

(define q1 (make-queue))
(insert-queue! q1 'a) ; ((a) a)
(insert-queue! q1 'b) ; ((a b) b)
(delete-queue! q1) ; ((b) b)
(delete-queue! q1) ; (() b)

; "It's all wrong!" he complains. "The interpreter's response shows that the last item is inserted
; into the queue twice. And when I delete both items, the second b is still there, so the queue isn't
; empty, even though it's supposed to be." Eva Lu Ator suggests that Ben has misunderstood what is
; happening. "It's not that the items are going into the queue twice," she explains. "It's just
; that the standard Lisp printer doesn't know how to make sense of the queue representation. If you
; want to see the queue printed correctly, you'll have to define your own print procedure for 
; queues." Explain what Eva Lu is talking about. In particular, show why Ben's examples produce the 
; printed results that they do. Define a procedure print-queue that takes a queue as input and prints 
; the sequence of items in the queue.

;; Ben Bitdiddle misunderstood the queue data representation, as the front-ptr is a pointer to the
;; front of the list/queue, it surely contains all the items the queue it have, but it works as a 
;; head pointer, and the rear-ptr works a tail pointer, which only point to the last item in the
;; queue/list, both of them are represented as pointer instead of a whole data of the queue/list.

(define (print-queue q)
    (front-ptr q))

(print-queue q1)
