#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(require "ex3.25.rkt")

(define a (make-table eq?))
((a 'insert-proc!) 'key-1 'a 3)
((a 'insert-proc!) 'key-1 'b 4)
((a 'insert-proc!) 'a 'a-3)
((a 'insert-proc!) 'b 'b-4)

((a 'lookup-proc) 'key-1 'b) ; 3
((a 'lookup-proc) 'b) ; 4
((a 'lookup-proc) 'a) ; 'a-3
((a 'lookup-proc) 'key-1 'a) ; 'b-4

((a 'insert-proc!) 'key-1 'key-2 'key-3 100)
((a 'insert-proc!) 'key-1 'key-2 'key-4 'a 400)

((a 'lookup-proc) 'key-1 'b) ; 3
((a 'lookup-proc) 'b) ; 4
((a 'lookup-proc) 'a) ; a-3
((a 'lookup-proc) 'key-1 'a) ; 'b-4
((a 'lookup-proc) 'key-1 'key-2 'key-3) ; 100
((a 'lookup-proc) 'key-1 'key-2 'key-3 'a) ; #f
((a 'lookup-proc) 'key-1 'key-2 'key-4 'a) ; 400
