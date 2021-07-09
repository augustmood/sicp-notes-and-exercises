#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(require "ex3.26.rkt")

; (define proc (lambda (key) (and (number? key) (>= (expt 10 3) ))))
(define a (make-table))
(define b (make-table))

(insert-table! a '1234 'abcd)
(insert-table! a  '7889 '0dh3)
(insert-table! a  '7809 '4hjf)
(insert-table! a  '1927 'dj1k)

(insert-table! b '1234 'sid1)
(insert-table! b  '7889 'dfds)
(insert-table! b  '7809 '2wsd)
(insert-table! b  '1000 '3erf)

(lookup-table a '1234) ; abcd
(lookup-table a '7889) ; 0dh3
(lookup-table a '7809) ; 4hjf
(lookup-table a '1927) ; dj1k
(lookup-table a '1000) ; #f

(newline)

(lookup-table b '1234) ; sid1
(lookup-table b '7889) ; dfds
(lookup-table b '7809) ; 2wsd
(lookup-table b '1927) ; #f
(lookup-table b '1000) ; 3erf
