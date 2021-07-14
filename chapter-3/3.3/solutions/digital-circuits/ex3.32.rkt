#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(require "wire.rkt")
(require "basic-gates.rkt")

; The procedures to be run during each time segment of the agenda are kept in a
; queue. Thus, the procedures for each segment are called in the order in which
; they were added to the agenda (first in, first out). Explain why this order 
; must be used. In particular, trace the behavior of an and-gate whose inputs 
; change from 0,1 to 1,0 in the same segment and say how the behavior would 
; differ if we stored a segment's procedures in an ordinary list, adding and 
; removing procedures only at the front (last in, first out).

(define input-1 (make-wire))
(define input-2 (make-wire))
(define output (make-wire))
(probe 'output output)
(set-signal! input-2 1)
(and-gate input-1 input-2 output)
(set-signal! input-1 1)
(set-signal! input-2 0)
(propagate)
