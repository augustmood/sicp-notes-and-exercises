#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)
(require "wire.rkt")
(require "basic-gates.rkt")

(define (ripple-carry-adder list-a list-b list-s c-in c-out)
  
  (define (assertion)
    (and (or (apply = (map length (list list-a list-b list-s)))
             (error 
              (string-append "; RIPPLE-CARRY-ADDER:\n"
                             ";  the length of the arguments lists varies")))
         (void)))
  
  (define (rc-adder-rec list-a list-b list-s wire-c-in wire-c-out)
    (assertion)
    (if (null? list-a)
        (begin 
          (set! c-out wire-c-out)
          'ok)
        (begin
          (full-adder (car list-a) (car list-b) wire-c-in
                      (car list-s) (make-wire))
          (rc-adder-rec (cdr list-a) (cdr list-b) (cdr list-s) 
                        wire-c-out (make-wire)))))
  
  (rc-adder-rec list-a list-b list-s c-in (make-wire)))

;; delay_half-adder = (max (+ delay_and delay_inverter) delay_or) 
;;                     + delay_inverter + delay_and
;;
;; delay_full-adder = 2 * delay_half-adder + delay_or
;;
;; delay_ripple-carry-adder = n * delay_full-adder
