#lang sicp

((lambda (a) 
   (define (f x) 
     (let ([b '*unassigned*] 
           [a '*unassigned*]) 
       (set! a 5)
       (set! b (+ a x)) 
       (+ a b))) 
   (f 10)) 1)