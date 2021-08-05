#lang sicp
(#%require "global-env.rkt")

(driver-loop)

; (map (lambda (x) (* x 2)) '(1 2 3 4 5))

; ((procedure (x) ((* x 2)) 
; ((
; (false true car cdr cons null? display read + - * / map) 
; #f
; #t 
; (primitive #<procedure:mcar>) 
; (primitive #<procedure:mcdr>) 
; (primitive #<procedure:mcons>) 
; (primitive #<procedure:null?>) 
; (primitive #<procedure:mdisplay>) 
; (primitive #<procedure:mread>) 
; (primitive #<procedure:+>) 
; (primitive #<procedure:->) 
; (primitive #<procedure:*>) 
; (primitive #<procedure:/>) 
; (primitive #<procedure:mmap>)))) (1 2 3 4 5))<- a-p-p-args