#lang racket
(require racket/trace)

; Suppose we define the procedure

(define (f g)
  (g 2))

; Then we have

; (define (square i)
;     (* i i))

; (f square)
; 4

(f (lambda (z) (* z (+ z 1))))
; 6

; What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.
; (f f)

(f f)
(f 2)
(2 2)

; as 2 is not even a procedure, which means the function cannot be evaluated.