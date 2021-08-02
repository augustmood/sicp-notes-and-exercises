#lang sicp

; Instead of representing a frame as a pair of lists, we can represent a frame 
; as a list of bindings, where each binding is a name-value pair. Rewrite the 
; environment operations to use this alternative representation.

(define (generic-map proc . args)
  (if (null? (car args))
      nil
      (cons
       (apply proc (map car args))
       (apply generic-map
              (cons proc (map cdr args))))))

(define (make-frame variables values)
  (generic-map cons variables values))

(define (frame-variables frame) (map car frame))

(define (frame-values frame) (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (let ([temp (car frame)])
    (set-car! frame (cons var val))
    (set-cdr! frame (cons temp (cdr frame)))))
