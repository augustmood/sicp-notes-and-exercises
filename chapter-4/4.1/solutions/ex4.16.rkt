#lang sicp
(#%require "global-env.rkt")

;; Exercise 4.16

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (or (eq? var '*unassigned*) (eq? env the-empty-environment))
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (add-back vals new-val)
  (set-cdr! vals (append (cdr vals) (list new-val))))

(define (scan-out-defines body)
  (define (scan exp-body defn-pairs new-body)
    (if (null? exp-body)
        (sequence->exp new-body)
        (let ([part (car exp-body)])
          (if (definition? part)
              (let ([defn-var (definition-variable part)]
                    [defn-val (definition-value (car exp-body))]
                    [placeholder '*unassigned*])
                (begin 
                  (add-back defn-pairs (cons defn-var placeholder))
                  (add-back new-body (list 'set! defn-var defn-val))))
              (add-back new-body (car exp-body))))))
  (scan body '() '()))


;; make-procedure, since it is called only when evaluating the lambda expression