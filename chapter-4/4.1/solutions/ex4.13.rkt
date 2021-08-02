#lang sicp
(#%require "ex4.12.rkt")

; Scheme allows us to create new bindings for variables by means of define, 
; but provides no way to get rid of bindings. Implement for the evaluator a 
; special form make-unbound! that removes the binding of a given symbol from the
; environment in which the make-unbound! expression is evaluated. This problem 
; is not completely specified. For example, should we remove only the binding in
; the first frame of the environment? Complete the specification and justify 
; any choices you make.

(define (make-unbound! var env)
  (let ([env-func (lambda (env)
                    'done)]
        [vals-func (lambda (vars vals)
                     (if (null? (cdr vars))
                         (begin (set! vars nil)
                                (set! vals nil))
                         (begin (set-car! vars (cadr vars))
                                (set-cdr! vars (cddr vars))
                                (set-car! vals (cadr vals))
                                (set-cdr! vals (cddr vals)))))]
        [error-message (lambda () (error "Unbound variable" var))])
    (env-traversal var env env-func vals-func error-message)))

;; From my understanding, we should only remove the binding just in the first 
;; frame, since we may don't want to mess up the global env. Also, if we let it
;; traversal the frames but without telling in which frames we removed the var,
;; it might cause a lot of troubles.
