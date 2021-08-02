#lang sicp
(#%provide (all-defined))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

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
  (cons (cons var val) frame))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (env-traversal var env env-func vals-func error-message)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             ;; function that manipulate on the environment
             (env-func env))
            ((eq? var (car vars))
             ;; function that manipulate on vars and vals
             (vals-func vars vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        ;; error-message
        (error-message)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (lookup-variable-value var env)
  (let ([env-func (lambda (env)
                 (lookup-variable-value var 
                                        (enclosing-environment env)))]
        [vals-func (lambda (vars vals) (car vals))]
        [error-message (lambda () (error "Unbound variable" var))])
    (env-traversal var env env-func vals-func error-message)))

(define (set-variable-value! var val env)
  (let ([env-func (lambda (env)
                 (lookup-variable-value var 
                                        (enclosing-environment env)))]
        [vals-func (lambda (vars vals) (set-car! vals val))]
        [error-message (lambda () (error "Unbound variable -- SET!" var))])
    (env-traversal var env env-func vals-func error-message)))

(define (define-variable! var val env)
  (let ([env-func (lambda (env)
                    (add-binding-to-frame! var val (first-frame env)))]
        [vals-func (lambda (vars vals) (set-car! vals val))]
        [error-message (lambda () (error "Unexpected error -- DEFINE!" var))])
    (env-traversal var env env-func vals-func error-message)))
