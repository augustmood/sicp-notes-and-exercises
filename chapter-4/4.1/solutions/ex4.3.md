# Exercise 4.3

Rewrite `eval` so that the dispatch is done in data-directed style. Compare this
with the data-directed differentiation procedure of exercise 2.73. (You may use
the `car` of a compound expression as the type of the expression, as is
appropriate for the syntax implemented in this section.) .

#

```scheme
(define (operator exp) (car exp))

(define (install-eval-package)
  (put 'eval 'quoted?
       (lambda (exp env) (text-of-quotation exp)))
  (put 'eval 'assignment? assignment)
  (put 'eval 'definition? eval-definition)
  (put 'eval 'if? eval-if)
  (put 'eval 'lambda?
       (lambda (exp env)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env)))
  (put 'eval 'begin?
       (lambda (exp env)
         (eval-sequence (begin-actions exp) env)))
  (put 'eval 'cond?
       (lambda (exp env)
         (eval-if (cond->if exp) env)))
  'done)

(define (eval exp env)
  (install-eval-package)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        [(get 'eval (operator exp)) ((get 'eval (operator exp)) exp env)]
        [(application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env))]
        [else (error "Unknown expression type -- EVAL" exp)]))
```
