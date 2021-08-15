# Exercise 4.4

Recall the definitions of the special forms `and` and `or` from chapter 1:

- `and`: The expressions are evaluated from left to right. If any expression
  evaluates to false, false is returned; any remaining expressions are not
  evaluated. If all the expressions evaluate to true values, the value of the
  last expression is returned. If there are no expressions then true is
  returned.

- `or`: The expressions are evaluated from left to right. If any expression
  evaluates to a true value, that value is returned; any remaining expressions
  are not evaluated. If all expressions evaluate to false, or if there are no
  expressions, then false is returned.

Install `and` and `or` as new special forms for the evaluator by defining
appropriate syntax procedures and evaluation procedures `eval-and` and
`eval-or`. Alternatively, show how to implement `and` and `or` as derived
expressions.

#

```scheme
(define (or? exp)
  (tagged-list? exp 'or))
(define (or-clauses exp)
  (cdr exp))
(define (or->if exp)
  (expand-or-clauses (or-clauses exp)))
(define (expand-or-clauses clauses)
  (if (null? clauses)
      #f
      (let* ([first (car clauses)]
             [rest (cdr clauses)])
        (if first
            #t
            (expand-or-clauses rest)))))

(define (and? exp)
  (tagged-list? exp 'and))
(define (and-clauses exp)
  (cdr exp))
(define (and->if exp)
  (expand-or-clauses (or-clauses exp)))
(define (expand-and-clauses clauses)
  (if (null? clauses)
      #t
      (let* ([first (car clauses)]
             [rest (cdr clauses)])
        (if first
            (expand-or-clauses rest)
            #f))))
```
