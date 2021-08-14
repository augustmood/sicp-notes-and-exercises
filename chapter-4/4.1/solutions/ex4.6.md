# Exercise 4.6

`Let` expressions are derived expressions, because

```scheme
(let ((<var1> <exp1>) ... (<var_n> <exp_n>))
  <body>)
```

is equivalent to

```scheme
((lambda (<var1> ... <var_n>)
   <body>)
 <exp1>
 .
 .
 .
 <exp_n>)
```

Implement a syntactic transformation `let->combination` that reduces evaluating
`let` expressions to evaluating combinations of the type shown above, and add
the appropriate clause to `eval` to handle `let` expressions.

#

```scheme
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-let bindings body)
  (cons 'let (cons bindings body)))
(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (variable binds)
  (car binds))
(define (expression binds)
  (cdr binds))
(define (variable-list exp)
  (map variable (let-bindings exp))) ;; we can use recursion to do this as well
(define (expression-list bind)
  (map expression (let-bindings exp))) ;; we can use recursion to do this as well
(define (let->combination exp)
  (cons
   (make-lambda
    (variable-list exp)
    (let-body exp)))
  (expression-list exp))

;; add this line to eval:
((let? exp) (eval (let->combination exp) env))
```
