# Exercise 4.8

`Named let` is a variant of `let` that has the form

```scheme
(let <var> <bindings> <body>)
```

The `<bindings>` and `<body>` are just as in ordinary let, except that `<var>`
is bound within `<body>` to a procedure whose body is `<body>` and whose
parameters are the variables in the `<bindings>`. Thus, one can repeatedly
execute the `<body>` by invoking the procedure named `<var>`. For example, the
iterative Fibonacci procedure _`(section 1.2.2)`_ can be rewritten using named `let`
as follows:

```scheme
(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))
```

Modify `let->combination` of _`exercise 4.6`_ to also support named `let`.

#

```scheme
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (variable binds)
  (car binds))
(define (expression binds)
  (cdr binds))
(define (variable-list bind)
  (map variable clauses)) ;; we can use recursion to do this as well
(define (expression-list bind)
  (map expression clauses)) ;; we can use recursion to do this as well

;; named let
;; '(let <var> <bindings> . <body>)

(define (make-define var value)
  (cons 'define (cons var value)))

;; Corresponding to the given form: (define <var> <value>)

(define (named-let? exp) (symbol? (cadr exp)))
(define (named-let-var exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-body exp) (cdddr exp))
(define (named-variable-list exp)
  (map variable (named-let-bindings exp)))
(define (named-expression-list exp)
  (map expression (named-let-bindings exp)))
(define (named-let->define exp)
  (make-define (named-let-var exp)
               (make-lambda
                (named-variable-list exp)
                (named-let-body exp))))

(define (let->combination exp)
  (if (named-let? exp)
      (cons
       (make-lambda
        '()
        (sequence->exp
         (list (named-let->define exp)
               (cons (named-let-var exp)
                     (named-expression-list exp)))))
       '())
      (cons
       (make-lambda
        (variable-list (let-body exp))
        (let-body exp)))
      (expression-list (let-body exp))))

;; add this line to eval:
((let? exp) (eval (let->combination exp) env))
```
