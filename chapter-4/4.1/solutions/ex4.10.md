# Exercise 4.10

By using data abstraction, we were able to write an `eval` procedure that is
independent of the particular syntax of the language to be evaluated. To
illustrate this, design and implement a new syntax for Scheme by modifying the
procedures in this section, without changing `eval` or `apply`.

#

Let's use the `C++`'s `and\or` syntax, which is:

```scheme
;; and => &&
;; or  => ||
```

And we only need a few lines of code to install them into our program:

```scheme
(define (and? exp) (tagged-list? exp '&&))

(define (or? exp) (tagged-list? exp '||))

;; assume there's a table for primitive expressions

(put-pexp '&& eval-and)
(put-pexp '|| eval-or)
```

As shown above, we indeed do not need to change the `eval` or `apply`...
