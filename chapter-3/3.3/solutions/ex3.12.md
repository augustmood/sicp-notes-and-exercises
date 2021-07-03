# Exercise 3.12

```scheme
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z ; (a b c d)
(cdr x) ;; '(b)
(define w (append! x y))
w ; (a b c d)
(cdr x) ;; '(b c d)
```

![ex3.12](pics/ex3.12/ex3.12.svg)
