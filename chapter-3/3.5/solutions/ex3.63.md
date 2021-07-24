# Exercise 3.63

Louis Reasoner asks why the `sqrt-stream` procedure was not written in the
following more straightforward way, without the local variable `guesses`:

```scheme
(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))
```

Alyssa P. Hacker replies that this version of the procedure is considerably less
efficient because it performs redundant computation. Explain Alyssa's answer.
Would the two versions still differ in efficiency if our implementation of
`delay` used only `(lambda () <exp>)` without using the optimization provided by
`memo-proc` (section 3.5.1)?

#

Compared to this one (the original one):

```scheme
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)
```

Louis's `sqrt-stream` version will create a new stream each time we force the
promised procedure in cdr, and since it's new, it's not be memoized.

If our implementation of `delay` used only `(lambda () <exp>)` without memoizer,
the two versions won't differ since then they both are bot memoized.
