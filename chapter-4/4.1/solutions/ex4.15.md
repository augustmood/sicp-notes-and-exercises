# Exercise 4.15

Given a one-argument procedure `p` and an object `a`, `p` is said to `halt` on
`a`if evaluating the expression`(p a)` returns a value (as opposed to
terminating with an error message or running forever). Show that it is
impossible to write a procedure halts? that correctly determines whether `p`
halts on `a` for any procedure `p` and object `a`. Use the following reasoning:
If you had such a procedure `halts?`, you could implement the following program:

```scheme
(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))
```

Now consider evaluating the expression `(try try)` and show that any possible
outcome (either halting or running forever) violates the intended behavior of
`halts?`.

#

Assume when executing `(try try)`, `try` can be halted, then `(halt? try try)`
will return true, and thus the procedure `try` will running forever, which
contradicts the fact that it can be halted.

Also, if `try` cannot be halted, the `(halt? try try)` will return false, while
the procedure `try` is gonna return the `'halted` result, which contradicts the
fact that it cannot be halted.
