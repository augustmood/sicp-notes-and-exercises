# Exercise 3.57

How many additions are performed when we compute the *n*th Fibonacci number
using the definition of `fibs` based on the `add-streams` procedure? Show that
the number of additions would be exponentially greater if we had implemented
`(delay <exp>)` simply as `(lambda () <exp>)`, without using the optimization
provided by the `memo-proc` procedure described in section 3.5.1.

#

The definition of fibs:

```scheme
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))
```

From the above code, there're only `n - 2` additions performed to compute the
*n*th Fibonacci number, as each number on the Fibonacci Sequence only computed
once.

## Explanation:

The definition of `fibs` is based on the `add-streams` program. `fibs` calles
`add-streams` in such a way that it uses two copies of itself as arguments,
except that these two copies start at different indexes (one starts with 0 and
the other starts with 1). And those copies passed into `fibs` as arguments to
the `add-streams` of `fibs` will also have the `add-streams` program executed
and also use itself as arguments, thus as the Fibonacci variable becomes larger,
the number of additions will grow exponentially (2 ^ (n - 2)).
