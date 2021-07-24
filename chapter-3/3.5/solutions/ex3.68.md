# Exercise 3.68

Louis Reasoner thinks that building a stream of pairs from three parts is
unnecessarily complicated. Instead of separating the pair `(S0,T0)` from the
rest of the pairs in the first row, he proposes to work with the whole first
row, as follows:

```scheme
(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s) (stream-cdr t))))
```

Does this work? Consider what happens if we evaluate `(pairs integers integers)`
using Louis's definition of `pairs`.

#

The given implementation of `interleave`:

```scheme
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
```

This will not work because there's no delay occurring in `pairs` and once we
pass the new `pairs` call to the interleave, it will be executed immediately,
while this new `pairs` call which also contains a call to `pairs`, and which
will cause an infinite loop. Even if the arguments passed to the original
`pairs` are finite stream, it will still cause an error since there's an empty
stream at the end of every stream, but the function `pairs` will try to call
`stream-cdr` on it, even though it already reaches the deepest of the stream.
