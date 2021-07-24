# Exercise 3.66

Examine the stream `(pairs integers integers)`. Can you make any general
comments about the order in which the pairs are placed into the stream? For
example, about how many pairs precede the pair `(1,100)`? the pair `(99,100)`?
the pair `(100,100)`? (If you can make precise mathematical statements here, all
the better. But feel free to give more qualitative answers if you find yourself
getting bogged down.)

#

For a pair structure `(x, y)`, by the given implementation of pairs from the
book:

```scheme
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))
```

For a pair `(x, y)`, there are:

$$
(2^x - 2) + \bigg\lceil{\frac{(y-x)^2} {(y-x)^2 + 1}}\bigg\rceil *
(2^{x-1} + (y - x - 1)*2^x)
$$

pairs precede it.

Thus, for the pair `(1, 100)`, there's `197` pairs precede it, for the pair
`(99, 100)`, there's `950737950171172051122527404030` or `2^99 + 2^98 - 2` pairs
precede it, and for the pair `(100, 100)` there's
`1267650600228229401496703205374` or `2^100 - 2` pairs precede it.
