# Exercise 3.40

Give all possible values of x that can result from executing

```scheme
(define x 10)

(parallel-execute (lambda () (set! x (* x x))) ;; P1
                  (lambda () (set! x (* x x x)))) ;; P2
```

Which of these possibilities remain if we instead use serialized procedures:

```scheme
(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))
```

#

- `100`: `P1` accesses `x` (twice), then `P2` sets `x` to 1000, then `P1` sets
  `x` to `100`.
- `1000`: `P2` accesses `x`, then `P1` sets x to `100`, then `P2` sets `x` to
  `1000`.
- `10000`:
  - `P2` changes x from `10` to `1000` between the two times that `P1` accesses
    the value of x during the evaluation of `(* x x)`.
  - `P1` changes x from `10` to `100` between the second time and the third time
    that `P2` accesses the value of x during the evaluation of
    `(* x x x) <- (* 10 10 100)`, which is `10000`.
- `100000`: `P1` changes x from `10` to `100` between the first time and the
  second time that `P2` accesses the value of x during the evaluation of
  `(* x x x) <- (* 10 100 100)`, which is `100000`.
- `1000000`:
  - `P1` sets `x` to 100, and then `P2` sets `x` to `(* 100 100 100)`, which is
    `1000000`.
  - `P2` sets `x` to `1000`, and then `P1` sets `x` to `(* 1000 1000)`, which is
    `1000000`.

## if we instead use serialized procedures:

- Only `1000000` will remain:
  - `P1` sets `x` to 100, and then `P2` sets `x` to `(* 100 100 100)`, which is
    `1000000`.
  - `P2` sets `x` to `1000`, and then `P1` sets `x` to `(* 1000 1000)`, which is
    `1000000`.
