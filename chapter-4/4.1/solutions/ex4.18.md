# Exercise 4.18

Consider an alternative strategy for scanning out definitions that translates
the example in the text to

```scheme
(lambda <vars>
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (let ((a <e1>)
          (b <e2>))
      (set! u a)
      (set! v b))
    <e3>))
```

Here a and b are meant to represent new variable names, created by the
interpreter, that do not appear in the user's program. Consider the solve
procedure from section 3.5.4:

```scheme
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
```

Will this procedure work if internal definitions are scanned out as shown in
this exercise? What if they are scanned out as shown in the text? Explain.

#

```scheme
(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
```

Followed the new rule, `solve` can be represented as:

```scheme
(define solve
  (lambda (f y0 dt)
    (let ((y '*unassigned*)
          (dy '*unassigned*))
      (let ((a (integral (delay dy) y0 dt))
            (b (stream-map f y)))
        (set! u a)
        (set! v b))
      y)))
```

which is literally the same as this one:

```scheme
(define solve
  (lambda (f y0 dt)
    ((lambda (f dy)
       ((lambda (a b)
          (set! y a)
          (set! dy b))
        (integral (delay dy) y0 dt)
        (stream-map f y))
       y) '*unassigned* '*unassigned*)))
```

For the inner lambda, we need to evaluate `(integral (delay dy) y0 dt)` and
`(stream-map f y)` first, although `dy` and `y` is unassigned at the time, `dy`
is literally delayed, when we first evaluate `integral`, but we don't actually
know if the `y` in stream-map gets delayed. If so, the transformation will work,
if not, it will not work. 
