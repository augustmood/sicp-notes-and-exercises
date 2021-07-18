# Exercise 3.51

In order to take a closer look at delayed evaluation, we will use the following
procedure, which simply returns its argument after printing it:

```scheme
(define (show x)
  (display-line x)
  x)
```

What does the interpreter print in response to evaluating each expression in the
following sequence?

```scheme
(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)
```

#

The given implementation of `stream-ref` in the book:

```scheme
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))
```

Recall the implementation of `stream-cdr`, we will call `force` on
`(cdr stream)` each time we call `stream-cdr`, and `cons-stream` is equivalent
to `(cons <a> (delay <b>))`, which means when we construct the stream, the first
item is known.

| base               | 0                  | 1                  | 2                  | 3                  | 4                  | 5                  | 6                  | 7                  | 8   | 9   | 10  |
| ------------------ | ------------------ | ------------------ | ------------------ | ------------------ | ------------------ | ------------------ | ------------------ | ------------------ | --- | --- | --- |
| `x`                | (display-line 0) 0 |                    |                    |                    |                    |                    |                    |                    |     |     |     |
| `(stream-ref x 5)` | 0                  | (display-line 1) 1 | (display-line 2) 2 | (display-line 3) 3 | (display-line 4) 4 | (display-line 5) 5 |                    |                    |     |     |     |
| `(stream-ref x 7)` | 0                  | 1                  | 2                  | 3                  | 4                  | 5                  | (display-line 6) 6 | (display-line 7) 7 |     |     |     |

```scheme
;; (define x (stream-map show (stream-enumerate-interval 0 10)))

0 ;; '(0 (delay (stream-enumerate-interval 1 10)))

;; -----------------------------------------------------------------------------

;; (stream-ref x 5)

;; n = 5
1 ;; '(1 (delay (stream-enumerate-interval 2 10)))

;; n = 4
2 ;; '(2 (delay (stream-enumerate-interval 3 10)))

;; n = 3
3 ;; '(3 (delay (stream-enumerate-interval 4 10)))

;; n = 2
4 ;; '(4 (delay (stream-enumerate-interval 5 10)))

;; n = 1
5 ;; '(5 (delay (stream-enumerate-interval 6 10)))

;; n = 0
;; returned value will also be printed in the interpreter:
5

;; -----------------------------------------------------------------------------

;; (stream-ref x 7)

;; n = 7
;; nothing printed, as we've already know the result of
;; `(delay (stream-enumerate-interval 1 10))`
;; '(0 (1 (2 (3 (4 (5 (delay (stream-enumerate-interval 6 10))))))))

.
.
.

;; n = 2
6 ;; '(6 (delay (stream-enumerate-interval 7 10)))

;; n = 1
7 ;; '(7 (delay (stream-enumerate-interval 8 10)))

;; n = 0
;; returned value will also be printed in the interpreter:
7
```
