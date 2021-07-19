# Exercise 3.53

Without running the program, describe the elements of the stream defined by

```scheme
(define s (cons-stream 1 (add-streams s s)))
```

#

The definition of `add-streams`:

```scheme
(define (add-streams s1 s2)
  (stream-map + s1 s2))
```

Some basic procedures:

```scheme
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))
```

The stream is presenting a list: '(1 2 4 8 16 ... 2^(n - 1))
