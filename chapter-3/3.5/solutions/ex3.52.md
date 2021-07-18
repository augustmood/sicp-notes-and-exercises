# Exercise 3.52

Consider the sequence of expressions

```scheme
(define sum 0)
(define (accum x)
 (set! sum (+ x sum))
 sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                        seq))
(stream-ref y 7)
(display-stream z)
```

What is the value of `sum` after each of the above expressions is evaluated?
What is the printed response to evaluating the `stream-ref` and `display-stream`
expressions? Would these responses differ if we had implemented `(delay <exp>)`
simply as `(lambda () <exp>)` without using the optimization provided by
`memo-proc` ? Explain.

#

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
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))
```

### With _memoizer_:

| base                 | 1     | 2   | 3     | 4      | 5   | 6   | 7   | 8   | 9   | 10  | 11  | 12  | 13  | 14  | 15  | 16      | 17  | 18  | 19  | 20      |
| -------------------- | ----- | --- | ----- | ------ | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ------- | --- | --- | --- | ------- |
| `seq`                | _`1`_ |     |       |        |     |     |     |     |     |     |     |     |     |     |     |         |     |     |     |         |
| `y`                  | 1     | 3   | _`6`_ |        |     |     |     |     |     |     |     |     |     |     |     |         |     |     |     |         |
| `z`                  | 1     | 3   | 6     | _`10`_ |     |     |     |     |     |     |     |     |     |     |     |         |     |     |     |         |
| `(stern-ref y 7)`    | 1     | 3   | 6     | 10     | 15  | 21  | 28  | 36  | 45  | 55  | 66  | 78  | 91  | 105 | 120 | _`136`_ |     |     |     |         |
| `(display-stream z)` | 1     | 3   | 6     | 10     | 15  | 21  | 28  | 36  | 45  | 55  | 66  | 78  | 91  | 105 | 120 | 136     | 153 | 171 | 190 | _`210`_ |

```scheme
(define sum 0)

(define (accum x)
 (set! sum (+ x sum))
 sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
;; sum = 1

(define y (stream-filter even? seq))
;; sum = 1 + 2 + 3 = 6

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                        seq))
;; sum = 6 + 4 = 10

(stream-ref y 7)
;; sum = 136
;; print:
;; 136

(display-stream z)
;; sum = 136 + 17 + 18 + 19 + 20 = 210
;; print:
;; 10
;; 15
;; 45
;; 55
;; 105
;; 120
;; 190
;; 210
;; 'done
```

### Without _memoizer_:

| base                 | 1     | 2   | 3     | 4      | 5   | 6   | 7   | 8   | 9   | 10  | 11  | 12  | 13  | 14  | 15  | 16  | 17      | 18  | 19  | 20      |
| -------------------- | ----- | --- | ----- | ------ | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ------- | --- | --- | ------- |
| `seq`                | _`1`_ |     |       |        |     |     |     |     |     |     |     |     |     |     |     |     |         |     |     |         |
| `y`                  | 1     | 3   | _`6`_ |        |     |     |     |     |     |     |     |     |     |     |     |     |         |     |     |         |
| `z`                  | 1     | 8   | 11    | _`15`_ |     |     |     |     |     |     |     |     |     |     |     |     |         |     |     |         |
| `(stern-ref y 7)`    | 1     | 3   | 6     | 19     | 24  | 30  | 37  | 45  | 54  | 64  | 75  | 87  | 100 | 114 | 129 | 145 | _`162`_ |     |     |         |
| `(display-stream z)` | 1     | 8   | 11    | 15     | 167 | 173 | 180 | 188 | 197 | 207 | 218 | 230 | 243 | 257 | 272 | 288 | 305     | 323 | 342 | _`362`_ |

Without memoizer, in some circumstances, we may call accum several times at the
same index of the sequence.

```scheme
(define sum 0)

(define (accum x)
 (set! sum (+ x sum))
 sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
;; sum = 1

(define y (stream-filter even? seq))
;; sum = 6

(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
;; sum = 15

(stream-ref y 7)
;; sum = 162
;; print:
;; 162

(display-stream z)
;; sum = 362
;; print:
;; 15
;; 180
;; 230
;; 305
;; 'done
```
