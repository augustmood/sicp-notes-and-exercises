# Chapter 3.5 Streams

<p style="color:#FF6666; font-weight: bold; font-style: italic"> All the codes
and some sentences in this note are from the book: SICP <p>

- Streams can mitigate some of the complexity of modeling state.

- In this section, we will see how to model change in terms of sequences that
  represent the time histories of the systems being modeled. To accomplish this,
  we introduce new data structures called `streams`.

- We introduce the technique of `delayed evaluation`, which enables us to
  represent very large (even infinite) sequences as streams.

- Stream processing lets us model systems that have state without ever using
  assignment or mutable data. This has important implications, both theoretical
  and practical, because we can build models that avoid the drawbacks inherent
  in introducing assignment. On the other hand, the stream framework raises
  difficulties of its own, and the question of which modeling technique leads to
  more modular and more easily maintained systems remains open.

## 3.5.1 Streams Are Delayed Lists

- The book gives an implementation of `list-ref`, `map` and `for-each` in a
  `stream` way:

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

- As a data abstraction, streams are the same as lists, the difference is the
  time at which the elements are evaluated. With ordinary lists, both the `car`
  and the `cdr` are evaluated at construction time. With streams, the cdr is
  evaluated at selection time.

- Our implementation of streams will be based on a special form called `delay`,
  Evaluating `(delay <exp>)` does not evaluate the expression `<exp>`, but
  rather returns a so-called _`delayed object`_, which we can think of as a
  "promise" to evaluate `<exp>` at some future time.

- As a companion to `delay`, there is a procedure called `force` that takes a
  delayed object as argument and performs the evaluation -- in effect, forcing
  the `delay` to fulfill its promise.

- `(cons-stream <a> <b>)` is equivalent to `(cons <a> (delay <b>))`, which means
  that we can construct streams using pairs. `stream-car` and `stream-cdr` can
  be defined as:

  ```scheme
  (define (stream-car stream) (car stream))
  (define (stream-cdr stream) (force (cdr stream)))
  ```

- Although `stream-car` and `stream-cdr` can be defined as procedures,
  `cons-stream` must be a special form. If `cons-stream` were a procedure, then,
  according to our model of evaluation, evaluating `(cons-stream <a> <b>)` would
  automatically cause `<b>` to be evaluated, which is precisely what we do not
  want to happen. For the same reason, `delay` must be a special form, though
  `force` can be an ordinary procedure.

### The stream implementation in action

- In general, we can think of delayed evaluation as "demand-driven" programming,
  whereby each stage in the steam process is activated only enough to satisfy
  the next stage.

### Implementing `delay` and `force`

- `delay` must package an expression so that it can be evaluated later on
  demand, and we can accomplish this simply by treating the expression as the
  body of a procedure. `delay` can be a special form such that:

  ```scheme
  (delay <exp>)
  ```

  is syntactic sugar for

  ```scheme
  (lambda () <exp>)
  ```

  Force simply calls the procedure (of no arguments) produced by delay, so we
  can implement force as a procedure:

  ```scheme
  (define (force delayed-object)
    (delayed-object))
  ```

- In many applications, we end up forcing the same delayed object many times.
  This can lead to serious inefficiency in recursive programs involving streams.
  The solution is to build delayed objects so that the first time they are
  forced, they store the value that is computed. Subsequent forcings will simply
  return the stored value without repeating the computation.

- The book finally implements `force` with a memoizer, thus, `(delay <exp>)` is
  equivalent to `(memo-proc (lambda () <exp>))`, the `memo-proc` implementation
  is also given:

  ```scheme
  (define (memo-proc proc)
      (let ((already-run? false) (result false))
        (lambda ()
        (if (not already-run?)
            (begin (set! result (proc))
                   (set! already-run? true)
                   result)
                result))))
  ```

## 3.5.2 Infinite Streams

- We can use streams to represent sequences that are infinitely long.

### Defining streams implicitly

- An alternative way to specify streams is to take advantage of delayed
  evaluation to define streams implicitly, the book gives some examples:

  ```scheme
  (define ones (cons-stream 1 ones))
  ;; which is an infinite stream of `one`s:

  (define (add-streams s1 s2)
    (stream-map + s1 s2))

  (define integers (cons-stream 1 (add-streams one integers)))

  (define fibs (cons-streams 0
                             (cons-streams 1 (add-streams (streams-cdr fibs)
                                                          fibs))))
  (define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor)) stream))

  (define primes
    (cons-stream
      2
      (stream-filter prime? (integers-starting-from 3))))
  (define (prime? n)
    (define (iter ps)
      (cond ((> (square (stream-car ps)) n) true)
            ((divisible? n (stream-car ps)) false)
            (else (iter (stream-cdr ps)))))
    (iter primes))
  ;; for every n we test for primality, either n is not prime (in which case
  ;; there is a prime already generated that divides it) or n is prime (in which
  ;; case there is a prime already generated -- i.e., a prime less than
  ;; n -- that is greater than n).
  ```

## 3.5.3 Exploiting the Stream Paradigm

- The stream approach can be illuminating because it allows us to build systems
  with different module boundaries than systems organized around assignment to
  state variables.

### Formulating iterations as stream processes

- `sqrt` procedure in stream:

  ```scheme
  (define (sqrt-improve guess x)
    (average guess (/ x guess)))

  (define (sqrt-stream x)
    (define guesses
      (cons-stream 1.0
                  (stream-map (lambda (guess)
                                (sqrt-improve guess x))
                              guesses)))
    guesses)
  ```

- Euler accelerator:

  ```scheme
  (define (euler-transform s)
    (let ((s0 (stream-ref s 0))           ; Sn-1
          (s1 (stream-ref s 1))           ; Sn
          (s2 (stream-ref s 2)))          ; Sn+1
      (cons-stream (- s2 (/ (square (- s2 s1))
                            (+ s0 (* -2 s1) s2)))
                  (euler-transform (stream-cdr s)))))
  ```

### Infinite streams of pairs

- The implementation given by the book:

  ```scheme
  (define (pairs s t)
    (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))
  ```

### Stream as signals

- We can use streams to model signal-processing systems in a very direct way,
  representing the values of a signal at successive time intervals as
  consecutive elements of a stream.

## 3.5.4 Streams and Delayed Evaluation

- `delay` is crucial for using streams to model signal-processing systems that
  contain loops. Without `delay`, our models would have to be formulated so that
  the inputs to any signal-processing component would be fully evaluated before
  the output could be produced.

### Normal-order evaluation

- Creating sepate classes of procedures forces us to create separate classes of
  higher-order procedures as well.

- One way to avoid the need for two different classes of procedures is to make
  all procedures take delayed arguments. We could adopt a model of evaluation in
  which all arguments to procedures are automatically delayed and arguments are
  forced only when they are actually needed (for example, when they are required
  by a primitive operation), and this would transform our language to use
  normal-order evaluation.
