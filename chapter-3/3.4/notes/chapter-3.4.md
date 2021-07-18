# Chapter 3.4 Concurrency: Time Is of the Essence

<p style="color:#FF6666; font-weight: bold; font-style: italic"> All the codes
and some sentences in this note are from the book: SICP <p>

- The central issue lurking beneath complexity of state, sameness, and change is
  that by introducing assignment we are forced to admit time into our
  computational models.

- The execution of assignment statements delineates _*moments in time*_ when
  values change. The result of evaluating an expression depends not only on the
  expression itself, but also on whether the evaluation occurs before or after
  these moments.

## 3.4.1 The Nature of Time in Concurrent Systems

### Correct behavior of concurrent programs

- One possible restriction on concurrency would stipulate that no two operations
  that change any shared state variables can occur at the same time, but this
  would probably be both inefficient and overly conservative.

- A less stringent restriction on concurrency would ensure that a concurrent
  systems produces the same result as if the process had run sequentially in
  some order. There are two important aspects to this requirement. First, it
  does not require the processes to actually run sequentially, but only to
  produce results that are the same as _if_ they had run sequentially. Second,
  there may be more than one possible "correct" result produced by a concurrent
  program, because we require only that the result be the same as for some
  sequential order.

## 3.4.2 Mechanisms for Controlling Concurrency

- A more practical approach to the design of concurrent systems is to devise
  general mechanisms that allow us to constrain the interleaving of concurrent
  process so that we can be sure that the program behavior is correct.

### Serializing access to shared state

- Serialization implements the following idea: Processes will execute
  concurrently, but there will be certain collections of procedures that cannot
  be executed concurrently. More precisely, serialization creates distinguished
  sets of procedures such that only one execution of a procedure in each
  serialized set is permitted to happen at a time. If some procedure in the set
  is being executed, then a process that attempts to execute any procedure in
  the set will be forced to wait until the first execution has finished.

### Serializers in Scheme

- parallel-execute:

  ```scheme
  (parallel-execute <p1> <p2> ... <pk>)
  ```

  In the _`mit-scheme`_ implementation, the concurrent processes run
  concurrently with the original scheme process. Also, the value returned by
  `parallel-execute` is a special control object that can be used to halt the
  newly created processes.

- We can constrain the concurrency by using the serialized procedures, which are
  created by serializers. A serializer takes a procedure as argument and returns
  a serialized procedure that behaves like the original procedure. All calls to
  a given serializer return serialized procedures in the same set.

### Complexity of using multiple shared resources

- While using serializers is relatively straightforward when there is only a
  single shared resources, concurrent programming can be treacherously diffcult
  when there are multiple shared resources.

### Implementing serializers

- We implement serializers in terms of a more primitive synchronization
  mechanism called a mutex.

- A mutex is an object that supports two operations -- the mutex can be
  acquired, and the mutex can be released. Once a mutex has been acquired, no
  other acquire operations on that mutex may proceed until the mutex is
  released.

- The term `mutex` is an abbreviation for _`mutual exclusion`_. The general
  problem for arranging a mechanism that permits concurrent processes to safely
  share recourses is called the mutual _`exclusion problem`_. The acquire and
  release operations were originally called `P` and `V`, from the Dutch word
  _`passeren`_ (to pass) and _`vrijgeven`_ (to release), in reference to the
  semaphores used on railroad systems.

- The book gives the implementation of `make-serializer`:

  ```scheme
  (define (make-serializer)
    (let ((mutex (make-mutex)))
      (lambda (p)
        (define (serialized-p . args)
          (mutex 'acquire)
          (let ((val (apply p args)))
            (mutex 'release)
            val))
        serialized-p)))
  ```

- The mutex is a mutable object that can hold the value true or false, When the
  value is false, the mutex is available to be acquired. When the value is true,
  the mutex is unavailable, and any process that attempts to acquire the mutex
  must wait.

- The book gives the implementation of `make-mutex`:

  ```scheme
  (define (make-mutex)
    (let ((cell (list false)))
      (define (the-mutex m)
        (cond ((eq? m 'acquire)
              (if (test-and-set! cell)
                  (the-mutex 'acquire))) ; retry
              ((eq? m 'release) (clear! cell))))
      the-mutex))
  (define (clear! cell)
    (set-car! cell false))

  (define (test-and-set! cell)
    (if (car cell)
        true
        (begin (set-car! cell true)
              false)))
  ```

- We must guarantee that, once a process has tested the cell and found it to be
  false, the cell contents will actually be set to true before any other process
  can test the cell.

### Deadlock

- The situation that each process in a concurrent executing is stalled forever,
  waiting for the other is called a `deadlock`

- The general technique for avoiding deadlock by numbering the shared resources
  and acquiring them in order is due to Havender (1968). Situations where
  deadlock cannot be avoided require `deadlock-recovery` methods, which entail
  having processes ``back out'' of the deadlocked state and try again.

### Concurrency, time and communication

- In essence, any notion of time in concurrency control must be intimately tied
  to communication.
