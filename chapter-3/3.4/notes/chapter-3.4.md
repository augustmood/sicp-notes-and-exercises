# Chapter 3.4 Concurrency: Time Is of the Essence

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


