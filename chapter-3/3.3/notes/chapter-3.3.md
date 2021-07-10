# Chapter 3.3 Modeling with Mutable Data

<p style="color:#FF6666; font-weight: bold; font-style: italic"> All the codes
and some sentences in this note are from the book: SICP <p>

- In order to model compound objecs with chaging state, we will design data
  abstractions to include, in addtion to selectors and constructors, operations
  called `mutators`, which modify data objects.

- Data objects for which mutators are defined are known as
  `mutable data objects`.

## 3.3.1 Mutable List Structure

- `set-car!` and `set-cdr!` return implementation-dependent values. Like set!,
  they should be used only for their effect.

- The book gives an example of implementing `cons`:

  ```scheme
  (define (cons x y)
      (let ((new (get-new-pair)))
          (set-car! new x)
          (set-cdr! new y)
          new))
  ```

### Sharing and identity

- The mutation operations `set-car!` and `set-cdr!` should be used with care;
  unless we have a good understanding of how our data objects are shared,
  mutation can have unanticipated results.

### Mutation is just assignment

- The book gives an example of implementation of constructors and selectors
  mutable pair:

  ```scheme
  (define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
      (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)
  (define (car z) (z 'car))
  (define (cdr z) (z 'cdr))
  (define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
  (define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

  ```

## 3.3.2 Representing Queues

- The book introduces the data structure `queue`:

  - A queue is represented as a pair of pointers, `front-ptr` and `rear-ptr`,
    which indicate the first and last pairs in an ordinary list.

  - To define the queue operations we use the following procedures, which enbale
    us to select and to modify the front and rear pointers of a queue:

    ```scheme
    (define (front-ptr queue) (car queue))
    (define (rear-ptr queue) (cdr queue))
    (define (set-front-ptr! queue item) (set-car! queue item))
    (define (set-rear-ptr! queue item) (set-cdr! queue item))

    (define (empty-queue? queue) (null? (front-ptr queue)))
    (define (make-queue) (cons '() '()))
    (define (front-queue queue)
        (if (empty-queue? queue)
            (error "FRONT called with an empty queue" queue)
            (car (front-ptr queue))))
    (define (insert-queue! queue item)
        (let ((new-pair (cons item '())))
            (cond ((empty-queue? queue)
                    (set-front-ptr! queue new-pair)
                    (set-rear-ptr! queue new-pair)
                    queue)
                (else
                    (set-cdr! (rear-ptr queue) new-pair)
                    (set-rear-ptr! queue new-pair)
                    queue))))
    (define (delete-queue! queue)
        (cond ((empty-queue? queue)
                (error "DELETE! called with an empty queue" queue))
                (else
                (set-front-ptr! queue (cdr (front-ptr queue)))
                queue)))
    ```

## 3.3.3 Representing Tables

- We build the table as a _`headed list`_, a headed list has a special backbone
  pair at the beginning, whichi holds a dummy "record" -- in this case (examples
  given by the book) the arbitrarily chosen symbol `*table*`.

- To extract information from a table we use the `lookup` procedure, which takes
  a key as argument and returns the associated value (or false if there is no
  value stored under that key). `lookup` is defined in terms of the `assoc`
  operation, which expects a key and a list of records as arguments.

  ```scheme
  (define (lookup key table)
    (let ((record (assoc key (cdr table))))
      (if record
          (cdr record)
          false)))

  (define (assoc key records)
    (cond ((null? records) false)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

  (define (insert! key value table)
    (let ((record (assoc key (cdr table))))
      (if record
          (set-cdr! record value)
          (set-cdr! table
                    (cons (cons key value) (cdr table)))))
    'ok)

  (define (make-table)
    (list '*table*))
  ```

### Two-dimensional tables

In a two-dimensional table, each value is indexed by two keys. We can construct
such a table as a one-dimensional table in which each key identifies a subtable.

### Creating local tables

Another way to deal with multiple tables is to have separate `lookup` and
`insert!` procedures for each table. We can do this by representing a table
procedurally, as an object that maintains an internal table as part of its local
state:

```scheme
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
```

## 3.3.4 A Simulator for Digital Circuits

- In this section we design a system for performing digital logic simulations.
  This system typifies a kind of program called an _event-driven_ simulation, in
  which actions ("events") trigger further events that happen at a later time,
  which in turn trigger more events, and so on.

### Primitive function boxes

- To build function boxes, we use the following operations on wires:

  ```scheme
  (get-signal <wire>)
  ;; returns the current value of the signal on the wire.
  (set-signal! <wire> <new value>)
  ;; changes the value of the signal on the wire to the new value.
  (add-action! <wire> <procedure of no arguments>)
  ;; asserts that the designated procedure should be run whenever the signal on
  ;; the wire changes value. Such procedures are the vehicles by which changes 
  ;; in the signal value on the wire are communicated to other wires.
  ```

  ```scheme
  (define (inverter input output)
    (define (invert-input)
      (let ((new-value (logical-not (get-signal input))))
        (after-delay inverter-delay
                    (lambda ()
                      (set-signal! output new-value)))))
    (add-action! input invert-input)
    'ok)

  (define (logical-not s)
    (cond ((= s 0) 1)
          ((= s 1) 0)
          (else (error "Invalid signal" s))))

  (define (and-gate a1 a2 output)
    (define (and-action-procedure)
      (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
        (after-delay and-gate-delay
                    (lambda ()
                      (set-signal! output new-value)))))
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure)
    'ok)
  ```
