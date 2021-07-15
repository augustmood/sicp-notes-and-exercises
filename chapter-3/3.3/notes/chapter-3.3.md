# Chapter 3.3 Modeling with Mutable Data

<p style="color:#FF6666; font-weight: bold; font-style: italic"> All the codes
and some sentences in this note are from the book: SICP <p>

- In order to model compound objects with changing state, we will design data
  abstractions to include, in addition to selectors and constructors, operations
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

  - To define the queue operations we use the following procedures, which enable
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
  pair at the beginning, which holds a dummy "record" -- in this case (examples
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

- There are _`wires`_, which carry _`digital signals`_. A `digital signal` may
  at any moment have only one of two possible values, 0 and 1.

- There are also various types of digital _`function boxes`_, which connect
  wires carrying input signals to other output wires. Such boxes produce output
  signals computed from their input signals.

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

### Representing wires

### The agenda

- `(make-agenda)`: returns a new empty agenda.
- `(empty-agenda? <agenda>)`: is true if the specified agenda is empty.
- `(first-agenda-item <agenda>)`: returns the first item on the agenda.
- `(remove-first-agenda-item! <agenda>)`: modifies the agenda by removing the
  first item.
- `(add-to-agenda! <time> <action> <agenda>)`: modifies the agenda by adding the
  given action procedure to be run at the specified time.
- `(current-time <agenda>)`: returns the current simulation time.

### A sample simulation

### Implementing the agenda

## 3.3.5 Propagation of Constraints

- In this section, we sketch the design of a language that enables us to work in
  terms of relations themselves. The primitive elements of the language are
  _`primitive constraints`_, which state that certain relations hold between
  quantities. For example, `(adder a b c)` specifies that the quantities `a`,
  `b`, and `c` must be related by the equation `a + b = c`, `(multiplier x y z)`
  expresses the constraint `xy = z`, and `(constant 3.14 x)` says that the value
  of `x` must be `3.14`.

- Our language provides a means of combining primitive constraints in order to
  express more complex relations. We combine constraints by constructing
  `constraint networks`, in which constraints are joined by `connectors`. A
  connector is an object that "holds" a value that may participate in one or
  more constraints.

### Using the constraint system

- The non-directionality of computation is the distinguishing feature of
  constraint-based systems.

### Implementing the constraint system

- The basic operations on connectors are the following:

  - `(has-value? <connector>)`

    tells whether the connector has a value.

  - `(get-value <connector>)`

    returns the connector's current value.

  - `(set-value! <connector> <new-value> <informant>)`

    indicates that the informant is requesting the connector to set its value to
    the new value.

  - `(forget-value! <connector> <retractor>)`

    tells the connector that the retractor is requesting it to forget its value.

  - `(connect <connector> <new-constraint>)`

    tells the connector to participate in the new constraint.

- Implementations is also given by the book:

  ```scheme
  (define (adder a1 a2 sum)
    (define (process-new-value)
      (cond ((and (has-value? a1) (has-value? a2))
            (set-value! sum
                        (+ (get-value a1) (get-value a2))
                        me))
            ((and (has-value? a1) (has-value? sum))
            (set-value! a2
                        (- (get-value sum) (get-value a1))
                        me))
            ((and (has-value? a2) (has-value? sum))
            (set-value! a1
                        (- (get-value sum) (get-value a2))
                        me))))
    (define (process-forget-value)
      (forget-value! sum me)
      (forget-value! a1 me)
      (forget-value! a2 me)
      (process-new-value))
    (define (me request)
      (cond ((eq? request 'I-have-a-value)
            (process-new-value))
            ((eq? request 'I-lost-my-value)
            (process-forget-value))
            (else
            (error "Unknown request -- ADDER" request))))
    (connect a1 me)
    (connect a2 me)
    (connect sum me)
    me)
  
  (define (inform-about-value constraint)
    (constraint 'I-have-a-value))
  (define (inform-about-no-value constraint)
    (constraint 'I-lost-my-value))
  
  (define (multiplier m1 m2 product)
    (define (process-new-value)
      (cond ((or (and (has-value? m1) (= (get-value m1) 0))
                 (and (has-value? m2) (= (get-value m2) 0)))
            (set-value! product 0 me))
            ((and (has-value? m1) (has-value? m2))
            (set-value! product
                        (* (get-value m1) (get-value m2))
                        me))
            ((and (has-value? product) (has-value? m1))
            (set-value! m2
                        (/ (get-value product) (get-value m1))
                        me))
            ((and (has-value? product) (has-value? m2))
            (set-value! m1
                        (/ (get-value product) (get-value m2))
                        me))))
    (define (process-forget-value)
      (forget-value! product me)
      (forget-value! m1 me)
      (forget-value! m2 me)
      (process-new-value))
    (define (me request)
      (cond ((eq? request 'I-have-a-value)
            (process-new-value))
            ((eq? request 'I-lost-my-value)
            (process-forget-value))
            (else
            (error "Unknown request -- MULTIPLIER" request))))
    (connect m1 me)
    (connect m2 me)
    (connect product me)
    me)

  (define (constant value connector)
    (define (me request)
      (error "Unknown request -- CONSTANT" request))
    (connect connector me)
    (set-value! connector value me)
    me)

  (define (probe name connector)
    (define (print-probe value)
      (newline)
      (display "Probe: ")
      (display name)
      (display " = ")
      (display value))
    (define (process-new-value)
      (print-probe (get-value connector)))
    (define (process-forget-value)
      (print-probe "?"))
    (define (me request)
      (cond ((eq? request 'I-have-a-value)
            (process-new-value))
            ((eq? request 'I-lost-my-value)
            (process-forget-value))
            (else
            (error "Unknown request -- PROBE" request))))
    (connect connector me)
    me)

  (define (make-connector)
    (let ((value false) (informant false) (constraints '()))
      (define (set-my-value newval setter)
        (cond ((not (has-value? me))
              (set! value newval)
              (set! informant setter)
              (for-each-except setter
                                inform-about-value
                                constraints))
              ((not (= value newval))
              (error "Contradiction" (list value newval)))
              (else 'ignored)))
      (define (forget-my-value retractor)
        (if (eq? retractor informant)
            (begin (set! informant false)
                  (for-each-except retractor
                                    inform-about-no-value
                                    constraints))
            'ignored))
      (define (connect new-constraint)
        (if (not (memq new-constraint constraints))
            (set! constraints 
                  (cons new-constraint constraints)))
        (if (has-value? me)
            (inform-about-value new-constraint))
        'done)
      (define (me request)
        (cond ((eq? request 'has-value?)
              (if informant true false))
              ((eq? request 'value) value)
              ((eq? request 'set-value!) set-my-value)
              ((eq? request 'forget) forget-my-value)
              ((eq? request 'connect) connect)
              (else (error "Unknown operation -- CONNECTOR"
                          request))))
      me))

  (define (for-each-except exception procedure list)
    (define (loop items)
      (cond ((null? items) 'done)
            ((eq? (car items) exception) (loop (cdr items)))
            (else (procedure (car items))
                  (loop (cdr items)))))
    (loop list))
  
  (define (has-value? connector)
    (connector 'has-value?))
  (define (get-value connector)
    (connector 'value))
  (define (set-value! connector new-value informant)
    ((connector 'set-value!) new-value informant))
  (define (forget-value! connector retractor)
    ((connector 'forget) retractor))
  (define (connect connector new-constraint)
    ((connector 'connect) new-constraint))
  ```
