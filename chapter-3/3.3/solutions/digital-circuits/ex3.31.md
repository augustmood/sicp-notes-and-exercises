# Exercise 3.31

The internal procedure accept-action-procedure! defined in make-wire specifies
that when a new action procedure is added to a wire, the procedure is
immediately run. Explain why this initialization is necessary. In particular,
trace through the half-adder example in the paragraphs above and say how the
system's response would differ if we had defined `accept-action-procedure!` as

```scheme
(define (accept-action-procedure! proc)
   (set! action-procedures (cons proc action-procedures)))

```

#

### _`test code:`_

```scheme
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(probe 'sum sum)
(probe 'carry carry)
(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)
```

The initialization is necessary since the correct of the inital logical of the
circuit need to be right in the first place. For instance, in the full-adder
case, the initial signal value of wires:

|     |     |
| --- | --- |
| A   | 0   |
| B   | 0   |
| C   | 0   |
| D   | 0   |
| E   | 0   |
| S   | 0   |

## With the initialization in the `add-action!`:

- the signal value of wires:

|     |     |
| --- | --- |
| A   | 0   |
| B   | 0   |
| C   | 0   |
| D   | 0   |
| E   | 1   |
| S   | 0   |

- Once we set the signal of `A`/`input-1` to 1, the signal value of wires:

|     |     |     |     |
| --- | --- | --- | --- |
| A   | 0   | --> | 1   |
| B   | 0   | --- | 0   |
| C   | 0   | --- | 0   |
| D   | 0   | --> | 1   |
| E   | 1   | --- | 1   |
| S   | 0   | --> | 1   |

## Without the initialization in the `add-action!`:

- the signal value of wires:

|     |     |
| --- | --- |
| A   | 0   |
| B   | 0   |
| C   | 0   |
| D   | 0   |
| E   | 0   |
| S   | 0   |

- Once we set the signal of `A`/`input-1` to `1`, the or-gate will be triggered,
  and the signal of wire D will be set from `0` to `1`. At the same time, the
  and-gate located in wire `C` will also be triggered but the signal value of
  `C` will not change in any way and will remain as `0`, and thus the inverter
  will not change either, therefore, the signal value of the wire `S` will
  remain as `0`.

  the signal value of wires:

|     |     |     |     |
| --- | --- | --- | --- |
| A   | 0   | --> | 1   |
| B   | 0   | --- | 0   |
| C   | 0   | --- | 0   |
| D   | 0   | --> | 1   |
| E   | 0   | --- | 0   |
| S   | 0   | --- | 0   |

## From the presepective of the implementation of the codes

- There is only one change in the implementation code:

  ```scheme
  ;; original:
  (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

  ;; changed one:
  (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures)))
  ```

  But only the original one is correct. Many people intrepret this to mean that
  if we eliminate the calling the procedure itself when adding it to wire, then
  there will be no program in the `the-agenda` queue. But I think this is not
  true, in my opinion, and from the actual tests by separately executing both
  versions of `accept-action-procedure!`, removing `(proc)` causes some
  procedures not to be called by `call-each`, for instance if at the beginning,
  the signal value of some wires is not expressed correctly which violate the
  given logical of those gates. This will result in some wires not being added
  to the `agenda` table when they should have a signal value change to trigger a
  gate, but this does not mean that all procedures will not be added. The
  `agenda` queue will only miss some procedures, but not all.

  In our [`test-code`](#test-code) example, the two wires in which the procedure
  `propagate` appears are never triggered to call the procedure `set-signal`,
  since their signal value are never changed, even though they should be changed
  at some point, thus nothing is printed except for `ok` and `done`.
