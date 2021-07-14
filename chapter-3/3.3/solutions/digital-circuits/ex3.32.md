# Exercise 3.32

The procedures to be run during each time segment of the agenda are kept in a
queue. Thus, the procedures for each segment are called in the order in which
they were added to the agenda (first in, first out). Explain why this order must
be used. In particular, trace the behavior of an and-gate whose inputs change
from 0,1 to 1,0 in the same segment and say how the behavior would differ if we
stored a segment's procedures in an ordinary list, adding and removing
procedures only at the front (last in, first out).

#

If we use queue as the data structure of segments set, then obviously all the
items in the queue will followed the LIFO rule as mentioned in this question
description. For instance, there're wires input-1 input-2 and output, and assume
the signal value of input-1 is 0, the signal value of input-2 is 1 and the
signal value of output is 0. If we wanna change the signal value from (0, 1) to
(1, 0), we may need to call

```scheme
(set-signal! input-1 1)
(set-signal! input-2 0)
```

and this will lead the procedures below to be called:

```scheme
and-action-procedure ; -- a1
and-action-procedure ; -- a2
```

After the execution of the above codes, the segment list will be added some
procedures:

```scheme
'(lambda-a2 lambda-a1)
'((lambda () (set-signal! output 0))  (lambda () (set-signal! output 1)))
```

If we use the ordinary list as data structure, the procedure
`(lambda () (set-signal! output 0))` will be executed first and then the
"set-to-1" will be executed, in the end, the signal value of the wire output by
the wire will be set to `1`, instead of the correct, as obtained by the
procedures in the chronological fulfillment of a time segment sets -- `0`.
