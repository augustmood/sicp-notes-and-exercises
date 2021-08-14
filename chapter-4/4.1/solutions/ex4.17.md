# Exercise 4.17

Draw diagrams of the environment in effect when evaluating the expression
_`<e3>`_ in the procedure in the text, comparing how this will be structured
when definitions are interpreted sequentially with how it will be structured if
definitions are scanned out as described. Why is there an extra frame in the
transformed program? Explain why this difference in environment structure can
never make a difference in the behavior of a correct program. Design a way to
make the interpreter implement the "simultaneous" scope rule for internal
definitions without constructing the extra frame.

#

- Assume when executing the original program, we can only have E1 environment,
  when executing the transformed one, we may need E2 to define `"unassigned"`
  vars, and assign values to those variables in this environment.

- There's an extra in the transformed program, because the `let` that we used
  creates a new environment to hold those `"unassigned"` vars.

- Another implementation without constructing the extra frame.

  ```scheme
  (lambda <vars>
    (define u '*unassigned*)
    (define v '*unassigned*)
    (set! u <e1>)
    (set! v <e2>)
    <e3>)
  ```
