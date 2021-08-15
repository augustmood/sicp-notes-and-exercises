# Chapter 4.2 Variation on a Scheme -- Lazy Evaluation

<p style="color:#FF6666; font-weight: bold; font-style: italic"> All the codes 
and some sentences in this note are from the book: SICP <p>

## 4.2.1 Normal Order and Applicative Order

- `Scheme` is an _`applicative-order`_ language, namely, that all the arguments
  to Scheme procedures are evaluated when the procedure is applied.

- In contrast, _`normal-order`_ languages delay evaluation of procedure
  arguments until the actual arguments values are needed.

- Delaying evaluation of procedure arguments until the last possible moment
  (e.g., until they are required by a primitive operation) is called
  _`lazy evaluation`_.

- If the body of a procedure is entered before an argument has been evaluated we
  say that the procedure is _`non-strict`_ in that argument.

- If the argument is evaluated before the body of the procedure is entered we
  say that the procedure is _`strict`_ in that argument.

- The "strict" versus "non-strict" terminology means essentially the same thing
  as "applicative-order" versus "normal-order", except that it refers to
  individual procedures and arguments rather than to the language as a whole. At
  a conference on programming languages you might hear someone say, "The
  normal-order language Hassle has certain strict primitives. Other procedures
  take their arguments by lazy evaluation."

- In a purely `applicative-order` language, all procedures are `strict` in each
  argument. In a purely `normal-order` language, all compound procedures are
  `non-strict` in each argument, and primitive procedures may be either `strict`
  or `non-strict`.
