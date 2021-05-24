# Chapter 1

## 1.1 The Elements of Programming
- Every powerful language has three machanisms for accomplishing this:
    - primitive expressions: the simplest entities the language is concerned with
    - means of combination: compound elements are built from simpler ones
    - means of abstraction: compound elements can be named and manipulated as units

### 1.1.1 Expressions
- The convention of placing the operator to the left of the operands is known as prefix notations.
    ```lisp
    (+ 1 2 3)
    ```

### 1.1.2 Naming and the Environment
- In the scheme dialect of lisp, we name things with `define`.
    ```lisp
    (define size 2)
    ```
    Define is lisp's simplest means of abstraction.

### 1.1.3 Evaluating Combinations
- Exceptions like `define` to the general evaluation rule are called *special forms*. 

### 1.1.4 Compound Procedures
- Some elements that must appear in any powerful programming language:
    - Numbers and arithmetic operations are primitive data and procedures.
    - Nesting of combinations provides a means of combining operations.
    - Definitions that associate names with values provide a limited means of abstraction.

- The general form of a procedure definition is
```lisp
(define (<name> <formal parameters>) <body>)
```

### 1.1.5 The substitution Model for Procedure Application
- Substitution model: It can be taken as a model that determines the meaning of procedure 
application. Two point should be stressed:
    - The purpose of the substitution is to help us think about procedure application, not to 
    provide a description of how the interpreter really works. Typical interpreters do not evaluate 
    procedure applications by manipulating the text of a procedure to substitute values for the 
    formal parameters. In practice, the ``substitution`` is accomplished by using a local 
    environment for the formal parameters.
    - As we examine things in greater detail, these simple models become inadequate and must be 
    replaced by more refined models.

- Example:

    Functions:
    ```lisp
    (define (sqaure x) (* x x))

    (define (sum-of-sqaures x y)
        (+ (sqaure x) (square y)))

    (define (f a)
        (sum-of-sqaures (+ a 1) (* a 2)))
    ```

    We could perform the evaluation like this:
    ```lisp
    (f 5)
    ↓ 
    ;; Retrieving the body of f:
    (sum-of-sqaures (+ a 1) (* a 2))
    ↓ 
    ;; Replacing the formal parameter a by argument 5:
    (sum-of-sqaures (+ 5 1) (* 5 2))
    ↓ 
    (+ (sqaure 6) (sqaure 10))
    ↓ 
    (+ (* 6 6) (* 10 10))
    ↓ 
    (+ 36 100)
    ↓ 
    136
    ```

    The method we use in the above evaluation which first evaluating the arguments and then apply is 
    *applicative order*.

    #### *Applicative order versus Normal order*
    There is another way to perform the evaluation, which would not evaluate the operands until their 
    values were needed. Instead it would first substitute operand expression for parameters until it 
    obtained an expression invovling only primitive operators, and would then perform the evaluation:

    ```lisp
    (f 5)
    ↓ 
    (sum-of-sqaures (+ 5 1) (* 5 2))
    ↓ 
    (+ (sqaure (+ 5 1)) (sqaure (* 5 2)))
    ↓
    (+ (* (+ 5 1)) (* (* 5 2) (* 5 2)))
    ↓
    (+ (* 6 6) (* 10 10))
    ↓
    (+ 36 100)
    ↓
    136
    ```

    The method used in the above evaluation is known as *normal-order evaluation*.

- Lisp uses *applicative-order* evaluation, partly beacause of the additional efficiency obtained 
from avoiding multiple evaluations of expressions, and more significantly because *normal-order* 
becomes much more complicated to deal with when we leave the realm of procedures that can be modeled
 by substitution.

### 1.1.6 Conditional Expression and Predicates
- *case analysis* in Lisp:
    - Special form in Lisp for notating case analysis:
        ```Lisp
        (cond (<p1> <e1>)
            (<p2> <e2>)
            .
            .
            .
            (<pn> <en>))
        ```
      consisting of the symbol `cond` followed by parenthesized pairs of expressions `(<p> <e>)`
      called clauses. The first expression in each pair is a `predicate`. (expression return a 
      boolean value). If none of `<p>`'s is found to be true, the value of the `cond` is undefined.
    - general form of an if expression is:
        ```lisp
        (if <predicate> <consequent> <alternative>)
        ```
    - We could construct compound predicates:
        ```lisp
        (and <e_1> ... <e_n>)
        (or <e_1> ... <e_n>)
        (not <e>)
        ```

### 1.1.7 Example: Square Roots by Newton's Method


### 1.1.8 Procedures as Black-Box Abstractions
- A procedure definition should be able to suppress detail, the users of the procedure may not have 
written the procedure themselves, but may have obtained it from another programmer as a black box.

#### Local names
- A formal parameter of a procedure has a very special role in the procedure definition, in that it 
doesn't matter what name the formal parameter has. Such a name is called a *bound variable*, and we 
say that the procedure definition *binds* its formal parameters. The meaning of a procedure 
definition is unchanged if a bound variable is consistently renamed throughout the definitoin.
- If a variable is not bound, we say that it is *free*.
- The set of expressions for which a binding defines a name is called the scope of that name.

#### Internal definitions and block structure
- We allow a procedure to have internal definitions that are local to a procedure. Example:
    ```lisp
    (define (sqrt x)
    (define (good-enough? guess x)
        (< (abs (- (square guess) x)) 0.001))
    (define (improve guess x)
        (average guess (/ x guess)))
    (define (sqrt-iter guess x)
        (if (good-enough? guess x)
            guess
            (sqrt-iter (improve guess x) x)))
    (sqrt-iter 1.0 x))
    ```
    Such nesting of definitons is called *block structure*.

- Actually it is not necessary to pass `x` explicitly to each of the procedures in the above 
procedures `sqrt`. We allowed `x` to be a free variable in the internal definitions:
    ```lisp
    (define (sqrt x)
    (define (good-enough? guess)
        (< (abs (- (square guess) x)) 0.001))
    (define (improve guess)
        (average guess (/ x guess)))
    (define (sqrt-iter guess)
        (if (good-enough? guess)
            guess
            (sqrt-iter (improve guess))))
    (sqrt-iter 1.0))
    ```
    This is called *lexical scoping*.


## 1.2 Procedures and the Process They Generate
A procedure is a pattern for the *local evolution* of a computational process. It specifies how each
 stage of the process is built upon the previous stage.

### 1.2.1 Linear Recursion and Iteraion
- Provide two different way to compute `6!`, One is linear recursive process:
    ```lisp
    (define (factorial n)
    (if (= n 1)
        1
        (* n (factorial (- n 1)))))
    ```
    The evaluation be like:
    ```lisp
    (factorial 6)
    (* 6 (factorial 5))
    (* 6 (* 5 (factorial 4)))
    (* 6 (* 5 (* 4 (factorial 3))))
    (* 6 (* 5 (* 4 (* 3 (factorial 2)))))
    (* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))
    (* 6 (* 5 (* 4 (* 3 (* 2 1)))))
    (* 6 (* 5 (* 4 (* 3 2))))
    (* 6 (* 5 (* 4 6)))
    (* 6 (* 5 24))
    (* 6 120)
    720
    ```
    The expansion occurs as the process builds up a chain of *deferred operations*. The contraction 
    occurs as the operations are actually performed. This type of process, characterized by a chain 
    of deferred operations, is called a *recursive process*. Carrying out this process requires that
    the interpreter keep track of teh operations to be performed later on. In the computation of n!,
    the length of the chain of deferred multiplications, and hence the amount of information needed 
    to keep track of it, grows linearly with n (is proportional to n), just like the number of 
    steps. Such a process is called a *linear recursive process*.

    Another is linear iterative process:
    ```lisp
    (define (factorial n)
    (fact-iter 1 1 n))

    (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                    (+ counter 1)
                    max-count)))
    ```

    The evaluation be like:
    ```lisp
    (factorial 6)
    (fact-iter   1   1   6)
    (fact-iter   1   2   6)
    (fact-iter   2   3   6)
    (fact-iter   6   4   6)
    (fact-iter  24   5   6)
    (fact-iter 120   6   6)
    (fact-iter 720   7   6)
    ```
    - *Iterative process*: a process is one whose state can be summarized by a fixed number of
    *state variables*.
    - *State variables*: a fixed number which in the iterative process and can be related to the 
    variables.
    - *Linear iterative process*: the process whose number of steps required grows linearly with n.

### 1.2.2 Tree Recursion

#### Example: Counting change

### 1.2.3 Orders of Growth

### 1.2.4 Exponentiation

### 1.2.5 Greatest Common Divisors

### 1.2.6 Example: Testing for Primality

#### Searching for divisors

```lisp
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
```

- test-devisor cannot be larger than `(sqrt n)` so the number of steps required to identify a prime
number is `\theta((sqrt(n)))`

#### The Fermat test

```lisp
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))        

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
```

## 1.3 Formulating Abstractions with Higher-Order Procedures

One of the things  we should demand from a powerful programming language is the ability to build 
abstractions by assigining names to common patterns and then to work in terms of the abstractions 
directly. 

Procedures that manipulate procedures are called *higer-order procedures*.

### 1.3.1 Procedures as Arguments
- The template concluded by the three examples's pattern:
    ```lisp
    (define (<name> a b)
        (if (> a b)
            0
            (+ (<term> a)
                (<name> (<next> a) b))))
    ```

### 1.3.2 Constructing Procedures Using `Lambda`
Rather than define some procedures by `(define)`, it would be more convenient to have a way to 
directly specify their functionality by using `lambda`, for example:
```lisp
(lambda (x) (+ x 4))
```
and
```lisp
(lambda (x) (/ 1.0 (* x (+ x 2))))
```

In general, `lambda` is used to create procedures in the same way as `define`, except that no name 
is specified for the procedures, template be like:
```lisp
(lambda (<formal-parameters>) <body>)
```

we can read a lamda expression as follows
```lisp
(lambda             (x)                     (+          x       4))
  THE PRODUCRE      OF AN ARGUMENT OF X   THAT ADDS     X  &    4
```

a `lambda` expression can be used as the operator in a combination such as:
```lisp
((lambda (x y z) (+ x y (square z))) 1 2 3)
```

#### Using `let` to create local variables
For examples:
```lisp
(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y)) 
            (- 1 y)))
```
We could use a `lambda` expression to specify an anoymous procedures for binding our local 
variables:
```lisp
(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))
```
But we could use `let` to make it more convenient:
```lisp
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
```
The general form of a `let` expression is:
```lisp
(let ((<var1> <exp1>)
      (<var2> <exp2>)
      
      (<varn> <expn>))
   <body>)
```
which can be thought of as saying
```
let	<var1> have the value <exp1> and
    <var2> have the value <exp2> and
    .
    .
    .
    <varn> have the value <expn>
in	<body>
```
The first part of the `let` expression is a list of name-expression pairs. When the `let` is 
evaluated, each name is associated with the value of the corresponding expression. The body of the 
`let` is evaluated with these names bound as local variables. The way this happens is that the `let`
expression is interpreted as an alternate syntax for
```lisp
((lambda (<var1> ...<varn>)
    <body>)
 <exp1>
 
 <expn>)
```

A `let` expression is simply syntactic sugar for the underlying `lambda` application.
We can see from this equivalence that the scope of a variable specified by a `let` expression is the
body of the `let`. This implies that:
- `let` allows one to bind variables as locally as possible to where they are to be used. For 
    example, if the value of x is 5, the value of expression
    ```lisp
    (+ (let ((x 3))
            (+ x (* x 10)))
        x)
    ```
    is 38.
- The variables's values are computed outside the `let`. This matters when the expressions that 
    provide the values for the local variables depend upon variables having the same names as the 
    local variables themselves. For example, if the value of `x` is 2, the expression
    ```lisp
    (let ((x 3)
          (y (+ x 2)))
        (* x y))
    ```
    will have the value 12. inside body of the `let`, `x` will be 3 and `y` will be 4 (which is the
    outer `x` plus 2).

We prefer to use internal `define` only for internal procedures, and for defining local variables,
we prefer `let`.

### 1.3.3 Procedures as General Methods
#### Finding roots of equations by the half-interval method

#### Finding fixed points of functions

### 1.3.4 Procedures as Returned Values
Example in the book:
```lisp
(define (average-damp f)
    ((lambda (x) (average x (f x)))))
```
We can use `average-damp` to reformulate the sqaure-root procedure as follows:
```lisp
(define (sqrt x)
    (fixed-point (average-damp (lambda (y) (/ x y)))
                1.0))
```
this formulation makes explicit the three ideas in the method: fixed-point search, average damping, 
and the function `y -> x/y`.

#### Newton's method

#### Abstraction and first-class procedures
- Elements with the fewest restrictions are said to have first-class status. Some of the ``right and
priviledges`` of first-class elements are:
    - They may be named by variables.
    - They may be passed as arguments to procedures.
    - They may be returned as the results of procedures.
    - They may be included in data structures.