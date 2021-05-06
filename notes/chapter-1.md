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
