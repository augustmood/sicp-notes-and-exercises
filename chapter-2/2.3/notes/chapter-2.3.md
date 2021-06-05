# Chapter 2.3 Symbolic Data

Introducing the ability of our language to work with arbitrary symbols as data.

## 2.3.1 Quotation

- Introducing a new element in our language: the ability to `quote` data object:
    ```lisp
    (quote <expression>)
    ```
    The quotation mark `'` is just a single-character abbreviation for wrapping the next complete 
    expression with `quote` to form `(quote <expression>)`

- The book gives some examples:
    ```lisp
    (define a 1)

    (define b 1)

    (list a b)
    > (1 2)

    (list 'a 'b)
    > (a b)

    (list 'a b)
    > (a 2)
    ```

- Quotation also allows us to type in compound objects, using the conventional printed 
representation for lists:
    ```lisp
    (car '(a b c))
    > a

    (cdr '(a b c))
    > (b c)
    ```
    We can obtain the empty list by evaluating `'()`, and thus dispense with the variable `nil`.

- One additional primitve used in manipulating symbols is `eq?`, which takes two symbols as
arguments and tests whether they are the same:
    ```lisp
    (define (memq item x)
        (cond ((null? x) false)
                ((eq? item (car x)) x)
                (else (memq item (cdr x)))))
    ```

## 2.3.2 Example: Symbolic Differentiation

- In developing the symbolic-differentiation program, we will first define a differentiation 
algorithm that operates on abstract objects such as ``sums``, ``product``, and ``variables`` without
worrying about how these are to be prepared.

    ### The differentiation program with abstract data

    - The book gives an example of implementing the simple symbolic-differentiation:
        ```lisp
        (define (deriv exp var)
            (cond ((number? exp) 0)
                    ((variable? exp)
                        (if (same-variable? exp var) 1 0))
                    ((sum? exp)
                        (make-sum (deriv (addend exp) var)
                                (deriv (augend exp) var)))
                    ((product? exp)
                        (make-sum
                        (make-product (multiplier exp)
                                        (deriv (multiplicand exp) var))
                        (make-product (deriv (multiplier exp) var)
                                        (multiplicand exp))))
                    (else
                        (error "unknown expression type -- DERIV" exp))))
        ```
    
    ### Representing algebraic expressions
