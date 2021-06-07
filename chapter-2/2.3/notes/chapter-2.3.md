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

## 2.3.3 Example: Representing Sets

- A set is simply a collection of distinct objects: we define `set` by specifying the operation that
are to be used on sets, like `union-set`, `intersection-set`, `element-of-set`?, and `adjoin-set`.

    ## Sets as unordered lists

    - Giving few examples of implementation of these procedures:
        ```lisp
        (define (element-of-set? x set)
            (cond ((null? set) false)
                  ((equal? x (car set)) true)
                  (else (element-of-set? x (cdr set)))))
        ```

        ```lisp
        (define (adjoin-set x set)
            (if (element-of-set? x set)
                set
                (cons x set)))
        ```

        ```lisp
        (define (intersection-set set1 set2)
            (cond ((or (null? set1) (null? set2)) '())
                  ((element-of-set? (car set1) set2)        
                   (cons (car set1)
                    (intersection-set (cdr set1) set2)))
                  (else (intersection-set (cdr set1) set2))))
        ```
        So the third one, is just go through the whole set1, and for each element we might go
        through the whole set2 to check if the element is already there. And this way might not that
        efficient as the work is `O(n^2)`.

    ## Sets as ordered lists

    - In order to sort, we might need to compare two objects so that we can say which is bigger, we 
    may compare symbols lexicographically, for instance. and we could also use some methods for 
    assigning a unique number to an object and then do the comparision.

    - If the set is ordered, then the element-of-set? is gonna be somewhat effiecient:
        ```lisp
        (define (element-of-set? x set)
            (cond ((null? set) false)
                    ((= x (car set)) true)
                    ((< x (car set)) false)
                    (else (element-of-set? x (cdr set)))))
        ```
        since if we reach a set element that is larger than the item we are looking for, then we 
        could know that the item is not in the set.

        On average, we should expect to have examine about half of the items in the set. Although 
        this is still $\Theta(n)$

    - We obtain a more impressive speedup with `intersection-set`, unordered one required 
      $\Theta(n^2)$ steps, as we need to performed a complete scan of set2 for each element in set1.
      While in ordered representation, we could do this in a much better way:
      ```lisp
       (define (intersection-set set1 set2)
            (if (or (null? set1) (null? set2))
                '()    
                (let ((x1 (car set1)) (x2 (car set2)))
                    (cond ((= x1 x2)
                        (cons x1
                                (intersection-set (cdr set1)
                                                (cdr set2))))
                        ((< x1 x2)
                        (intersection-set (cdr set1) set2))
                        ((< x2 x1)
                        (intersection-set set1 (cdr set2)))))))
        ```
        And the required steps is about $\Theta(n)$ instead of $\Theta(n^2)$.
