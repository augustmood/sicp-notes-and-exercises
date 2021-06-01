# Chapter 2.2 Hierarchical Data and the Closure Property

- Pairs provide a primitve *glue* that we can use to construct compound data objects.

- The ability to create pairs whose elementary elements are pairs is the essence of list structure's
importance as a representation tool, this ability is the *closure property* of cons

- *Hierarchical structures*: structures made up of parts, which themselves are made up of of parts, 
and so on.


## 2.2.1 Representing Sequences

- *sequence*: an ordered collection of data objects.
    - examples:
    ```lisp
    (cons 1
        (cons 2
            (cons 3
                (cons 4 nil)))
    ```
        
    - The `car` of each pair is the corresponding item in the chain, and the `cdr` of the pair is the 
    the next pair in the chain.

    - `nil`: the `cdr` of the final pair signals the end of the sequence by pointing to a 
    distinguished value that is not a pair. the word `nil` is a contraction of the Latin word
    `nihil`, which means nothing. [link 10](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#footnote_Temp_158).
     Btw, there's no nil in racket, we can still use it by using *sicp* package. 

- A sequence of pairs, formed by nested `cons`es, is called a list. There's a primitive `list` in 
Scheme. The code in above example is equivalent to:
   - examples:
        ```lisp
        (list 1 2 3 4)
        ```
   - In general,
        ```lisp
        (list <a_1> <a_2> ... <a_n>) === (cons <a_1> (cons <a_2> (cons ... (cons <a_n> nil))))
        ```

   - Another example: 
        ```lisp
        > (define test (list 1 2 3 4))
        > test
        '(1 2 3 4)
        > (car test)
        1
        > (car (cdr test))
        2
        > (car (cdr (cdr test)))
        3
        > (car (cdr (cdr (cdr test))))
        4
        ```
    Don't messed up (list 1 2 3 4) and (1 2 3 4)...

    ### List Operations

    - The book gives an example of the procedure `list-ref`:
        ```lisp
        (define (list-ref items n)
            (if (= n 0)
                (car items)
                (list-ref (cdr items) (- n 1))))
        (define squares (list 1 4 9 16 25))

        (list-ref squares 3)
        16
        ```
    - Scheme has a primitive predicate `null?`, which tests whether its argument is the empty
    list, example:
        ```lisp
        (define (length items)
            (if (null? items)
                0
                (+ 1 (length (cdr items)))))
        (define odds (list 1 3 5 7))
        (length odds)
        4
        ```
        ofc, the `length` in an iterative style:
        ```
        (define (length items)
            (define (length-iter a count)
                (if (null? a)
                    count
                    (length-iter (cdr a) (+ 1 count))))
            (length-iter items 0))
        ```
    - Another conventional programming technique is to `cons` up an answer list while cdring down a 
    list:
        ```lisp
        (append squares odds)
        (1 4 9 16 25 1 3 5 7)

        (append odds squares)
        (1 3 5 7 1 4 9 16 25)
        ```
        *`Append` is also implemented using a recursive plan:*

        ```lisp
        (define (append list1 list2)
            (if (null? list1)
                list2
                (cons (car list1) (append (cdr list1) list2))))
        ```
    
    ### Mapping over lists
    The book gives an example:
    ```lisp
    (define (scale-list items factor)
        (if (null? items)
            nil
            (cons (* (car items) factor)
                    (scale-list (cdr items) factor))))
    (scale-list (list 1 2 3 4 5) 10)
    (10 20 30 40 50)
    ```

    There's a higher-order procedure called *`map`*:
    ```lisp
    (define (map proc items)
        (if (null? items)
            nil
            (cons (proc (car items))
                    (map proc (cdr items)))))
    ```

    `map` examples:
    ```lisp
    (map abs (list -10 2.5 -11.6 17))
    (10 2.5 11.6 17)
    ```

    Thus, the scale-list can be re-written as:
    ```lisp
    (defie (scale-list items factor)
        (map (lambda (x) (* x factor)) items))
    ```

    - `map` helps establish an abstraction barrier that isolate the implementation of procedures 
    that transform lists from the details of how the elements of the list are extracted and 
    combined.

## 2.2.2 Hierarchical Structures

- Scheme provides primitive predicate `pair?`

- The book gives an example to count leaves:
    ```lisp
    (define (count-leaves x)
        (cond ((null? x) 0)  
                ((not (pair? x)) 1)
                (else (+ (count-leaves (car x))
                        (count-leaves (cdr x))))))
    ```

    ### Mapping over trees

    - The book gives an example:
    
        `Scale-tree` in recursive way:

        ```lisp
        (define (scale-tree tree factor)
            (cond ((null? tree) nil)
                    ((not (pair? tree)) (* tree factor))
                    (else (cons (scale-tree (car tree) factor)
                                (scale-tree (cdr tree) factor)))))
        > (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
                        10)
        > (10 (20 (30 40) 50) (60 70))
        ```

        We can implement this by using `map`:

        ```lisp
        (define (scale-tree tree factor)
            (map (lambda (sub-tree)
                    (if (pair? sub-tree)
                        (scale-tree sub-tree factor)
                        (* sub-tree factor)))
                tree))
        ```

## 2.2.3 Sequences as Conventional Interfaces

- *`conventional interfaces`*

    ### Sequence Operations
    - `filter`:
        ```lisp
        (define (filter predicate sequence)
            (cond ((null? sequence) nil)
                  ((predicate (car sequence))
                    (cons (car sequence)
                        (filter predicate (cdr sequence))))
                  (else (filter predicate (cdr sequence)))))
        ```
    
    - `accumulate`:
        ```lisp
        (define (accumulate op initial sequence)
            (if (null? sequence)
                inital
                (op (car sequence)
                    (accumulate op initial (cdr sequence)))))
        ```

        - Examples:
            ```lisp
            (accumulate + 0 (list 1 2 3 4 5))
            15
            (accumulate * 1 (list 1 2 3 4 5))
            120
            ```
    
    - *modular design*: designs that are constructed by combining relatively independent pieces, it 
    is a powerful strategy for controlling complexity in engineering design.

    ### Nested Mappings
    
    - The book gives an example of solving the problem: Given a positive integer n, find all ordered
     pairs of distinct positive integers i and j, where 1 < j < i < n, such that i + j is prime:

        ```lisp
        (accumulate append
            nil
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n)))
        ```

        - Enumerating `i` : `(enumerate-interval 1 n)`

        - Enumerating `j` : `(enumerate-interval 1 (- i 1))`

        - Pairing `j`s with single `i`:
            ```lisp
            (map (lambda (j) (list i j))
                (enumerate-interval 1 (- i 1)))
            ```
        
        - Mapping the pairing function over the list `i`:
            ```lisp
            (map (lambda (i)
                    (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                  (enumerate-interval 1 n))
            ```
        
        - Then using `accumulate` to append all the `(i j)` pairs together, since this kind of 
        combination of mapping and accumulating with `append` is common, the book gives a procedure:
            ```lisp
            (define (flatmap proc seq)
                (accumulate append nil (map proc seq)))
            ```

        - We also need a filter procedure to determine whether the sum of elements in the given
        pairs is a prime number:
            ```lisp
            (define (prime-sum? pair)
                (prime? (+ (car pair) (cadr pair))))
            ```
        
        - And we may need to add the sums to the orginal pairs:
            ```lisp
            (define (make-pair-sum pair)
                (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
            ```
            The reason we using `cadr` is that we construct the original pair by using `list`.

        - The complete procedure:
            ```lisp
            (define (prime-sum-pairs n)
                (map make-pair-sum
                    (filter prime-sum?
                            (flatmap
                                (lambda (i)
                                (map (lambda (j) (list i j))
                                    (enumerate-interval 1 (- i 1))))
                                (enumerate-interval 1 n)))))
            ```

            
    <p style="color:red; font-weight: bold; font-style: italic"> 
    All the codes in this note are from the book: SICP <p>
        
    - Another example is generating all the permutation of a given set S:
        ```lisp
        (define (permutations s)
            (if (null? s)                    ; empty set?
                (list nil)                   ; sequence containing empty set
                (flatmap (lambda (x)
                            (map (lambda (p) (cons x p))
                                (permutations (remove x s))))
                        s)))

        (define (remove item sequence)
            (filter (lambda (x) (not (= x item)))
                sequence))
        ```