# Chapter 2.3 Symbolic Data

<p style="color:#FF6666; font-weight: bold; font-style: italic"> All the codes and some sentences in 
this note are from the book: SICP <p>

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

    ### Sets as ordered lists

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

    ### Sets as binary trees
    - Each node of the tree holds one element of the set, called the `entry` at that node, and a 
    link to each of two other nodes:
        - The `left` link points to elements smaller than the one at the node.
        - The `right` link to elements greater than the one at the node.
  
    - The only thing we require for a valid representation is that all elements in the left subtree 
    be smaller than the node entry and that all elements in the right subtree be larger.

    - Required work of searching in tree is $\mathcal{O}(log_n)$. (if the tree is `balanced`)

    - We can represent trees by using lists. Each node will be a list of three items: `node`, `the 
   left subtree`, `the right subtree`.

   - The book gives an example of implementation:
        ```lisp
        (define (entry tree) (car tree))
        (define (left-branch tree) (cadr tree))
        (define (right-branch tree) (caddr tree))
        (define (make-tree entry left right)
        (list entry left right))

        (define (element-of-set? x set)
            (cond ((null? set) false)
                    ((= x (entry set)) true)
                    ((< x (entry set))
                    (element-of-set? x (left-branch set)))
                    ((> x (entry set))
                    (element-of-set? x (right-branch set)))))
        
        (define (adjoin-set x set)
            (cond ((null? set) (make-tree x '() '()))
                    ((= x (entry set)) set)
                    ((< x (entry set))
                    (make-tree (entry set) 
                                (adjoin-set x (left-branch set))
                                (right-branch set)))
                    ((> x (entry set))
                    (make-tree (entry set)
                                (left-branch set)
                                (adjoin-set x (right-branch set))))))
        ```

    ### Sets and information retrieval

    - Giving an example of implementing the procedure `lookup` if the records is implemented as un
    unordered list:
        ```lisp
        (define (lookup given-key set-of-records)
            (cond ((null? set-of-records) false)
                    ((equal? given-key (key (car set-of-records)))
                    (car set-of-records))
                    (else (lookup given-key (cdr set-of-records)))))
        ```

## 2.3.4 Example: Huffman Encoding Trees

- Code such as ASCII and the A-through-H code above are known as `fixed-length` codes, because 
they represent each symbol in the message with the same number of bits.

- It sometimes advantageous to use variable-length codes, in which different symbols may be 
represented by different numbers of bits. `we can encode data more efficiently if we assign
shorter codes to the frequent symbols.

- `separator code`: in Morse code it's a pause after the sequence of dots and dashes for each other.

- `prefix code`: the code which there's no complete code for any symbol is the beginning (or 
`prefix`) of the code for another symbol.

- A Huffman code can be represented as a binary tree whose leaves are the symbols that are encoded.\

    ### Representing Huffman trees
    - The book gives an example of implementing the representation:
  
        - Leaves of the tree are represented by a list consisting of the symbol `leaf`:
            ```lisp
            (define (make-leaf symbol weight)
                (list 'leaf symbol weight))
            (define (leaf? object)
                (eq? (car object) 'leaf))
            (define (symbol-leaf x) (cadr x))
            (define (weight-leaf x) (caddr x))
            ```
        
        - A general tree will be a list of a left branch, a right branch, a set of symbols, and a 
            weight. The set of symbols will be simply a list of the symbols, rather than some more
            sophisticated set representation:
            ```lisp
            (define (make-code-tree left right)
                (list left
                      right
                      (append (symbols left) (symbols right))
                      (+ (weight left) (weight right))))
            
            (define (left-branch tree) (car tree))

            (define (right-branch tree) (cadr tree))

            (define (symbols tree)
                (if (leaf? tree)
                    (list (symbol-leaf tree))
                    (caddr tree)))

            (define (weight tree)
                (if (leaf? tree)
                    (weight-leaf tree)
                    (cadddr tree)))
            ```
    ### The decoding procedure

    - The book gives an example of implementing the decoding algorithm:
        ```lisp
        (define (decode bits tree)
            (define (decode-1 bits current-branch)
                (if (null? bits)
                    '()
                    (let ((next-branch
                        (choose-branch (car bits) current-branch)))
                    (if (leaf? next-branch)
                        (cons (symbol-leaf next-branch)
                                (decode-1 (cdr bits) tree))
                        (decode-1 (cdr bits) next-branch)))))
            (decode-1 bits tree))

        (define (choose-branch bit branch)
            (cond ((= bit 0) (left-branch branch))
                  ((= bit 1) (right-branch branch))
                  (else (error "bad bit -- CHOOSE-BRANCH" bit))))
        ```

    ### Sets of weighted elements

    -  We will represent a set of leaves and trees as a list of elements, arragned in increasing 
    order of weight,

    - The book gives an example of implementing `adjoin-set` procedure for constructing sets:
        ```lisp
        (define (adjoin-set x set)
            (cond ((null? set) (list x))
                    ((< (weight x) (weight (car set))) (cons x set))
                    (else (cons (car set)
                                (adjoin-set x (cdr set))))))
        ```

        The following procedure takes a list of symbol-frequency pairs such as 
        ((A 4) (B 2) (C 1) (D 1)) and constructs an initial ordered set of leaves, ready to be 
        merged according to the Huffman algorithm:

        ```lisp
        (define (make-leaf-set pairs)
            (if (null? pairs)
                '()
                (let ((pair (car pairs)))
                    (adjoin-set (make-leaf (car pair)    ; symbol
                                        (cadr pair))  ; frequency
                                (make-leaf-set (cdr pairs))))))
        ```