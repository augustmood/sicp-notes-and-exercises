# Chapter 2.5 Systems with Generic Operations

<p style="color:#FF6666; font-weight: bold; font-style: italic"> All the codes and some sentences in 
this note are from the book: SICP <p>

## 2.5.1 Generic Arithmetic Operations

- The book gives an example of implenting arithmetic operations:

    - The generic arithmetic procedures are defined as follows:
        ```scheme
        (define (add x y) (apply-generic 'add x y))
        (define (sub x y) (apply-generic 'sub x y))
        (define (mul x y) (apply-generic 'mul x y))
        (define (div x y) (apply-generic 'div x y))
        ```
        For *ordinary* numbers, which are the primitive numbers of our language, we will tag these
        with the symbol `scheme-number`.

        ```scheme
        (define (install-scheme-number-package)
            (define (tag x)
                (attach-tag 'scheme-number x))    
            (put 'add '(scheme-number scheme-number)
                (lambda (x y) (tag (+ x y))))
            (put 'sub '(scheme-number scheme-number)
                (lambda (x y) (tag (- x y))))
            (put 'mul '(scheme-number scheme-number)
                (lambda (x y) (tag (* x y))))
            (put 'div '(scheme-number scheme-number)
                (lambda (x y) (tag (/ x y))))
            (put 'make 'scheme-number
                (lambda (x) (tag x)))
            'done)
        ```

        And user of the package above will create ordinary numbers by means of the procedure:

        ```scheme
        (define (make-scheme-number n)
            ((get 'make 'scheme-number) n))
        ```

        For rational number:
        ```scheme
        (define (install-rational-package)
            ;; internal procedures
            (define (numer x) (car x))
            (define (denom x) (cdr x))
            (define (make-rat n d)
                (let ((g (gcd n d)))
                (cons (/ n g) (/ d g))))
            (define (add-rat x y)
                (make-rat (+ (* (numer x) (denom y))
                            (* (numer y) (denom x)))
                        (* (denom x) (denom y))))
            (define (sub-rat x y)
                (make-rat (- (* (numer x) (denom y))
                            (* (numer y) (denom x)))
                        (* (denom x) (denom y))))
            (define (mul-rat x y)
                (make-rat (* (numer x) (numer y))
                        (* (denom x) (denom y))))
            (define (div-rat x y)
                (make-rat (* (numer x) (denom y))
                        (* (denom x) (numer y))))
            ;; interface to rest of the system
            (define (tag x) (attach-tag 'rational x))
            (put 'add '(rational rational)
                (lambda (x y) (tag (add-rat x y))))
            (put 'sub '(rational rational)
                (lambda (x y) (tag (sub-rat x y))))
            (put 'mul '(rational rational)
                (lambda (x y) (tag (mul-rat x y))))
            (put 'div '(rational rational)
                (lambda (x y) (tag (div-rat x y))))

            (put 'make 'rational
                (lambda (n d) (tag (make-rat n d))))
            'done)
        (define (make-rational n d)
            ((get 'make 'rational) n d))
        ```

        Also, for complex numbers, we can install a similar package to handle complex numbers, using
        the tag `complex`:

        ```scheme
        (define (install-complex-package)
            ;; imported procedures from rectangular and polar packages
            (define (make-from-real-imag x y)
                ((get 'make-from-real-imag 'rectangular) x y))
            (define (make-from-mag-ang r a)
                ((get 'make-from-mag-ang 'polar) r a))
            ;; internal procedures
            (define (add-complex z1 z2)
                (make-from-real-imag (+ (real-part z1) (real-part z2))
                                    (+ (imag-part z1) (imag-part z2))))
            (define (sub-complex z1 z2)
                (make-from-real-imag (- (real-part z1) (real-part z2))
                                    (- (imag-part z1) (imag-part z2))))
            (define (mul-complex z1 z2)
                (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                                (+ (angle z1) (angle z2))))
            (define (div-complex z1 z2)
                (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                                (- (angle z1) (angle z2))))
            ;; interface to rest of the system
            (define (tag z) (attach-tag 'complex z))
            (put 'add '(complex complex)
                (lambda (z1 z2) (tag (add-complex z1 z2))))
            (put 'sub '(complex complex)
                (lambda (z1 z2) (tag (sub-complex z1 z2))))
            (put 'mul '(complex complex)
                (lambda (z1 z2) (tag (mul-complex z1 z2))))
            (put 'div '(complex complex)
                (lambda (z1 z2) (tag (div-complex z1 z2))))
            (put 'make-from-real-imag 'complex
                (lambda (x y) (tag (make-from-real-imag x y))))
            (put 'make-from-mag-ang 'complex
                (lambda (r a) (tag (make-from-mag-ang r a))))
            'done)
        ```
        Program outside this package can construct complex number either from real and imaginary
        parts or from magnitudes and angles.
        ```scheme
        (define (make-complex-from-real-imag x y)
            ((get 'make-from-real-imag 'complex) x y))
        (define (make-complex-from-mag-ang r a)
            ((get 'make-from-mag-ang 'complex) r a))
        ```

## 2.5.2 Combining Data of Different Types

- We have ignored an important issue: the operations we have defined so far treated the different 
data types as being completely independent. For instance, there are separate packages for adding, 
say, two ordinary numbers, or two complex numbers.

- One way to handle cross-type operations is to design a different procedure for each possible
combination of types for which the operation is valid, for example:
    ```lisp
    (define (add-complex-to-schemenum z x)
        (make-from-real-imag (+ (real-part z) x)
                            (imag-part z)))
        (put 'add '(complex scheme-number)
            (lambda (z x) (tag (add-complex-to-schemenum z x))))
    ```
    The above code extend the complex-number package to make `add` work between complex number and 
    ordinary number.

    The technique works, but it is cumbersome:
    - the cost of introducing a new type is not just the construction of the package of procedures
    for that type but also installation of the procedures that implement the cross-type operations.
    - The method also undermines our ability to combine separate packages additively.

    ### Coercion
    - Often the different data types are not completely independent, and there may be ways by which
    objects of one type may be viewed as being of another type, this is called *`coercion`*.
    - The book gives an implementation of new `apply-generic` since we use `coercion` to manipulate 
    the table:
    
        ```lisp
        (define (apply-generic op . args)
            (let ((type-tags (map type-tag args)))
                (let ((proc (get op type-tags)))
                (if proc
                    (apply proc (map contents args))
                    (if (= (length args) 2)
                        (let ((type1 (car type-tags))
                              (type2 (cadr type-tags))
                              (a1 (car args))
                              (a2 (cadr args)))
                            (let ((t1->t2 (get-coercion type1 type2))
                                (t2->t1 (get-coercion type2 type1)))
                            (cond (t1->t2
                                    (apply-generic op (t1->t2 a1) a2))
                                    (t2->t1
                                    (apply-generic op a1 (t2->t1 a2)))
                                    (else
                                    (error "No method for these types"
                                            (list op type-tags))))))
                        (error "No method for these types"
                                (list op type-tags)))))))
        ```
    - The appropriate transformation between types depends only on the types themselves, not on
    the operation to be applied.

    - But it is still not general enough, when neither of the objects to be combined can be 
    converted to the type of the other it may still be possible to perform the operation by
    converting both objects to a third type.

### Hierarchies of types

    - Introducing a type structure called `tower`.

    - And the book gives an example of redesigning the `apply-generic` procedure: 
        
        For each type, they need to supply a `raise` procedure, which `raise` objects of that type
        one level in the tower. Then when the system is required to operate on objects of different
        types it can successively raise the lower types until all objects are at the same level in 
        the tower.
    
### Inadequacies of hierarchies

## 2.5.3 Example: Symbolic Algebra

### Arithmetic on polynomials

### Representing term lists

- Thoughts about exercise 2.90:
  - If we want to make the polynomial system adapt with our tower system well, we may need to add
  add the type in our tower system. While, I don't really think this is a good idea, as polynomial
  should be an expression but numbers, and our tower-systems are just an system served for numbers.
  I've been stuck for a while, but I'm clearly what I need to implement and implement well as 
  thought, but not work. This is because I literally messed up the `tags` and the `apply-generic`'s
  `get-put` tables, when will the function drop the tag, and when will the attach the tag, is the 
  key-point tothis question, at least in my strategy of implementaion.