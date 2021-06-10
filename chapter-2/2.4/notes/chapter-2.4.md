# Chapter 2.4 Multiple Representations for Abstract Data

<p style="color:#FF6666; font-weight: bold; font-style: italic"> All the codes and some sentences in 
this note are from the book: SICP <p>

- There might be more than one useful representation for a data object, for example, for 
    `complex numbers` sometimes the rectangular form is more appropriate and sometimes polar form
    is more appropriate. So it is perfectly plausible to imagine a system in which complex numbers 
    are represented in both ways.

- In addition to the data-abstraction barriers that isolate representation from use, we need 
    abstraction barriers that isolate different design choices from each other and permit different 
    choices to coexist in a single program.

- We need conventions that permit programmers to incorporate modules into larger systems 
    `additively`, that is, without having to redesign or reimplement these modules.

- `generic procedures`: procedures that can operate on data that may be represented in more than one
    way.

- `type tags`: explicit information about how they are to be processed.

- `data-directed programming`: a powerful and convenient implementation strategy for additively 
    assembling systems with generic operations.

## 2.4.1 Representations for Complex Numbers

- The book gives an example of implementing `complex numbers` package in two different ways:
    ```lisp
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (magnitude z)
        (sqrt (+ (square (real-part z)) (square (imag-part z)))))
    (define (angle z)
        (atan (imag-part z) (real-part z)))
    (define (make-from-real-imag x y) (cons x y))
    (define (make-from-mag-ang r a) 
        (cons (* r (cos a)) (* r (sin a))))
    ```

    ```lisp
    (define (real-part z)
        (* (magnitude z) (cos (angle z))))
    (define (imag-part z)
        (* (magnitude z) (sin (angle z))))
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-real-imag x y) 
        (cons (sqrt (+ (square x) (square y)))
            (atan y x)))
    (define (make-from-mag-ang r a) (cons r a))
    ```

## 2.4.2 Tagged data

- One way to view data abstraction is as an application of `principle of least commitment`. The 
    abstraction barrier formed by the selectors and constructors permits us to defer to the last 
    possible moment the choice of a concrete representation for our data objects and thus retain 
    maximum flexibility in our system design.

- In order to manipulate tagged data, we will assume that we have procedures `type-tag` and
    `contents` that extract from a data object the tag and the actual contents, we will also 
    postulate a procedure `attach-tag` taht takes a tag and contents and produces a tagged data 
    object.

- The book gives an example of how to implement this:
    ```lisp
    (define (attach-tag type-tag contents)
        (cons type-tag contents))
    (define (type-tag datum)
        (if (pair? datum)
            (car datum)
            (error "Bad tagged datum -- TYPE-TAG" datum)))
    (define (contents datum)
        (if (pair? datum)
            (cdr datum)
            (error "Bad tagged datum -- CONTENTS" datum)))
    ```

    ```lisp
    (define (rectangular? z)
        (eq? (type-tag z) 'rectangular))
    (define (polar? z)
        (eq? (type-tag z) 'polar))
    ```

    And thus, the previous representations of `complex numbers` can be improved as:

    ```lisp
    (define (real-part-rectangular z) (car z))
    (define (imag-part-rectangular z) (cdr z))
    (define (magnitude-rectangular z)
        (sqrt (+ (square (real-part-rectangular z))
                (square (imag-part-rectangular z)))))
    (define (angle-rectangular z)
        (atan (imag-part-rectangular z)
                (real-part-rectangular z)))
    (define (make-from-real-imag-rectangular x y)
        (attach-tag 'rectangular (cons x y)))
    (define (make-from-mag-ang-rectangular r a) 
        (attach-tag 'rectangular
                    (cons (* r (cos a)) (* r (sin a)))))
    ```

    ```lisp
    (define (real-part-polar z)
        (* (magnitude-polar z) (cos (angle-polar z))))
    (define (imag-part-polar z)
        (* (magnitude-polar z) (sin (angle-polar z))))
    (define (magnitude-polar z) (car z))
    (define (angle-polar z) (cdr z))
    (define (make-from-real-imag-polar x y) 
        (attach-tag 'polar
                    (cons (sqrt (+ (square x) (square y)))
                            (atan y x))))
    (define (make-from-mag-ang-polar r a)
        (attach-tag 'polar (cons r a)))
    ```


    ```lisp
    (define (real-part z)
        (cond ((rectangular? z) 
                (real-part-rectangular (contents z)))
                ((polar? z)
                (real-part-polar (contents z)))
                (else (error "Unknown type -- REAL-PART" z))))

    (define (imag-part z)
        (cond ((rectangular? z)
                (imag-part-rectangular (contents z)))
                ((polar? z)
                (imag-part-polar (contents z)))
                (else (error "Unknown type -- IMAG-PART" z))))

    (define (magnitude z)
        (cond ((rectangular? z)
                (magnitude-rectangular (contents z)))
                ((polar? z)
                (magnitude-polar (contents z)))
                (else (error "Unknown type -- MAGNITUDE" z))))

    (define (angle z)
        (cond ((rectangular? z)
                (angle-rectangular (contents z)))
                ((polar? z)
                (angle-polar (contents z)))
                (else (error "Unknown type -- ANGLE" z))))
    
    (define (make-from-real-imag x y)
        (make-from-real-imag-rectangular x y))

    (define (make-from-mag-ang r a)
        (make-from-mag-ang-polar r a))
    ```

## 2.4.3 Data-Directed Programming and Additivity

- The general strategy of checking the type of a datum and calling an appropriate procedure is 
    called *`dispatching on type`*.