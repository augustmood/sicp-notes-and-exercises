# Chapter 2.1 Introduction to Data Abstraction

- The analogous notion for compound data is called *data abstraction*. Data abstraction is a 
methodolodgy that enables us to isolate how a compound data object is used from the details of how 
it is constructed from more primitive data objects.

- The basic idea of data abstraction is to structure the programs that are to use compound data 
objects so that they operate on ``abstract data``. 

## 2.1.1 Example: Arithmetic Operations for Rational Numbers

### Pairs

- to enable us to implement the concrete level of our data abstraction, our language provides a 
compound structure called a *pair*, which can be constructed with the priitive procedure `cons`. 
It takes two arguments and returns a compound data object that contains the two arguments as parts.

- Given a pair, we can extract the parts using `car` and `cdr`. [abt the naming](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-14.html#footnote_Temp_133)

- Data objects constructed from pairs are called *list-structured* data.

- Examples:
```lisp
(define x (cons 1 2))

(car x)
1

(cdr x)
2
```

#### Representing rational numbers

### 2.1.2 Abstraction Barriers

### 2.1.3 What is Meant by Data?

### 2.1.4 Extended Exercise: Interval Arithmetic