# Chapter 3 Modularity, Objects, and State

<p style="color:#FF6666; font-weight: bold; font-style: italic"> All the codes and some sentences in 
this note are from the book: SICP <p>

# Chapter 3.1 Assignment and Local State

- Each computational object must have its own `local state variables` describing the actual object's 
state. Since the states of objects in the system being modeled change over time, the state variables
of the corresponding computational objects must also change. 

- If we wish to model state variables by ordinary symbolic names in the programming language, then
the language must provide an `assignment operator` to enable us to change the value associated with 
a name.

## Chapter 3.1.1 Local State Variables

- The book gives an example use the `set!` special form, whose syntax is:
  ```scheme
  (set! <name> <new-value>)
  ```
  Here `<name>` is a symbol and `<new-value>` is any expression. `Set!` changes `<name>` so that its
  value is the result obtained by evaluating `<new-value>`.
  
  In general, evaluating the expression:
  ```scheme
  (begin <exp_1> <exp_2> ... <exp_k>)
  ```
  causes the expressions `<exp_1>` though `<exp_k>` to be evaluated in sequence and the value of the
  final expression `exp_k` to be returned as the value of the entire `begin` form.

- As soon as we introduce assignment into our language, `substitution` is no longer an adequate 
  model of procedure application.

## Chapter 3.1.2 The benefits of Introducing Assignment

- Viewing systems as collections of objects with local state is a powerful technique for maintaining
a modular design.

- The book gives the Monte Carlo example, illustrates the phenomenon that: From the point of view of
one part of a complex process, the other parts appear to change with time. They have hidden 
time-varying local state. If we wish to write computer programs whose structure reflects this
decomposition, we make computational objects whose behaviour changes with time. We model state with 
local state variables, and we model the changes of state with assignments to those variables.

- By introducing assignment and the technique of hiding state in local variables, we *may* be able 
to structure systems in a more modular fashion than if all state had to be manipulated explicitly, 
by passing additional parameters.

## Chapter 3.1.3 The Costs of Introducing Assignment

- Our programming can no longer be interpreted in terms of the substitution model of procedure
application wehn we use `set!` operation to model objects taht have local state.

- Programing without any use of assignments is accordingly known as `funcitonal programming`.

- The book gives an example to show why substitution cannot be used when the operation `set!` be
called:
```scheme
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

((make-simplified-withdraw 25) 20)

((lambda (amount) (set! balance (- 25 amount)) 25) 20)

(set! balance (- 25 20)) 25 ;; this will set balance to 5 while returning 25
```

- The trouble here is that substitution is based ultimately on the notion that the symbols in our
language are essentially names for values.

### Sameness and change

- A language that supports the concept that *`equal can be substituted for equals`* in an expression
without changing the value of the expression is said to be `referentially transparent`.

- We cannot determine `change` without some a priori notion of `sameness`, and we cannot determine 
sameness without observing the effects of change.

### Pitfalls of imperative programming

- In contrast to functional programming, programming that makes extensive use of assignment is known
as `imperative progamming`.

- In general, programming with assignment forces us to carefully consider the relative orders of the 
assignments to make sure that each statement is using the correct version of the variables that have
been changed. 

