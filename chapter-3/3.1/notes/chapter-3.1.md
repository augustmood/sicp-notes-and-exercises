# Chapter 3 Modularity, Objects, and State

<p style="color:#FF6666; font-weight: bold; font-style: italic"> All the codes and some sentences in 
this note are from the book: SICP <p>

# Chapter 3.1 

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


