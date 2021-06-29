# Chapter 3.2 The Environment Model of Evaluation
<p style="color:#FF6666; font-weight: bold; font-style: italic"> All the codes and some sentences in 
this note are from the book: SICP <p>

- What is meant by applying a procedure to arguments:
    
    To apply a compound procedure to arguments, evaluate the body of the procedure with each formal
    parameter replaced by the corresponding argument.

- Once we admit assignment into our programming language, such a definition is no long adequate.
  
- An environment is a sequence of frames, each frames is a table of `bindings`, which associate
variable names with their corresponding values. Each frame also has a pointer to its `enclosing
environment`, unless, for the purposes of dicussion, the frame is considered to be `global`. The
`value of a variable` with respect to an environment is the value given by the binding of the 
variable in the first frame in the environment that contains a binding for that variable. If no 
frame in the sequence specifies a binding for the variable, then the variable is said to be 
`unbound` in the environment.

## 3.2.1 The Rules of Evaluation

- In the environment model of evaluation, a procedure is always a pair consisting of some code and a
pointer to an environment. Procedures are created in one way only: by evaluating a `lambda` 
expression. This produces a procedure whose code is obtained from the text of the `lambda` 
expression and whose environment is the environment in which the lambda expression was evaluated to 
produce the procedure.

- `define` creates definitions by adding bindings to frames.

- Evaluating the expression `(set! <variable> <value>)` in some environment locates the binding of
the variable in the environment and changes that binding to indicate the new value. That is, one
finds the first frame in the environment that contains a binding for the variable and modifies that 
frame.

## 3.2.2 Applying Simple Procedures

## 3.2.3 Frames as the Repository of Local State

- 
