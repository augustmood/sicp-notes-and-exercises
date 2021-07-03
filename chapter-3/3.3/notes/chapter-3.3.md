# Chapter 3.3 Modeling with Mutable Data
<p style="color:#FF6666; font-weight: bold; font-style: italic"> All the codes and some sentences in 
this note are from the book: SICP <p>

- In order to model compound objecs with chaging state, we will design data abstractions to include, 
in addtion to selectors and constructors, operations called `mutators`, which modify data objects.

- Data objects for which mutators are defined are known as `mutable data objects`.

## 3.3.1 Mutable List Structure

- `set-car!` and `set-cdr!` return implementation-dependent values. Like set!, they should be used 
only for their effect.

- The book gives an example of implementing `cons`:
    ```scheme
    (define (cons x y)
        (let ((new (get-new-pair)))
            (set-car! new x)
            (set-cdr! new y)
            new))
    ```

### Sharing and identity

- The mutation operations `set-car!` and `set-cdr!` should be used with care; unless we have a good
understanding of how our data objects are shared, mutation can have unanticipated results.
