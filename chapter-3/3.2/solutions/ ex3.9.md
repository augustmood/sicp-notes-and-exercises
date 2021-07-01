# Exercise 3.9
## Recursive version:
```scheme
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))
```
![](pics/ex3.9/recursive.svg)

## Iterative version:
```scheme
(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
```
![](pics/ex3.9/iterative.svg)
