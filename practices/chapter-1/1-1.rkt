#lang racket
(require test-engine/racket-tests)
;; 1.1 The Elements of Programming

;; Exercise 1.1
10
;; 10
(+ 5 3 4)
;; 12
(- 9 1)
;; 8
(/ 6 2)
;; 3
(+ (* 2 4) (- 4 6))
;; 6
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
;; 19
(= a b)
;; #f
(if (and (> b a) (< b (* a b)))
    b
    a)
;; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;; 16
(+ 2 (if (> b a) b a))
;; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;; 16

;; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 1 3))))) (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3
(define (sum-of-large x y z)
    (- (+ x y z) (min x y z)))

(check-expect (sum-of-large 1 2 3) 5)
(check-expect (sum-of-large 100 10 1) 110)
(check-expect (sum-of-large 98 2 1) 100)
(test)

;; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; if b is postive, we get the sum of a and b, if b is negative, we get the diff between a and b.

;; Exercise 1.5
;Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using 
;applicative-order evaluation or normal-order evaluation. He defines the following two procedures:
;
;(define (p) (p))
;
;(define (test x y)
;  (if (= x 0)
;      0
;      y))
;
;Then he evaluates the expression
;
;(test 0 (p))
;
;What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What
;behavior will he observe with an interpreter that uses normal-order evaluation? Explain your answer.
;(Assume that the evaluation rule for the special form if is the same whether the interpreter is using
;normal or applicative order: The predicate expression is evaluated first, and the result determines
;whether to evaluate the consequent or the alternative expression.)

;; we cannot get out of the recursive call of (p), since we apply applicative-order, we must first 
;; evaluate all the arguments, as (p) is defined as (p) which is a infinite recursive call, so Ben 
;; will never get the value that the function return.


;; Exercise 1.6
;Alyssa P. Hacker doesn't see why if needs to be provided as a special form. ``Why can't I just define
;it as an ordinary procedure in terms of cond?'' she asks. Alyssa's friend Eva Lu Ator claims this can
;indeed be done, and she defines a new version of if:
;
;(define (new-if predicate then-clause else-clause)
;(cond (predicate then-clause)
;        (else else-clause)))
;Eva demonstrates the program for Alyssa:
;
;(new-if (= 2 3) 0 5)
;5
;
;(new-if (= 1 1) 0 5)
;0
;
;Delighted, Alyssa uses new-if to rewrite the square-root program:
;
;(define (sqrt-iter guess x)
;  (new-if (good-enough? guess x)
;          guess
;          (sqrt-iter (improve guess x)
;                     x)))
;What happens when Alyssa attempts to use this to compute square roots? Explain.


;; Since lisp evaluate the function as applicative-order, we must first evaluate all the arguments, so
;; it will evaluate `(sqrt-iter (improve guess x) x)` again and again, regardless of whether the guess
;; is good enough or not, and thus this will go into an infinite loop, and nothing will be returned.

; Exercise 1.7.  The good-enough? test used in computing square roots will not be very effective for 
; finding the square roots of very small numbers. Also, in real computers, arithmetic operations are 
; almost always performed with limited precision. This makes our test inadequate for very large 
; numbers. Explain these statements, with examples showing how the test fails for small and large 
; numbers. An alternative strategy for implementing good-enough? is to watch how guess changes from 
; one iteration to the next and to stop when the change is a very small fraction of the guess. Design 
; a square-root procedure that uses this kind of end test. Does this work better for small and large 
;  numbers?

(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 1234567890123)
;; 1111111.1061109055

; (sqrt 12345678901234)
;; cannot be finished

(sqrt 0.000000002)
;; 0.0312500213124971

(square (sqrt 0.000000002))
;; 0.000976563832031523

;; When the input is very large number, most of the computation cannot be finished.
;; When the input is very small. the returned square root is not precise.

(define (test-sqrt-iter guess one)
    (if (good-enough? guess one)
        guess
        (begin (println guess)
                (test-sqrt-iter (improve guess one) one))))

; (define (improve guess x)
;   (average guess (/ x guess)))

; (test-sqrt-iter 1.0 12345678901234)

;; For large numbers, the above call cannot be done since after multiple recursive calls, as the guess
;; reaches 3513641.8288200637, it cannot be improved anymore.

(improve 3513641.8288200637 12345678901234)
(average 3513641.8288200637 (/ 12345678901234 3513641.8288200637))
(abs (- (square 3513641.8288200637) 12345678901234))

;; and the different between the square-guess and the x will stuck at 0.001953125 which is bigger than
;; 0.001, thus back to the sqrt-iter, since it doesn't meet the condition of the predicate it will 
;; call itself again, and again and again...

;; For small numbers, the above call can be finished but not precise at all.
(test-sqrt-iter 1.0 0.0000000000000023)
;; Since the target parameter which we're going to get its square-root is pretty small, as the guess
;; reaches to a very small number as well, the square of it are also pretty small, and at some point 
;; that the diff between square guess and x is smaller than 0.001, it will immediately return the 
;; value, regardless of whether it is correct/precise or not.

;; improved version:

; (define (square x) (* x x))

; (define (sqrt-iter guess x)
;   (if (good-enough? guess x)
;       guess
;       (sqrt-iter (improve guess x)
;                  x)))

; (define (improve guess x)
;   (average guess (/ x guess)))

; (define (average x y)
;   (/ (+ x y) 2))

; (define (good-enough? guess x)
;   (< (abs (- (square guess) x)) 0.001))

; (define (sqrt x)
;   (sqrt-iter 1.0 x))

(define (improved-good-enough? prev-guess guess)
    (< (abs (- prev-guess guess)) 0.000000000000000001))

(define (improved-sqrt-iter guess x)
  (if (improved-good-enough? guess (improve guess x))
      guess
      (improved-sqrt-iter (improve guess x) x)))

(define (final-checker guess x func-1 func-2)
     (if (and (< guess 1) (< x 1)) 
        (< (abs (- (func-1 guess) x)) 0.000000000000000001)
        (improved-good-enough? guess (func-2 guess x))))

(improved-sqrt-iter 1.0 123456789012345)
(improved-sqrt-iter 1.0 0.00000000123456)

(check-expect (final-checker 11111111.061111081 123456789012345 square improve) #t)
(check-expect (final-checker 3.51363060095964e-5 0.00000000123456 square improve) #t)

(test)
;; now it worked.

;; PS: I didn't figure out on my own at first, I checked the website below and gradually understand 
;; why there're something wrong.
;; Reference: https://codology.net/post/sicp-solution-exercise-1-7/

;; Exercise 1.8

(define (cube x) (* x x x))

(define (curt-iter guess x)
    (if (improved-good-enough? guess (improve-curt guess x))
        guess
        (curt-iter (improve-curt guess x ) x)))

(define (improve-curt y x) 
    (/ (+ (/ x (square y)) 
        (* 2 y)) 3))

(curt-iter 1.0 27)

(check-expect (final-checker (curt-iter 1.0 27) 27 cube improve-curt) #t)
(check-expect (final-checker (curt-iter 0.0 0) 0 cube improve-curt) #t)
(check-expect (final-checker (curt-iter 1.0 0) 12345678901234 cube improve-curt) #f)
(check-expect (final-checker (curt-iter 1.0 12345678901234) 12345678901234 cube improve-curt) #t)
(check-expect (final-checker (curt-iter 1.0 123456789012345) 123456789012345 cube improve-curt) #t)
(check-expect (final-checker (curt-iter 1.0 0.027) 0.027 cube improve-curt) #t)
(check-expect (final-checker (curt-iter 1.0 0.0000000000002) 0.0000000000002 cube improve-curt) #t)
(test)

