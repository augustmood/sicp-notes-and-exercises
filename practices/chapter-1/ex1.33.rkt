#lang racket
(require test-engine/racket-tests)

(define (filtered-accumulate-iter combiner filter null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a b)
            (iter (next a) (combiner result (term a)))
            (iter (next a) result))))
  (iter a null-value))

(define (filtered-accumulate-re combiner filter null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a b)
          (combiner (term a) (filtered-accumulate-re combiner filter null-value term (next a) next b))
          (filtered-accumulate-re combiner filter null-value term (next a) next b))))

;; a
(define (square i)
  (* i i))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (prime? n times)
  (cond ((= n 1) false)
        ((= times 0) true)
        ((fermat-test n) (prime? n (- times 1)))
        (else false))) ;; 1 is not prime number, but the prime? function provided by the 
                       ;; book ignored this.

(define (addone i)
  (+ i 1))

(define (prime-sq-sum-1 a b)
    (filtered-accumulate-re + prime? 0 square a addone b))

(define (prime-sq-sum-2 a b)
    (filtered-accumulate-iter + prime? 0 square a addone b))

;; b
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (justify i)
    i)

(define (relative-prime? i n)
    (and (< i n) (= (gcd i n) 1)))

(define (relative-prime-product-1 n)
    (filtered-accumulate-re * relative-prime? 1 justify 1 addone n))

(define (relative-prime-product-2 n)
    (filtered-accumulate-iter * relative-prime? 1 justify 1 addone n))

;; test
(check-expect (prime-sq-sum-1 1 0) 0)
(check-expect (prime-sq-sum-1 2 0) 0)
(check-expect (relative-prime-product-1 0) 1)
(check-expect (relative-prime-product-2 0) 1)
(check-expect (prime-sq-sum-1 1 10) 87)
(check-expect (prime-sq-sum-1 2 10) 87)
(check-expect (relative-prime-product-1 10) 189)
(check-expect (relative-prime-product-2 10) 189)
(test)