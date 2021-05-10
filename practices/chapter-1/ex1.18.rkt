#lang racket
(require racket/trace)
(require test-engine/racket-tests)

(define (new-mul a b)
    (if (= b 0)
        0
        (new-mul-iter b a 0)))

(define (new-mul-iter counter product-a product-b)
    (cond ((= counter 1) (+ product-a product-b))
          ((even? counter) (new-mul-iter (/ counter 2) (+ product-a product-a) product-b))
          (else (new-mul-iter (- counter 1) product-a (+ product-a product-b)))))

(check-expect (new-mul 3 4) 12)
(check-expect (new-mul 2 5) 10)
(check-expect (new-mul 2 0) 0)
(check-expect (new-mul 2 1) 2)
(check-expect (new-mul 15 25) 375)
(test)


(trace new-mul-iter)
(new-mul-iter 2 9)
(new-mul-iter 2 10)