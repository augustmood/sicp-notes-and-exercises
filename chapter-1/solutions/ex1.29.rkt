#lang racket
(require test-engine/racket-tests)
(require racket/trace)

; (define (sum term a next b)
;   (if (> a b)
;       0
;       (+ (term a)
;          (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  
  (define (addone i)
    (+ i 1))
  
  (define (sum term i next n)
    (define (mul i)
      (cond ((or (= i 0) (= i n)) 1)
            ((even? i) 2)
            (else 4)))
        (if (> i n)
            0
            (+ (* (mul i) (term (+ a (* i h)))) 
                (sum term (next i) next n))))
  
  (* (/ h 3) (sum f a addone n)))


;; test part

(define (old-sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (old-sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (old-sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)
; .24998750000000042
(integral cube 0 1 0.001)
; .249999875000001

(newline)

; (trace sum)
(simpson cube 0 1 100)
(simpson cube 0 1 1000)
