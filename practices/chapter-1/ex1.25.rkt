#lang racket
(require test-engine/racket-tests)

(define (square n)
    (* n n))
;; original exp function

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;; fast-expt

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


;;
(define (new-expmod base exp m)
  (remainder (fast-expt base exp) m))


(check-expect (expmod 10 12 3) (new-expmod 10 12 3))
(check-expect (expmod 11 19 5) (new-expmod 11 19 5))
(check-expect (expmod 6 12 7) (new-expmod 6 12 7))
(test)

;; She is correct. while the new expmod considerably takes quite longer than the original expmod, 
;; as the new-expmod will first apply fast-expt to get a number with extremly long size, which means
;; the new-expmod is a bad modification.