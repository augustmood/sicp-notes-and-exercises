#lang racket

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;; original expmod

; (define (expmod base exp m)
;   (cond ((= exp 0) 1)
;         ((even? exp)
;          (remainder (square (expmod base (/ exp 2) m))
;                     m))
;         (else
;          (remainder (* base (expmod base (- exp 1) m))
;                     m))))

;; in lisp, we apply the applicative order, so the oprand will evaluate and substitue first, thus in
;; the modified expmod we need to evaluate each of the parameters to get the operators evaluated.
;; so the total steps is roughly like \bigO(2^(log_2(n))), which is \bigO(n), while the original 
;; is \bigO(logn) as we previous discussed.



;; Induction way:
;; Assume (square (expmod base n m)) takes a steps.
;; For (square (expmod base 2n m)), should be (square (square (expmod base n m))), which only takes
;; one more steps, thus I'd conclude that this is \bigO(log n) growth.
;; For (* (expmod base n m)
;;        (expmod base n m))
;; we assume when the exp value is n, the steps that we take is a, then when the exp value increase
;; twice as the original value,
;; ((* (expmod base 2n m)
;;         (expmod base 2n m))
;; will be
;; (* (* (expmod base n m)
;;       (expmod base n m))
;;     (* (expmod base n m)
;;        (expmod base n m)))
;; the steps that should be taken increases like fourth time than the original one. therefore, this is
;; \bigO(n) growth.