#lang racket

(define (reverse list)
  (if (null? list)
      empty
      (append (reverse (cdr list)) (cons (car list) empty))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))


(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? list)
  (null? list))

(define (except-first-denomination list)
  (cdr list))

(define (first-denomination list)
  (car list))

(cc 100 us-coins)
;; 292
(cc 100 (reverse us-coins))
;; 292

;; The order of the list coin-values doesn't affect the answer produced by cc, since as long as the 
;; value of parameters in the list has not changed, the procedure `cc` will always iterate through all
;; the parameters and return the corresponding value.