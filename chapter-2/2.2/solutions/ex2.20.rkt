#lang racket

; (define (same-parity a . b)
;   (define (fetch-parity l)
;     (cond 
;       ((null? l) (list)) ;; all of (list)/empty/'() work
;       ((= (remainder a 2) (remainder (car l) 2)) (cons (car l) (fetch-parity (cdr l))))
;       (else (fetch-parity (cdr l)))))
;   (append (list a) (fetch-parity b)))


(define (same-parity a . b)
  (define (fetch-parity l result)
    (cond 
      ((null? l) result)
      ((= (remainder a 2) (remainder (car l) 2)) 
       (fetch-parity (cdr l) (append result (list (car l)))))
      (else (fetch-parity (cdr l) result))))
  (fetch-parity b (list a)))

(same-parity 1 2 3 4 5 6 7)
;; '(1 3 5 7)
(same-parity 2 3 4 5 6 7)
;; '(2 4 6)