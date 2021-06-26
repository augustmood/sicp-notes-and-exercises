#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))


; (withdraw 25)
; (withdraw 25)
; (withdraw 60)
; (withdraw 15)

;; We can make balance internal to `withdraw` by rewriting the difinition as follows:

(define new-withdraw
  ;   (let ((balance balance))
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))


(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

; (define W1 (make-withdraw 100))
; (define W2 (make-withdraw 100))
; (W1 50)

; (W2 70)

; (W2 40)

; (W1 40)

;; W1 and W2 are completely independent objects, each with its own local state variable `balance`
;; We can also create objects that handle deposits as well as withdrawals, and thus we can represent
;; simple bank accounts.

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

;; Each call to `make-account` sets up an environment with a local state variable `balance`, Within
;; this environment, `make-account` defines procedures `deposit` and `withdraw` that takes a `message`
;; as input and returns one of the two local procedures.

(define acc (make-account 100))
((acc 'withdraw) 50)

((acc 'withdraw) 60)

((acc 'deposit) 40)

((acc 'withdraw) 60)
