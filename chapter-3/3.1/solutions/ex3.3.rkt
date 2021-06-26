#lang racket
(require sicp)
(print-as-expression #f)
(print-mpair-curly-braces #f)

; (define (make-account balance password)
;   (define (withdraw amount)
;     (if (>= balance amount)
;         (begin (set! balance (- balance amount))
;                balance)
;         "Insufficient funds"))
;   (define (deposit amount)
;     (set! balance (+ balance amount))
;     balance)
;   (define (dispatch pw m)
;     (if (eq? pw password)
;         (cond ((eq? m 'withdraw) withdraw)
;               ((eq? m 'deposit) deposit)
;               (else (error "Unknown request -- MAKE-ACCOUNT"
;                            m)))
;         (lambda (i) "Incorrect password")))
;   dispatch)

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw m)
    (cond ((not (eq? pw password)) (lambda (i) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40) ;; 60
((acc 'some-other-password 'deposit) 50) ;; "Incorrect password"
