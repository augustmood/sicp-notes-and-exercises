#lang racket
(require sicp)

(define (make-account balance password)
  (define password-list (list password))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw m)
    (cond [(not (memq pw password-list)) (lambda (i) "Incorrect password")]
          [(eq? m 'new-access) 
           (lambda (new-password)(set! password-list (cons new-password password-list)))]
          [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [else (error "Unknown request -- MAKE-ACCOUNT"
                       m)]))
  dispatch)

(define (make-joint acc old-password new-password)
  (begin
    ((acc old-password 'new-access) new-password)
    acc))

(define peter-acc (make-account 100 'open-sesame))

((peter-acc 'open-sesame 'withdraw) 40) ;; 60
((peter-acc 'some-other-password 'deposit) 50) ;; "Incorrect password"
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'rosebud 'withdraw) 10) ;; 50

;; There is a flaw in that paul-acc can use peter-acc's password to access the account, but I don't 
;; really think the banking system works in this way.
