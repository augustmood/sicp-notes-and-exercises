# Exercise 3.48

Explain in detail why the deadlock-avoidance method described above, (i.e., the
accounts are numbered, and each process attempts to acquire the smaller-numbered
account first) avoids deadlock in the exchange problem. Rewrite
`serialized-exchange` to incorporate this idea. (You will also need to modify
`make-account` so that each account is created with a number, which can be
accessed by sending an appropriate message.)

#

Suppose there are two serializers `S-1` for account `a1` and `S-1` for account
`a2`. In the deadlock case given in the book, Peter needs to acquire the mutex
in `S-1` first and then in `S-2`, while Paul needs to acquire the mutex in `S-2`
first and then in `S-1`, but both of their second mutex acquiring require the
other person to release their first acquired mutex first, but without releasing
the second acquired mutex, the first mutex cannot be released, so we get a
deadlock. And the order of the acquisitions of these two different mutex is what
causes the deadlock. Therefore, once we guarantee the order of mutex to be
acquired, our problem will be solved.

### modified serialized-exchage below:

```scheme
(define list-id (list))
(define (id-generate)
  (let ([id (random 99999999)])
    (if (memq id list-id)
        (id-generate)
        (begin (set! list-id (cons id list-id))
               id))))

(define (make-account-and-serializer balance)
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
  (let ([id (id-generate)])
    (let ((balance-serializer (make-serializer)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'balance) balance)
              ((eq? m 'serializer) balance-serializer)
              ((eq? m 'id) id)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m))))
      dispatch)))

(define (serialized-exchange account1 account2)
  (let ([serializer1 (account1 'serializer)]
        [serializer2 (account2 'serializer)])
    (let ([serialized-p (if (> (account1 'id) (account2 'id))
                            (serializer1 (serializer2 exchange))
                            (serializer2 (serializer1 exchange)))])
      (serialized-p account1 account2))))
```
