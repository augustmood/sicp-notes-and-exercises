# Exercise 3.45

Louis Reasoner thinks our bank-account system is unnecessarily complex and
error-prone now that deposits and withdrawals aren't automatically serialized.
He suggests that make-account-and-serializer should have exported the serializer
(for use by such procedures as serialized-exchange) in addition to (rather than
instead of) using it to serialize accounts and deposits as make-account did. He
proposes to redefine accounts as follows:

```scheme
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))
```

Then deposits are handled as with the original make-account:

```scheme
(define (deposit account amount)
 ((account 'deposit) amount))
```

Explain what is wrong with Louis's reasoning. In particular, consider what
happens when `serialized-exchange` is called.

#

```scheme
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))
```

When `serialized-exchange` is called, there are two different serializers on
these two accounts. When executing `exchange`, `serializer1` and `serializer2`
will be used at the same time, because both `exchange` and `deposit` are both in
`serializer2`. In order to execute `deposit`, we need to wait for `exchange` to
be completed, however `deposit` itself is part of `exchange`, which means that
if the execution of `deposit` doesn't complete, the `exchange` won't be
completed either.
