# Exercise 3.44

Consider the problem of transferring an amount from one account to another. Ben
Bitdiddle claims that this can be accomplished with the following procedure,
even if there are multiple people concurrently transferring money among multiple
accounts, using any account mechanism that serializes deposit and withdrawal
transactions, for example, the version of make-account in the text above.

```scheme
(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))
```

Louis Reasoner claims that there is a problem here, and that we need to use a
more sophisticated method, such as the one required for dealing with the
exchange problem. Is Louis right? If not, what is the essential difference
between the transfer problem and the exchange problem? (You should assume that
the balance in from-account is at least amount.)

No, we don't need that, the difference between the transfer and exchange problem
is that in the exchange problem we need to access the accounts to get the
difference, and there may be several people who can access the same accounts at
the same time and make the exchange concurrently, which will make the exchange
result incorrect, so we may need to serialize it so that other people can't
access the accounts until the one complete an action. However, in the case of
transfering, the amount is given as a parameter and we don't need to access each
account to know its value, so all we need to do is to execute the program
sequentially and possibly add an assertion to prevent some invalid transfers.
(For example, in some cases, the transfer amount is greater than the balance.)
