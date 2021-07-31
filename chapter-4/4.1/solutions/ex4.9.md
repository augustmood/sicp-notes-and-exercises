# Exercise 4.9

Many languages support a variety of iteration constructs, such as `do`, `for`,
`while`, and `until`. In Scheme, iterative processes can be expressed in terms of
ordinary procedure calls, so special iteration constructs provide no essential
gain in computational power. On the other hand, such constructs are often
convenient. Design some iteration constructs, give examples of their use, and
show how to implement them as derived expressions.

#

## while
```scheme
;; (while <cond> <body>)

;; (while (< i 100)
;;   (display "while\n"))

(define (while? exp) (tagged-list? exp 'while))
(define (while-predicate exp) (cadr exp))
(define (while-body exp) (cddr exp))
(define (while->combination exp)
  (sequence->exp
   (list (make-define 'while
                      (make-if (while-predicate exp)
                               (sequence->exp
                                (list (while-body exp)
                                      (list 'while)))
                               'quit))
                               (list 'while))))
```

## for
```scheme
;; (for <initialization/bindings> <condition> <updation> <body>)

;; (for ([x 0] [y 10]) (< y x) (inc x) 
;;    (display (- y x)))

(define (for? exp) (tagged-list? exp 'for))
(define (for-bindings exp) (cadr exp))
(define (for-binding-variables exp) (map car (cadr exp)))
(define (for-binding-expressions exp) (map cdr (cadr exp)))
(define (for-predicate exp) (caddr exp))
(define (for-updation exp) (cadddr exp))
(define (for-body exp) (cddddr exp))

(define (for->combination exp)
  (sequence->exp
   (cons (make-lambda 
          (for-binding-variables exp)
          (sequence->exp
           (list (make-define 'for
                              (make-if (for-predicate exp)
                                       (sequence->exp
                                        (list (for-body exp)
                                              (list 'for)
                                              (for-updation exp)))
                                       'quit))
                 (list 'for))))
         (for-binding-expressions exp))))
```
