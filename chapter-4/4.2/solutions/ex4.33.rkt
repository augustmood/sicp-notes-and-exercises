#lang sicp
(#%require "interp-4.26.rkt")

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))


(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp env)
  (let ([text (cadr exp)])
    (define (quoted-list text)
          (if (null? text)
              nil
              (list 'cons (list 'quote (car text)) 
                    (quoted-list (cdr text)))))
    (if (pair? text)
        (eval* (quoted-list text) env)
        text)))
