#lang sicp
(#%require "env.rkt")

;; if we implment `unless` as a special form, every time we use it, we might not
;; go into the recursion again.



; (define (if? exp) (tagged-list? exp 'if))
; (define (if-predicate exp) (cadr exp))
; (define (if-consequent exp) (caddr exp))
; (define (if-alternative exp)
;   (if (not (null? (cdddr exp)))
;       (cadddr exp)
;       'false))
; (define (make-if predicate consequent alternative)
;   (list 'if predicate consequent alternative))

(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-predicate exp) (cadr exp))
(define (unless-usual-val exp) (caddr exp))
(define (unless-except-val exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-unless predicate usual-val except-val)
  (list 'unless predicate usual-val except-val))
(define (unless->if exp)
  (make-if (unless-predicate exp)
           (unless-except-val exp)
           (unless-usual-val exp)))

;; add the line of code below to `eval`:
; [(unless? exp) (eval-if (unless->if exp))]

;; Using unless in our iterative procedures may be a littel more intuitive.

(driver-loop)