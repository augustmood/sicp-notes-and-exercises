#lang racket
(require "ex2.64.rkt")
(define (make-record key loi) ;; list of information
  (list key loi))

(define (key record)
  (car record))

(define (info record)
  (cadr info))

(define/match (lookup given-key set-of-records)
  [(given-key '()) false]
  [(given-key (list entry left-branch right-branch))
   (cond ((< given-key (key entry)) (lookup given-key left-branch))
         ((> given-key (key entry)) (lookup given-key right-branch))
         (else entry))])

(define records-a (list->tree (list (list 1 '(1 2 3) ) 
                                  (list 3 '(1 2 3) ) 
                                  (list 4 '(1 2 3) ) 
                                  (list 5 '(1 2 3) ) 
                                  (list 6 '(1 2 3)))))