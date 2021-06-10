#lang racket
(require "huffman.rkt")
(require "ex2.67.rkt")
(require test-engine/racket-tests)
(provide (all-defined-out))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; Encode-symbol is a procedure, which you must write, that returns the list of bits that encodes a 
;; given symbol according to a given tree. You should design encode-symbol so that it signals an error
;; if the symbol is not in the tree at all. Test your procedure by encoding the result you obtained in
;; exercise 2.67 with the sample tree and seeing whether it is the same as the original sample 
;; message.

(define (encode-symbol message tree)
  (define (existed? object)
    (and (memq message (symbols object)) #t))
  
  (define (finding-symbol object result)
    (if (leaf? object)
        (reverse result)
        (let ((left-br (left-branch object))
              (right-br (right-branch object)))
          (if (existed? left-br)
              (finding-symbol left-br (cons 0 result))
              (finding-symbol right-br (cons 1 result))))
        ))
  
  (if (existed? tree)
      (finding-symbol tree '())
      (error "No such symbol exists in the tree")))

(check-expect (encode (decode sample-message-a sample-tree) sample-tree) sample-message-a)
(check-expect (encode (decode sample-message-b sample-tree) sample-tree) sample-message-b)
(test)