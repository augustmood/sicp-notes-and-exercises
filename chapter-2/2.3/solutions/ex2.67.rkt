#lang racket
(require "huffman.rkt")

(provide (all-defined-out))

;; Define an encoding tree and a sample message:

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message-a '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define sample-message-b '(0 1 0 1 1 0 1 1 1)) 

; (decode sample-message-a sample-tree)
; '(A D A B B C A)

; (decode sample-message-b sample-tree)
; '(A B D C)